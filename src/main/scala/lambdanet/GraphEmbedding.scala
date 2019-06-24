package lambdanet

import botkop.numsca
import botkop.numsca.Tensor
import funcdiff._
import lambdanet.GraphEmbedding._
import lambdanet.translation.OldIR.IRType
import lambdanet.translation.OldPredicateGraph
import lambdanet.translation.OldPredicateGraph._

import scala.collection.parallel.ForkJoinTaskSupport
import scala.collection.{GenSeq, mutable}

object GraphEmbedding {

  val unknownTypeSymbol = 'UNKNOWN

  case class EmbeddingCtx(
      libraryTypeMap: Symbol => CompNode,
      labelMap: Symbol => CompNode,
      fieldKnowledge: Map[Symbol, (CompNode, CompNode)],
      varKnowledge: Map[Symbol, CompNode]
  )

  case class Embedding(nodeMap: Map[IRType, CompNode], stat: EmbeddingStat)

  case class EmbeddingStat(trueEmbeddingLengths: IS[Double])

  case class DecodingCtx(
      libraryTypes: IS[GType],
      projectTypes: IS[IRType]
  ) {

    require(SimpleMath.noDuplicate(libraryTypes))
    require(SimpleMath.noDuplicate(projectTypes))
    private val libraryTypeIndexMap = libraryTypes.zipWithIndex.toMap

    private val projectTypeIndexMap = {
      val base = libraryTypeIndexMap.size
      projectTypes.zipWithIndex.map { case (t, idx) => t -> (idx + base) }.toMap
    }
    val outOfScopeIdx: Int = libraryTypeIndexMap.size + projectTypeIndexMap.size
    val maxIndex: Int = outOfScopeIdx + 1

    def indexOfType(t: TypeLabel): Int = t match {
      case LibraryType(ty) => libraryTypeIndexMap.getOrElse(ty, outOfScopeIdx)
      case ProjectType(ty) => projectTypeIndexMap(ty)
      case OutOfScope      => outOfScopeIdx
    }

    def typeFromIndex(i: Int): TypeLabel = {
      if (i == outOfScopeIdx) OutOfScope
      else if (i < libraryTypes.length)
        LibraryType(libraryTypes(i))
      else ProjectType(projectTypes(i - libraryTypes.length))
    }
  }
}

import funcdiff._

case class GraphEmbedding(
    graph: OldPredicateGraph,
    ctx: EmbeddingCtx,
    layerFactory: LayerFactory,
    dimMessage: Int,
    taskSupport: Option[ForkJoinTaskSupport]
) {
  import ctx._
  import layerFactory._

  require(dimMessage % 2 == 0, "dimMessage should be even")

  val nodeInitVec: CompNode =
    getVar('nodeInitVec)(TensorExtension.randomUnitVec(dimMessage)) // randomVec()

  val KnowledgeMissing: CompNode =
    getVar('knowledgeMissingVec)(randomVec())

  /**
    * @return A prediction distribution matrix of shape (# of places) * (# of candidates)
    */
  def encodeAndDecode(
      iterations: Int,
      decodingCtx: DecodingCtx,
      placesToDecode: IS[IRType]
  ): (CompNode, IS[Embedding]) = {
    val embeddings = DebugTime.logTime('iterTime) {
      val stat = EmbeddingStat(graph.nodes.map(_ => 1.0))
      val init = Embedding(graph.nodes.map(n => n -> nodeInitVec).toMap, stat)
      IS.iterate(init, iterations + 1)(iterate)
    }

    val result = DebugTime.logTime('decodingTime) {
      decode(decodingCtx, placesToDecode, embeddings.last)
    }

    (result, embeddings)
  }

  /** Each message consists of a key-value pair */
  type Message = (CompNode, CompNode)

  def singleLayer(name: SymbolPath, input: CompNode): CompNode = {
    linear(name / 'linear, dimMessage)(input) ~> relu
  }

  def messageModel(name: SymbolPath, vec: CompNode): Message = {
    relu(linear(name / 'header, dimMessage)(vec)) -> relu(
      linear(name / 'content, dimMessage)(vec)
    )
  }

  def binaryMessage(name: SymbolPath, v1: CompNode, v2: CompNode): Message = {
    singleLayer(name / 'header, v1.concat(v2, axis = 1)) ->
      singleLayer(name / 'content, v1.concat(v2, axis = 1))
  }

  def fieldAccessMessage(
      name: SymbolPath,
      objEmbed: CompNode,
      fieldLabel: Symbol
  ): Message = {
    val input = objEmbed.concat(labelMap(fieldLabel), axis = 1)
    messageModel(name, singleLayer(name / 'compress, input))
  }

  def argAccessMessage(
      name: SymbolPath,
      fEmbed: CompNode,
      argId: Int
  ): Message = {
    val input = fEmbed.concat(positionalEncoding(argId), axis = 1)
    messageModel(name, singleLayer(name / 'compress, input))
  }

//  def messageModel2(name: SymbolPath, key: CompNode, value: CompNode): Message = {
//    linear(name / 'header, dimMessage)(key) -> linear(name / 'content, dimMessage)(
//      value
//    )
//  }

  def randomVec(): Tensor = {
    numsca.randn(1, dimMessage) * 0.01
  }

  def positionalEncoding(pos: Int): CompNode = {
    assert(pos >= -1)
    if (pos == -1)
      getVar('position / 'head) { randomVec() } else {
      getConst('position / Symbol(pos.toString)) {
        val phases = (0 until dimMessage / 2).map { dim =>
          pos / math.pow(1000, 2.0 * dim / dimMessage)
        }
        numsca
          .Tensor(phases.map(math.sin) ++ phases.map(math.cos): _*)
          .reshape(1, -1)
      }
    }
  }

  def iterate(
      embedding: Embedding
  ): Embedding = {
    import embedding._
    collection.concurrent.TrieMap(1 -> 2)
    TrainingCenter.note("iterate")

    val _messages =
      graph.nodes
        .map(n => n -> mutable.ListBuffer[Message]())
        .toMap

    def messages(node: IRType): MessageChannel =
      MessageChannel(node, _messages(node))

    case class MessageChannel(
        node: IRType,
        receiver: mutable.ListBuffer[Message]
    ) {
      def +=(msg: Message): Unit = receiver.synchronized {
        receiver.append(msg)
      }
    }

    type ObjType = IRType
    val fieldDefs = mutable.HashMap[Symbol, IS[(ObjType, IRType)]]()
    val fieldUsages = mutable.HashMap[Symbol, IS[(ObjType, IRType)]]()
    graph.predicates.collect {
      case DefineRel(v, ObjLiteralTypeExpr(fields)) =>
        fields.foreach {
          case (l, t) =>
            fieldDefs(l) = fieldDefs.getOrElse(l, IS()) :+ (v, t)
        }
      case DefineRel(v, FieldAccessTypeExpr(objType, l)) =>
        fieldUsages(l) = fieldUsages.getOrElse(l, IS()) :+ (objType, v)
    }

    /* for each kind of predicate, generate one or more messages */
    def sendPredicateMessages(predicate: TyVarPredicate): Unit =
      predicate match {
        case FreezeType(v, ty) =>
          messages(v) += messageModel('FreezeType, encodeGType(ty))
        case IsLibraryType(v, name) =>
          val knowledge = varKnowledge.getOrElse(name, KnowledgeMissing)
          messages(v) += messageModel('IsLibrary, knowledge)
        case HasName(v, name) =>
//        messages(v) += messageModel('HasName, labelMap(name)) //todo: properly handle name info
        case SubtypeRel(sub, sup) =>
          messages(sub) += binaryMessage(
            'SubtypeRel_sub,
            nodeMap(sub),
            nodeMap(sup)
          )
          messages(sup) += binaryMessage(
            'SubtypeRel_sup,
            nodeMap(sub),
            nodeMap(sup)
          )
        case AssignRel(lhs, rhs) =>
          messages(lhs) += binaryMessage(
            'AssignRel_lhs,
            nodeMap(lhs),
            nodeMap(rhs)
          )
          messages(rhs) += binaryMessage(
            'AssignRel_rhs,
            nodeMap(lhs),
            nodeMap(rhs)
          )
        case UsedAsBoolean(tyVar) =>
          messages(tyVar) += messageModel('UsedAsBoolean, nodeMap(tyVar))
        case InheritanceRel(child, parent) =>
          messages(child) += binaryMessage(
            'DeclaredAsSubtype,
            nodeMap(child),
            nodeMap(parent)
          )
          messages(parent) += binaryMessage(
            'DeclaredAsSupertype,
            nodeMap(child),
            nodeMap(parent)
          )
        case DefineRel(v, expr) =>
          expr match {
            case VarTypeExpr(rhs) =>
              messages(v) += binaryMessage(
                'DefineVar_lhs,
                nodeMap(v),
                nodeMap(rhs)
              )
              messages(rhs) += binaryMessage(
                'DefineVar_rhs,
                nodeMap(v),
                nodeMap(rhs)
              )
            case FuncTypeExpr(argTypes, returnType) =>
              val ids = returnType +: argTypes.toIndexedSeq
              ids.zipWithIndex.foreach {
                case (tId, i) =>
                  val argId = i - 1
                  messages(v) += argAccessMessage(
                    'FuncTypeExpr_toF,
                    nodeMap(tId),
                    argId
                  )
                  messages(tId) += argAccessMessage(
                    'FuncTypeExpr_toArg,
                    nodeMap(v),
                    argId
                  )
              }
            case CallTypeExpr(f, args) =>
              val fVec = nodeMap(f)
              val fKey = singleLayer('CallTypeExpr / 'fKey, fVec)
              val fPair = messageModel('CallTypeExpr / 'fPair, fVec)

              val argPairs = args.toIndexedSeq.zipWithIndex.map {
                case (argT, argId) =>
                  argAccessMessage(
                    'CallTypeExpr / 'embedArg,
                    nodeMap(argT),
                    argId
                  )
              }

              val fEmbed = attentionLayer('CallTypeExpr / 'fEmbed)(
                fKey,
                fPair +: argPairs
              )
              messages(v) += messageModel('CallTypeExpr / 'toV, fEmbed)
              args.zipWithIndex.foreach {
                case (arg, argId) =>
                  messages(arg) += argAccessMessage(
                    'CallTypeExpr / 'toArg,
                    fEmbed,
                    argId
                  )
              }
            case ObjLiteralTypeExpr(fields) =>
              fields.foreach {
                case (label, tv) =>
                  messages(v) += fieldAccessMessage(
                    'ObjLiteralTypeExpr / 'toV,
                    nodeMap(tv),
                    label
                  )
                  messages(tv) += fieldAccessMessage(
                    'ObjLiteralTypeExpr / 'toField,
                    nodeMap(v),
                    label
                  )
                  if (fieldUsages.contains(label)) {
                    messages(tv) += {
                      val att =
                        attentionLayer(
                          'ObjLiteralTypeExpr / 'fieldUsage,
                          transformKey = true
                        )(
                          nodeMap(v),
                          fieldUsages(label).map {
                            case (k, n) => nodeMap(k) -> nodeMap(n)
                          }
                        )
                      messageModel('ObjLiteralTypeExpr / 'usageMessage, att)
                    }
                  }
              }
            case FieldAccessTypeExpr(objType, label) =>
              messages(v) += fieldAccessMessage(
                'FieldAccess / 'toV,
                nodeMap(objType),
                label
              )
              messages(objType) += fieldAccessMessage(
                'FieldAccess / 'toObj,
                nodeMap(v),
                label
              )
              if (fieldDefs.contains(label)) {
                messages(v) += {
                  val att =
                    attentionLayer('FieldAccess / 'defs, transformKey = true)(
                      nodeMap(objType),
                      fieldDefs(label)
                        .map { case (k, n) => nodeMap(k) -> nodeMap(n) } ++
                        fieldKnowledge.get(label).toIndexedSeq
                    )
                  messageModel('FieldAccess / 'defsMessage, att)
                }
              }
          }
      }

    TrainingCenter.note("iterate/Before sending messages")
    par(graph.predicates).foreach(sendPredicateMessages)
    TrainingCenter.note("iterate/After sending messages")

    val outLengths = mutable.ListBuffer[Double]()
    val newNodeMap = par(_messages.keys.toSeq)
      .map { t =>
        val node = nodeMap(t)
//        val nodePair = messageModel('MessageAggregate / 'nodeMessage, nodeMap(id))
        val out = attentionLayer('MessageAggregate, transformKey = true)(
          node,
          _messages(t).toIndexedSeq :+ (node, node)
        )
        val outLen = sqrt(sum(square(out)))
        outLengths.synchronized {
          outLengths += outLen.value.squeeze()
        }
        val newEmbed = out / outLen
//      val newEmbed = gru('MessageAggregate / 'updateGru)(nodeVec, change)
        t -> newEmbed
      }
      .seq
      .toMap
    TrainingCenter.note("iterate/After updating")
    val stat = EmbeddingStat(outLengths.toIndexedSeq)
    Embedding(newNodeMap, stat)
  }

  /**
    * @return A prediction distribution logits matrix of shape (# of places) * (# of candidates)
    */
  def decode(
      decodingCtx: DecodingCtx,
      placesToDecode: IS[IRType],
      embedding: Embedding
  ): CompNode = {
    require(placesToDecode.nonEmpty)

    TrainingCenter.note("decode")

    val attentionHeads = 16
    val concretes = decodingCtx.libraryTypes.map(encodeGType)

    val certainty = getVar(Symbol("decode:certainty"))(Tensor(6.0))
    val logits = for (head <- par(0 until attentionHeads)) yield {
      val shrinkFactor = 2
      def mlp(rows: IS[CompNode], layerName: String, layers: Int): CompNode = {
        var input = concatN(axis = 0)(rows)
        var dim = dimMessage
        for (i <- 0 until layers) {
          dim /= shrinkFactor
          input = input ~>
            //          batchNorm(Symbol(s"$layerName-BN$i"), inTraining = true) ~>
            linear(Symbol(s"$layerName$i") / Symbol(s"head$head"), dim) ~>
            relu
        }
        input
      }

      import embedding._

      val candidateEmbeddings = {
        val c =
          mlp(concretes, "decode:concreteType", 2)
        val p =
          mlp(
            decodingCtx.projectTypes.map(t => nodeMap(t)),
            "decode:projectType",
            2
          )
        val unknownTypeVector = getVar(Symbol(s"unknownTypeVec$head")) {
          numsca.randn(1, c.shape(1)) * 0.01
        }
        c.concat(p, axis = 0).concat(unknownTypeVector, axis = 0)
      }

      val transformedNodes =
        mlp(placesToDecode.map(embedding.nodeMap), "decode:nodes", 2)
      transformedNodes
        .dot(candidateEmbeddings.t) * (certainty * 10.0 / dimMessage)
    }
    total(logits.seq.toVector) ~> relu
  }

  private val gTypeEmbeddingMap = mutable.HashMap[GType, CompNode]()
  def encodeGType(t: GType): CompNode =
    gTypeEmbeddingMap.getOrElseUpdate(
      t, {

        val funcInitKey = getVar('funcType / 'initKey)(randomVec())
        val funcInitValue = getVar('funcType / 'initValue)(randomVec())

        val objectInitKey = getVar('objType / 'initKey)(randomVec())
        val objectInitValue = getVar('objType / 'initValue)(randomVec())

        def rec(t: GType): CompNode = t match { //todo: need better ways
          case AnyType     => getVar('anyType)(randomVec())
          case TyVar(name) => libraryTypeMap(name)
          case FuncType(args, to) =>
            val messages = (to +: args).zipWithIndex.map {
              case (t, i) =>
                argAccessMessage('funcType / 'arg, rec(t), i - 1)
            }.toIndexedSeq
            attentionLayer('funcType / 'aggregate)(
              funcInitKey,
              messages
            )
          case ObjectType(fields) =>
            if (fields.isEmpty) {
              getVar('objectType / 'emptyObject)(randomVec())
            } else {
              val messages = fields.toIndexedSeq.map {
                case (label, t) =>
                  fieldAccessMessage('objectType / 'field, rec(t), label)
              }
              attentionLayer('objectType / 'aggregate)(
                objectInitKey,
                messages
              )
            }
        }

        rec(t)
      }
    )

  def par[T](xs: Seq[T]): GenSeq[T] = {
    taskSupport match {
      case None => xs
      case Some(ts) =>
        val r = xs.par
        r.tasksupport = ts
        r
    }
  }

}
