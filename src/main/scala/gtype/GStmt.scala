package gtype

import scala.language.implicitConversions
import GType.API._
import gtype.GExpr.GExprAPI
import funcdiff.SimpleMath.Extensions._
import collection.mutable

// @formatter:off
/** a program statement
  *
  * S :=                                    ([[GStmt]])
  *       var x: α = e                      ([[VarDef]])
  *       e := e                            ([[AssignStmt]])
  *       [return] e                        ([[ExprStmt]])
  *       if e then S else S                ([[IfStmt]])
  *       while e do S                      ([[WhileStmt]])
  *       // comment                        ([[CommentStmt]])
  * B in  { S; ...; S }                     ([[BlockStmt]])
  * f in  function x (x: α, ..., x:α): α B  ([[FuncDef]])
  *       class x (l: α, ..., l:α)          ([[ClassDef]])
  *       ↳ [extends x]{ f, ..., f }
  *
  * where x and l are [[Symbol]],
  *       α is [[GTMark]],
  *       e is [[GExpr]],
  * */
// @formatter:on
sealed trait GStmt {
  def prettyPrint(indentSpaces: Int = 2, indent: Int = 0): String = {
    GStmt
      .prettyPrintHelper(indent, this)
      .map {
        case (dent, text) => " " * (dent * indentSpaces) + text
      }
      .mkString("\n")
  }

  override def toString: String = prettyPrint()
}

// === Start of Statement definitions ====

case class VarDef(
  x: Symbol,
  ty: GTMark,
  init: GExpr,
  isConst: Boolean,
  exportLevel: ExportLevel.Value
) extends GStmt

case class AssignStmt(lhs: GExpr, rhs: GExpr) extends GStmt

case class ExprStmt(e: GExpr, isReturn: Boolean) extends GStmt

case class IfStmt(cond: GExpr, branch1: GStmt, branch2: GStmt) extends GStmt

case class WhileStmt(cond: GExpr, body: GStmt) extends GStmt

case class CommentStmt(text: String) extends GStmt

case class BlockStmt(stmts: Vector[GStmt]) extends GStmt

case class FuncDef(
  name: Symbol,
  tyVars: List[Symbol],
  args: List[(Symbol, GTMark)],
  returnType: GTMark,
  body: GStmt,
  exportLevel: ExportLevel.Value
) extends GStmt

case class ClassDef(
  name: Symbol,
  tyVars: List[Symbol],
  superType: Option[Symbol] = None,
  constructor: FuncDef,
  vars: Map[Symbol, GTMark],
  funcDefs: Vector[FuncDef],
  exportLevel: ExportLevel.Value
) extends GStmt {
  require(constructor.name == ClassDef.constructorName(name))
  require(constructor.returnType == GType.voidType, s"Get: ${constructor.returnType}")
}

object ClassDef {
  val thisSymbol = 'this
  val superSymbol = 'super
  def constructorName(className: Symbol): Symbol = Symbol(className.name + "-NEW")
  def isConstructor(name: Symbol) = name.name.endsWith("-NEW")
}

// === End of Statement definitions ====

object GStmt {

  import GExpr.typeCheckInfer

  val returnSymbol = 'return

  /*
   * S :=                                    ([[GStmt]])
   *       var x: α = e                      ([[VarDef]])
   *       e := e                            ([[AssignStmt]])
   *       [return] e                        ([[ExprStmt]])
   *       if e then S else S                ([[IfStmt]])
   *       while e do S                      ([[WhileStmt]])
   * B in  { S; ...; S }                     ([[BlockStmt]])
   * f in  function x (x: α, ..., x:α): α B  ([[FuncDef]])
   *       class x (l: α, ..., l:α)          ([[ClassDef]])
   *       ↳ [extends x]{ f, ..., f }
   */
  def prettyPrintHelper(indent: Int, stmt: GStmt): Vector[(Int, String)] = {
    import ExportLevel.asPrefix
    stmt match {
      case VarDef(x, ty, init, isConst, level) =>
        val keyword = if (isConst) "const" else "let"
        Vector(indent -> s"${asPrefix(level)}$keyword ${x.name}: $ty = $init;")
      case AssignStmt(lhs, rhs) =>
        Vector(indent -> s"$lhs = $rhs;")
      case ExprStmt(v, isReturn) =>
        val returnModifier = if (isReturn) "return " else ""
        Vector(indent -> s"$returnModifier$v;")
      case IfStmt(cond, e1, e2) =>
        lazy val elsePart = {
          if (e2 == BlockStmt(Vector())) Vector()
          else Vector(indent -> "else") ++ prettyPrintHelper(indent, e2)
        }
        Vector(indent -> s"if ($cond)") ++
          prettyPrintHelper(indent, e1) ++ elsePart
      case WhileStmt(cond, body) =>
        (indent -> s"while ($cond)") +: prettyPrintHelper(indent, body)
      case CommentStmt(text) =>
        Vector(indent -> ("/* " + text + " */"))
      case BlockStmt(stmts) =>
        (indent -> "{") +: stmts.flatMap(
          s => prettyPrintHelper(indent + 1, s)
        ) :+ (indent -> "}")
      case FuncDef(funcName, tyVars, args, returnType, body, level) =>
        val argList = args
          .map { case (v, tv) => s"${v.name}: $tv" }
          .mkString("(", ", ", ")")
        val tyVarPart = tyVarClause(tyVars)
        Vector(
          indent -> s"${asPrefix(level)}function ${funcName.name}$tyVarPart $argList: $returnType"
        ) ++
          prettyPrintHelper(indent, makeSureInBlock(body))
      case ClassDef(name, tyVars, superType, constructor, vars, funcDefs, level) =>
        val superPart = superType
          .map(t => s" extends $t")
          .getOrElse("")
        val tyVarPart = tyVarClause(tyVars)
        Vector(indent -> s"${asPrefix(level)}class ${name.name}$tyVarPart$superPart {") ++
          vars.toList.map {
            case (fieldName, tv) =>
              (indent + 1, s"${fieldName.name}: $tv;")
          } ++
          (constructor +: funcDefs).flatMap(
            fDef => prettyPrintHelper(indent + 1, fDef)
          ) ++
          Vector(indent -> "}")
    }
  }

  def tyVarClause(tyVars: List[Symbol]): String = {
    if(tyVars.isEmpty) ""
    else tyVars.map(_.name).mkString("<", ", ", ">")
  }

  /**
    * @param f is required to has the same return statement type as its input
    * */
  def modifyChildren(stmt: GStmt)(f: GStmt => GStmt): GStmt = {
    def rec(stmt: GStmt): GStmt = stmt match {
      case IfStmt(cond, branch1, branch2) => f(IfStmt(cond, rec(branch1), rec(branch2)))
      case WhileStmt(cond, body)          => f(WhileStmt(cond, rec(body)))
      case BlockStmt(stmts)               => f(BlockStmt(stmts.map(rec)))
      case fDef: FuncDef                  => f(fDef.copy(body = rec(fDef.body)))
      case cDef: ClassDef =>
        val c1 = cDef.copy(
          constructor = rec(cDef.constructor).asInstanceOf[FuncDef],
          funcDefs = cDef.funcDefs.map(x => rec(x).asInstanceOf[FuncDef])
        )
        f(c1)
      case other => f(other)
    }
    rec(stmt)
  }

  def assertAllTypesStripped(stmt: GStmt): Unit = {
    def fail(s: GStmt): Nothing = {
      throw new AssertionError(s"Type annotation appears in: $s")
    }

    modifyChildren(stmt) {
      case s: VarDef => if (s.ty.isInstanceOf[GType]) fail(s) else s
      case s: FuncDef =>
        if (s.args.exists(_._2.isInstanceOf[GType]) ||
            s.returnType.isInstanceOf[GType] && s.returnType != GType.voidType) fail(s)
        else s
      case s: ClassDef =>
        if (s.vars.exists(_._2.isInstanceOf[GType])) fail(s)
        else s
      case other => other
    }
  }

  case class TypeAnnotation(ty: GType, needInfer: Boolean){
    override def toString: String = {
      s"$ty${if(needInfer) "*" else ""}"
    }
  }

  /** An context used for constructing programs written in [[GStmt]] */
  class TypeHoleContext {
    var typeHoleId: Int = 0
    val holeTypeMap: mutable.HashMap[GTHole, GType] = mutable.HashMap[GTHole, GType]()
    val userAnnotatedSet: mutable.HashSet[GTHole] = mutable.HashSet[GTHole]()

    def newTHole(mark: Option[TypeAnnotation]): GTHole = {
      val h = GTHole(typeHoleId)
      typeHoleId += 1
      mark.foreach { m =>
        assert(!holeTypeMap.contains(h))
        holeTypeMap(h) = m.ty
        if (m.needInfer) {
          userAnnotatedSet += h
        }
      }
      h
    }
  }

  /** Replace all the type annotations with [[GTHole]]s */
  trait GStmtAPI extends GExprAPI {
    var typeHoleContext: TypeHoleContext = new TypeHoleContext()

    implicit def expr2Stmt(expr: GExpr): GStmt = ExprStmt(expr, isReturn = false)

    def RETURN(expr: GExpr) = ExprStmt(expr, isReturn = true)

    def VAR(
      x: Symbol,
      ty: GType,
      isConst: Boolean = false
    )(init: GExpr): VarDef = {
      VarDef(
        x,
        typeHoleContext.newTHole(Some(TypeAnnotation(ty, needInfer = true))),
        init,
        isConst,
        exportLevel = ExportLevel.Public
      )
    }

    def VAR(x: Symbol)(init: GExpr): VarDef = {
      VarDef(
        x,
        typeHoleContext.newTHole(None),
        init,
        isConst = false,
        exportLevel = ExportLevel.Public
      )
    }

    def BLOCK(stmts: GStmt*): BlockStmt = {
      BlockStmt(stmts.toVector)
    }

    def WHILE(cond: GExpr)(stmts: GStmt*): WhileStmt = {
      WhileStmt(cond, BLOCK(stmts: _*))
    }

    case class IFBuild(b: GExpr, branch1: BlockStmt, mkBranch2: BlockStmt => GStmt) {
      def ELSE(branch2: GStmt*) = IfStmt(b, branch1, mkBranch2(BLOCK(branch2: _*)))

      def NoElse: IfStmt = IfStmt(b, branch1, mkBranch2(BLOCK()))

      def EIF(cond: GExpr)(body: GStmt*): IFBuild = {
        IFBuild(b, branch1, block => {
          IfStmt(cond, BLOCK(body: _*), mkBranch2(block))
        })
      }
    }

    def IF(b: GExpr)(branch1: GStmt*) = IFBuild(b, BLOCK(branch1: _*), identity)

    def stripArgs(args: Seq[(Symbol, GType)]): List[(Symbol, GTHole)] = {
      args.toList.map {
        case (s, t) =>
          s -> typeHoleContext.newTHole(Some(TypeAnnotation(t, needInfer = true)))
      }
    }

    def stripType(t: GType): GTHole = {
      typeHoleContext.newTHole(Some(TypeAnnotation(t, needInfer = true)))
    }

    def FUNC(name: Symbol, returnType: GType)(
      args: (Symbol, GType)*
    )(body: GStmt*): FuncDef = {
      val a1s = stripArgs(args)
      FuncDef(
        name,
        List(),
        a1s,
        stripType(returnType),
        BLOCK(body: _*),
        exportLevel = ExportLevel.Public
      )
    }

    def CONSTRUCTOR(className: Symbol, args: (Symbol, GType)*)(body: GStmt*): FuncDef = {
      FuncDef(
        ClassDef.constructorName(className),
        List(),
        stripArgs(args),
        GType.voidType,
        BLOCK(body: _*),
        exportLevel = ExportLevel.Public
      )
    }

    def CLASS(name: Symbol, superType: Option[Symbol] = None)(
      vars: (Symbol, GType)*
    )(constructor: FuncDef, methods: FuncDef*): ClassDef = {
      assert(vars.length == vars.toMap.size)
      ClassDef(
        name,
        List(),
        superType,
        constructor,
        stripArgs(vars).toMap,
        methods.toVector,
        exportLevel = ExportLevel.Public
      )
    }
  }

  object API extends GStmtAPI

  def extractSignature(funcDef: FuncDef): FuncType = {
    funcDef.args.map(_._2.asInstanceOf[GType]) -: funcDef.returnType.asInstanceOf[GType]
  }

  /**
    * Allows forward reference to function definitions, but they will not escape their scope.
    **/
  @deprecated
  def typeCheckBlock(
    block: BlockStmt,
    ctx: ExprContext,
    returnType: GType
  ): Set[TypeCheckError] = {
    var currentCtx = ctx
    block.stmts.foreach {
      case f: FuncDef =>
        val signatureType = extractSignature(f)
        currentCtx = currentCtx.newVar(f.name, signatureType)
      case ClassDef(name, _, superType, constructor, vars, funcDefs, _) =>
        val sT = superType
          .map { s =>
            currentCtx.typeContext.typeUnfold(s).asInstanceOf[ObjectType]
          }
          .getOrElse(obj())
        var fields = sT.fields ++ vars
        funcDefs.foreach { f =>
          fields = fields.updated(f.name, extractSignature(f))
        }
        val constructorType = extractSignature(constructor).copy(to = name)
        val objT = ObjectType(fields.mapValuesNow(_.asInstanceOf[GType]))
        currentCtx = currentCtx.newTypeVar(name, objT).newVar(name, constructorType)
      case _ =>
    }
    var currentErrs = Set[TypeCheckError]()
    block.stmts.foreach { stmt =>
      val (newCtx, errs) = typeCheckStmt(stmt, currentCtx, returnType)
      currentCtx = newCtx
      currentErrs ++= errs
    }
    currentErrs
  }

  /**
    * Allows forward reference to function definitions, but they will not escape their scope.
    **/
  @deprecated
  private def typeCheckStmt(
    stmt: GStmt,
    ctx: ExprContext,
    returnType: GType
  ): (ExprContext, Set[TypeCheckError]) = {
    import ctx.typeContext.mkSubtypeError
    stmt match {
      case VarDef(x, ty, init, _, _) =>
        ty match {
          case ty: GType =>
            val ctx1 = ctx.newVar(x, ty)
            val (initT, es) = typeCheckInfer(init, ctx1)
            ctx1 -> (es ++ mkSubtypeError(initT, ty))
          case _: GTHole =>
            val ctx0 = ctx.newVar(x, any)
            val (initT, es) = typeCheckInfer(init, ctx0)
            val ctx1 = ctx.newVar(x, initT)
            ctx1 -> es
        }

      case AssignStmt(lhs, rhs) =>
        val (lT, e1) = typeCheckInfer(lhs, ctx)
        val (rT, e2) = typeCheckInfer(rhs, ctx)
        ctx -> (e1 ++ e2 ++ mkSubtypeError(rT, lT))
      case ExprStmt(e, isReturn) =>
        val (eT, es) = typeCheckInfer(e, ctx)
        ctx -> (if (isReturn) es ++ mkSubtypeError(eT, returnType) else es)
      case IfStmt(cond, branch1, branch2) =>
        val (condT, e0) = typeCheckInfer(cond, ctx)
        val (_, e1) = typeCheckStmt(branch1, ctx, returnType)
        val (_, e2) = typeCheckStmt(branch2, ctx, returnType)
        ctx -> (e0 ++ e1 ++ e2 ++ mkSubtypeError(condT, GType.boolType))
      case WhileStmt(cond, body) =>
        val (condT, e0) = typeCheckInfer(cond, ctx)
        val (_, e1) = typeCheckStmt(body, ctx, returnType)
        ctx -> (e0 ++ e1 ++ mkSubtypeError(condT, GType.boolType))
      case FuncDef(_, _, args, newReturn: GType, body, _) =>
        val ctxWithArgs = ctx.copy(
          varAssign =
            ctx.varAssign ++ args.toMap.mapValuesNow(_.asInstanceOf[GType])
        )
        val (_, errs) = typeCheckStmt(body, ctxWithArgs, newReturn)
        ctx -> errs
      case block: BlockStmt =>
        ctx -> typeCheckBlock(block, ctx, returnType)
      case ClassDef(name, _, superType, constructor, _, funcDefs, _) =>
        val ctxWithThis = ctx
          .newVar(ClassDef.thisSymbol, ctx.typeContext.typeUnfold(name))
          .newVar(
            ClassDef.superSymbol,
            superType
              .map(ctx.typeContext.typeUnfold)
              .getOrElse(obj())
          )
        var errors = Set[TypeCheckError]()
        (funcDefs :+ constructor).foreach { stmt =>
          val (_, e) = typeCheckStmt(stmt, ctxWithThis, returnType)
          errors ++= e
        }
        ctx -> errors
      case _ =>
        throw new NotImplementedError(s"GTHoles in Stmt: $stmt")
    }
  }

  def makeSureInBlock(stmt: GStmt): BlockStmt = {
    stmt match {
      case b: BlockStmt => b
      case _            => BlockStmt(Vector(stmt))
    }
  }
}
