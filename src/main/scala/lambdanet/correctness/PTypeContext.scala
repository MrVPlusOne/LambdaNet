package lambdanet.correctness

import lambdanet.LibDefs
import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph._

case class PTypeContext(
    typeUnfold: Map[PNode, PExpr],
    libDefs: LibDefs,
    subRel: Set[(PType, PType)]
) {

  /**
    * Plan:
    * 1. If c and p are PNode, great
    * 2. If they are other PExpr, because PExpr is not recursive, map all fields to PType
    * 3. Now we have either PType or a bunch of PType
    * 3.1. Can some PCall or PAccess be invalid? Jiayi says no. Need to check how they are generated
    * 4. PObject => PObjectType, PFunc => PFuncType, PCall => PFuncType.to, PAccess => PObjectType.fields(symbol)
    * 5. Now c and p are both PTypes, recursively check
    */
  def isSubtype(
      child: PNode,
      parent: PNode,
      assignment: Map[PNode, PType]
  ): Boolean = {
    val maybeSubRel = for {
      childType <- toType(child, assignment, subRel)
      parentType <- toType(parent, assignment, subRel)
      result <- checkSubtype(childType, parentType, assignment, subRel)
    } yield result
    maybeSubRel.nonEmpty
  }

  /**
    * Adapted from GType.checkSubtype
    */
  def checkSubtype(
      child: PType,
      parent: PType,
      assignment: Map[PNode, PType],
      subRel: Set[(PType, PType)]
  ): Option[Set[(PType, PType)]] = {
    if (child == PAny || parent == PAny || subRel.contains(child -> parent))
      return Some(subRel)
    lazy val nowSubRel = subRel + (child -> parent)
    (child, parent) match {
      case (c: PTyVar, p: PTyVar) if c.madeFromLibTypes && p.madeFromLibTypes =>
        if (c == p || c.node.nameOr("") + "Constructor" == p.node.nameOr(""))
          Some(nowSubRel)
        else None
      case (PTyVar(id), _) =>
        if (typeUnfold.contains(id)) {
          if (child == parent)
            Some(nowSubRel)
          else
            toType(typeUnfold(id), assignment, nowSubRel)
              .flatMap(unf => checkSubtype(unf, parent, assignment, nowSubRel))
        } else {
          assert(child.madeFromLibTypes, s"unknown type: $id")
          None
        }
      case (_, PTyVar(id)) =>
        if (typeUnfold.contains(id)) {
          if (child == parent)
            Some(nowSubRel)
          else
            toType(typeUnfold(id), assignment, nowSubRel)
              .flatMap(unf => checkSubtype(child, unf, assignment, nowSubRel))
        } else {
          assert(parent.madeFromLibTypes, s"unknown type: $id")
          None
        }
      case (PFuncType(cArgs, cReturn), PFuncType(pArgs, pReturn)) =>
        if (cArgs.length != pArgs.length) {
          return None
        }
        var currentSubRel = nowSubRel
        cArgs.zip(pArgs).foreach {
          case (c, p) =>
            checkSubtype(p, c, assignment, currentSubRel) match {
              case None     => return None
              case Some(sr) => currentSubRel = sr
            }
        }
        checkSubtype(cReturn, pReturn, assignment, currentSubRel)
      case (PObjectType(cFields), PObjectType(pFields)) =>
        var currentSubRel = nowSubRel
        pFields.foreach {
          case (label, pType) =>
            cFields.get(label).flatMap { cType =>
              checkSubtype(cType, pType, assignment, currentSubRel)
            } match {
              case None     => return None
              case Some(sr) => currentSubRel = sr
            }
        }
        Some(currentSubRel)
      case _ => None
    }
  }

  def toType(
      expr: PExpr,
      assignment: Map[PNode, PType],
      subRel: Set[(PType, PType)]
  ): Option[PType] =
    expr match {
      case node: PNode =>
        if (node.fromProject)
          Some(assignment(node))
        else
          libDefs.nodeMapping(node).typeOpt
      case PFunc(args, returnType) =>
        Some(PFuncType(args.map(assignment(_)), assignment(returnType)))
      case PCall(f, args) =>
        val argTypes = args.map(assignment(_))
        assignment(f) match {
          case PFuncType(paramTypes, to) if argTypes.zip(paramTypes).forall {
                case (child, parent) =>
                  checkSubtype(child, parent, assignment, subRel).nonEmpty
              } =>
            Some(to)
          case _ => None
        }
      case PObject(fields) =>
        Some(PObjectType(fields.mapValues(assignment(_)))) // TODO: or mapValuesNow?
      case PAccess(obj, label) =>
        assignment(obj) match {
          case PObjectType(fields) => fields.get(label)
          case _                   => None
        }
    }
}

object PTypeContext {
  def apply(
      graph: PredicateGraph,
      libDefs: LibDefs,
      subRel: Set[(PType, PType)] = Set.empty
  ): PTypeContext = {
    val defineRels = graph.predicates.collect {
      case p: DefineRel => p
    }
    val typeUnfold = defineRels.map {
      case DefineRel(v, expr) => (v, expr)
    }.toMap
    PTypeContext(typeUnfold, libDefs, subRel)
  }
}
