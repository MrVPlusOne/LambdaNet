package lambdanet.utils

import ammonite.ops.{RelPath, pwd, up}
import funcdiff.SimpleMath
import lambdanet.PrepareRepos.{ParsedProject, libDefsFile, parseProject}
import lambdanet.translation.ImportsResolution.ErrorHandler
import lambdanet.translation.PredicateGraph
import lambdanet.translation.PredicateGraph._
import lambdanet.{LibDefs, announced}

case class PTypeContext(
  typeUnfold: Map[PNode, PExpr],
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
  def isSubtype(child: PNode, parent: PNode, assignment: Map[PNode, PType]): Boolean = {
    val maybeSubRel = for {
      childType <- toType(child, assignment)
      parentType <- toType(parent, assignment)
      result <- checkSubtype(childType, parentType, assignment, subRel)
    } yield result
    maybeSubRel.nonEmpty
  }

  /**
    * Adapted from GType.checkSubtype
    */
  def checkSubtype(child: PType, parent: PType, assignment: Map[PNode, PType], subRel: Set[(PType, PType)]): Option[Set[(PType, PType)]] = {
    if (child == PAny || parent == PAny || subRel.contains(child -> parent))
      return Some(subRel)
    lazy val nowSubRel = subRel + (child -> parent)
    (child, parent) match {
      case (c: PTyVar, p: PTyVar) if c.madeFromLibTypes && p.madeFromLibTypes =>
        if (c == p) Some(nowSubRel) else None
      case (PTyVar(id), _) =>
        if (typeUnfold.contains(id)) {
          if (child == parent)
            Some(nowSubRel)
          else
            toType(typeUnfold(id), assignment).flatMap(unf => checkSubtype(unf, parent, assignment, nowSubRel))
        } else {
          assert(child.madeFromLibTypes, s"unknown type: $id")
          None
        }
      case (_, PTyVar(id)) =>
        if (typeUnfold.contains(id)) {
          if (child == parent)
            Some(nowSubRel)
          else
            toType(typeUnfold(id), assignment).flatMap(unf => checkSubtype(child, unf, assignment, nowSubRel))
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
              case None => return None
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
              case None => return None
              case Some(sr) => currentSubRel = sr
            }
        }
        Some(currentSubRel)
      case _ => None
    }
  }

  def toType(expr: PExpr, assignment: Map[PNode, PType]): Option[PType] =
    expr match {
      case node: PNode => Some(assignment(node))
      case PFunc(args, returnType) => Some(PFuncType(args.map(assignment(_)), assignment(returnType)))
      // TODO: Should I check if args are assigned to the right types?
      case PCall(f, _) =>
        assignment(f) match {
          case PFuncType(_, to) => Some(to)
          case _ => None
        }
      case PObject(fields) => Some(PObjectType(fields.mapValues(assignment(_)))) // TODO: or mapValuesNow?
      case PAccess(obj, label) =>
        assignment(obj) match {
          case PObjectType(fields) => fields.get(label)
          case _ => None
        }
    }
}

object PTypeContext {
  def apply(graph: PredicateGraph, subRel: Set[(PType, PType)] = Set.empty): PTypeContext = {
    val defineRels = graph.predicates.collect {
      case p: DefineRel => p
    }
    // TODO: When expr is PNode, we can optimize the lookup process by using union-find sets
    val typeUnfold = defineRels.map {
      case DefineRel(v, expr) => (v, expr)
    }.toMap
    PTypeContext(typeUnfold, subRel)
  }
}

object TypeChecker {
  def loadGraph(relPath: String): PredicateGraph = {
    val libDefs =
      announced(s"loading library definitions from $libDefsFile...") {
        SimpleMath.readObjectFromFile[LibDefs](libDefsFile.toIO)
      }

    val dir = pwd / RelPath(
      relPath
    )
    val parsed @ ParsedProject(_, qModules, irModules, g) =
      parseProject(
        libDefs,
        dir / up,
        dir,
        skipSet = Set(),
        errorHandler =
          ErrorHandler(ErrorHandler.StoreError, ErrorHandler.StoreError),
        shouldPrintProject = true
      ).mergeEqualities
    g
  }

  def violate(graph: PredicateGraph,
              assignment: Map[PNode, PType],
             ): Set[BinaryRel] = {
    assert(assignment.keySet == graph.nodes, "Assignment does not fully match nodes on the graph")
    val context = PTypeContext(graph)
    val binaryRels = graph.predicates.collect {
      case p: BinaryRel => p
    }
    binaryRels.filter(!satisfy(_, context, assignment))
  }

/*
  def sequenceMap[K, V](map: Map[K, Option[V]]): Option[Map[K, V]] =
    map.foldLeft(Map.empty[K, V]) {
      case (None, (_, _)) | (z, (_, None)) => None
      case (Some(acc), (k, Some(v))) => Some(acc + (k -> v))
    }

  def toGType(pType: PType, assignment: Map[PNode, PType]): Option[GType] =
    pType match {
      case PredicateGraph.PAny => Some(AnyType)
      case PredicateGraph.PTyVar(node) =>
        toGType(assignment(node), assignment) match {
          case Some(groundType: GroundType) => Some(TyVar(groundType.id))
          case _ => None
        }
      case PredicateGraph.PFuncType(args, to) =>
        for {
          gArgs <- args.map(toGType(_, assignment)).toList.sequence
          gTo <- toGType(to, assignment)
        } yield FuncType(gArgs, gTo)
      case PredicateGraph.PObjectType(fields) =>
        for {
          gFields <- sequenceMap(fields.mapValuesNow(toGType(_, assignment)))
        } yield ObjectType(gFields)
    }

  def toGType(pExpr: PExpr, assignment: Map[PNode, PType]): Option[GType] =
    pExpr match {
      case node: PNode => toGType(assignment(node), assignment)
      case PredicateGraph.PFunc(args, returnType) =>
        toGType(PFuncType(args.map(assignment(_)), assignment(returnType)), assignment)
      case PredicateGraph.PCall(f, args) => ???
      case PredicateGraph.PObject(fields) =>
        toGType(PObjectType(fields.mapValuesNow(assignment(_))), assignment)
      case PredicateGraph.PAccess(obj, label) =>
        for {
          shouldBeObj <- toGType()
        }
    }
*/

  def satisfy(predicate: BinaryRel,
              context: PTypeContext,
              assignment: Map[PNode, PType]
             ): Boolean = {
    val lhs = predicate.lhs
    val rhs = predicate.rhs
    predicate.category match {
      case BinaryRelCat.subtype | BinaryRelCat.inheritance => context.isSubtype(lhs, rhs, assignment)
      // TODO: Should we also check rhsType <: lhsType?
      case BinaryRelCat.assign | BinaryRelCat.equal => context.isSubtype(lhs, rhs, assignment)
      case BinaryRelCat.fixType | BinaryRelCat.fixAnnotation =>
        context.isSubtype(lhs, rhs, assignment) && context.isSubtype(rhs, lhs, assignment)
    }
  }
}
