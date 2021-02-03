import * as ts from "typescript";
import {ExportSpecifier, NodeArray, SyntaxKind} from "typescript";


export class GModule {
  constructor(public name: string, public stmts: GStmt[]) {
  }
}

export function mustExist<T>(v?: T, msg?: string): T {
  if (!v) {
    if (msg) {
      throw new Error("Must exists! Message: " + msg);
    } else {
      throw new Error("Must exists!");
    }
  }
  return v;
}

type SrcSpan = [[number, number], [number, number]]

// ASTs
type GMark = UserAnnot | Inferred | "missing";

class UserAnnot {
  public readonly category = "UserAnnot";

  constructor(public ty: GType) {
  }
}

class Inferred {
  public readonly category = "Inferred";

  constructor(public ty: GType) {
  }
}


type GType = TVar | AnyType | FuncType | ObjectType

class TVar {
  public readonly category = "TVar";

  constructor(public name: string) {
    mustExist(name);
  }
}

class AnyType {
  public readonly category = "AnyType";
  public readonly name = "any";

  private constructor() {
  }

  static instance = new AnyType();
}

class FuncType {
  public readonly category = "FuncType";

  constructor(public args: GType[], public to: GType) {
  }
}

class ObjectType {
  public readonly category = "ObjectType";

  constructor(public fields: NamedValue<GType>[]) {
  }
}

export const anyType = AnyType.instance;

let basicTypes = new Map<SyntaxKind, string>();
basicTypes.set(SyntaxKind.BooleanKeyword, "boolean");
basicTypes.set(SyntaxKind.TrueKeyword, "boolean");
basicTypes.set(SyntaxKind.FalseKeyword, "boolean");
basicTypes.set(SyntaxKind.NumberKeyword, "number");
basicTypes.set(SyntaxKind.StringKeyword, "string");
basicTypes.set(SyntaxKind.SymbolKeyword, "Symbol");
basicTypes.set(SyntaxKind.EnumKeyword, "Enum");
basicTypes.set(SyntaxKind.VoidKeyword, "void");
basicTypes.set(SyntaxKind.ObjectKeyword, "object");
basicTypes.set(SyntaxKind.BigIntKeyword, "BigInt");

let ignoredTypes = new Set<SyntaxKind>();
ignoredTypes.add(SyntaxKind.MappedType);
ignoredTypes.add(SyntaxKind.ConditionalType);
ignoredTypes.add(SyntaxKind.ThisType);
ignoredTypes.add(SyntaxKind.UnknownKeyword);
ignoredTypes.add(SyntaxKind.IndexedAccessType);
ignoredTypes.add(SyntaxKind.UndefinedKeyword);
ignoredTypes.add(SyntaxKind.NeverKeyword);
ignoredTypes.add(SyntaxKind.TypeOperator);
ignoredTypes.add(SyntaxKind.NullKeyword);

function parseTVars(n: { typeParameters?: ts.NodeArray<ts.TypeParameterDeclaration> }): string[] {
  return n.typeParameters ? n.typeParameters.map(p => p.name.text) : [];
}

/** Replace all occurrences of type variables with any  */
function eliminateTypeVars(ty: GType, tVars: string[]): GType {
  switch (ty.category) {
    case "TVar":
      if (tVars.includes(ty.name)) {
        return anyType;
      } else {
        return ty;
      }
    case "FuncType": {
      let newFrom = ty.args.map(t => eliminateTypeVars(t, tVars));
      let newTo = eliminateTypeVars(ty.to, tVars);
      return new FuncType(newFrom, newTo);
    }
    case "ObjectType": {
      let nf = ty.fields.map(nv => new NamedValue(nv.name, eliminateTypeVars(nv.value, tVars)));
      return new ObjectType(nf);
    }
    case "AnyType":
      return ty;
    default:
      throw new Error("Unknown category: " + JSON.stringify(ty));
  }
}

function parseSignatureType(sig: ts.SignatureDeclarationBase): FuncType {
  let tVars = parseTVars(sig);
  let argTypes = sig.parameters.map(p =>
    p.type ? eliminateTypeVars(parseTypeNode(mustExist(p.type)), tVars) : anyType);
  let retType = sig.type ? eliminateTypeVars(parseTypeNode(sig.type), tVars) : new TVar("void");
  return new FuncType(argTypes, retType);
}

function parseDeclarationName(n: ts.DeclarationName): string {
  switch (n.kind) {
    case SyntaxKind.Identifier:
      return n.text;
    case SyntaxKind.StringLiteral:
      return n.text;
    case SyntaxKind.NumericLiteral:
      return n.text;
    default:
      return "UnhandledDeclarationName";
  }
}

function parseTypeMember(member: ts.NamedDeclaration): NamedValue<GType> {
  if (member.name) {
    if (SyntaxKind.PropertyDeclaration == member.kind || SyntaxKind.PropertySignature == member.kind) {
      const x = (member as ts.PropertyDeclaration | ts.PropertySignature);

      return (new NamedValue(parseDeclarationName(x.name), x.type ? parseTypeNode(x.type) : anyType));
    } else if (SyntaxKind.MethodSignature == member.kind || SyntaxKind.MethodDeclaration == member.kind) {
      const x = (member as ts.MethodSignature | ts.MethodDeclaration);
      return (new NamedValue(
        parseDeclarationName(x.name),
        parseSignatureType(x as ts.MethodSignature)));
    } else {
      throw new Error("Unknown type member kind: " + SyntaxKind[member.kind]);
    }
  } else if (([SyntaxKind.IndexSignature, SyntaxKind.CallSignature,
    SyntaxKind.ConstructSignature] as SyntaxKind[]).includes(member.kind)) {
    let sig = member as ts.IndexSignatureDeclaration | ts.CallSignatureDeclaration | ts.ConstructSignatureDeclaration;
    let methodName = sig.kind == SyntaxKind.IndexSignature ? "access"
      : (sig.kind == SyntaxKind.ConstructSignature ? "CONSTRUCTOR" : "call");
    return (new NamedValue(methodName,
      parseSignatureType(sig)));
  } else {
    throw new Error("Unknown type element: " + ts.SyntaxKind[member.kind]);
  }
}


function parseEntityName(n: ts.EntityName): string {
  if (n.kind == SyntaxKind.Identifier) {
    return n.text;
  } else {
    return parseEntityName(n.left) + "." + n.right.text;
  }
}

function parseTypeNode(node: ts.TypeNode): GType {
  if (node.kind == SyntaxKind.AnyKeyword || node.kind == SyntaxKind.ThisKeyword) {
    return anyType;
  } else if (ts.isTypeReferenceNode(node)) {
    let n = node as ts.TypeReferenceNode;
    return new TVar(parseEntityName(n.typeName));
  } else if (basicTypes.has(node.kind)) {
    return new TVar(basicTypes.get(node.kind)!);
  } else if (node.kind == SyntaxKind.ArrayType) {
    return new TVar("Array");
  } else if (node.kind == SyntaxKind.FunctionType || node.kind == SyntaxKind.ConstructorType) {
    let n = node as ts.FunctionOrConstructorTypeNode;
    let ret: GType = parseTypeNode(n.type);
    let args: GType[] = n.parameters.map(p => {
      return p.type ? parseTypeNode(p.type) : anyType;
    });

    return eliminateTypeVars(new FuncType(args, ret), parseTVars(n));
  } else if (node.kind == SyntaxKind.TypeLiteral) {
    let n = node as ts.TypeLiteralNode;
    let members = n.members.map(parseTypeMember);
    return new ObjectType(members);
  } else if (node.kind == SyntaxKind.UnionType) {
    let n = node as ts.UnionTypeNode;
    let filtered = n.types.filter(t => {
      let text = t.getText()
      return text != "undefined" && text != "null"
    })
    if(filtered.length == 1)
      return parseTypeNode(filtered[0]);
    else
    return anyType;
  } else if (ignoredTypes.has(node.kind)) {
    return anyType;
  } else if (node.kind == SyntaxKind.LiteralType) {
    let n = node as ts.LiteralTypeNode;
    switch (n.literal.kind) {
      case SyntaxKind.StringLiteral:
        return new TVar("string");
      case SyntaxKind.TrueKeyword:
      case SyntaxKind.FalseKeyword:
        return new TVar("boolean");
      case SyntaxKind.NumericLiteral:
        return new TVar("number");
      default:
        return anyType;
    }
  } else if (node.kind == SyntaxKind.IntersectionType) {
    return anyType;
  } else if (node.kind == SyntaxKind.ParenthesizedType) {
    let n = node as ts.ParenthesizedTypeNode;
    return parseTypeNode(n.type);
  } else if (node.kind == SyntaxKind.FirstTypeNode || node.kind == SyntaxKind.LastTypeNode) {
    return new TVar("boolean");
  } else if (node.kind == SyntaxKind.TupleType) {
    return new TVar("Array");
  } else if (node.kind == SyntaxKind.TypeQuery) {
    return anyType; // fixme: handle type query
  } else {
    throw new Error("Unknown Type Kind: " + ts.SyntaxKind[node.kind]);
  }
}

class NamedValue<V> {
  constructor(public name: string, public value: V) {
  }
}

interface GExpr {
  category: string
  mark: GMark
}

class Var implements GExpr {
  category: string = "Var";
  mark: GMark = "missing";

  constructor(public name: string) {
    mustExist(name);
  }
}

class Const implements GExpr {
  category: string = "Const";
  mark: GMark;

  constructor(public value: string, public ty: GType, public line: number) {
    mustExist(value);
    this.mark = new Inferred(ty);
  }
}

class Cast implements GExpr {
  category: "Cast" = "Cast";
  mark: GMark;

  constructor(public expr: GExpr, public ty: GType) {
    mustExist(expr);
    this.mark = new Inferred(ty);
  }
}

class FuncCall implements GExpr {
  category: string = "FuncCall";

  constructor(public f: GExpr, public args: GExpr[], public mark: GMark) {
  }
}

class ObjLiteral implements GExpr {
  category: string = "ObjLiteral";

  constructor(public fields: NamedValue<GExpr>[], public mark: GMark) {
  }
}

class Access implements GExpr {
  category: string = "Access";

  constructor(public expr: GExpr, public field: string, public mark: GMark) {
    mustExist(field);
  }
}

class IfExpr implements GExpr {
  category: string = "IfExpr";

  constructor(public cond: GExpr, public e1: GExpr, public e2: GExpr, public mark: GMark) {
  }
}


export interface GStmt {
  category: string
}

class VarDef implements GStmt {
  category: string = "VarDef";

  constructor(public x: string, public mark: GMark,
              public init: GExpr | null, public isConst: boolean,
              public modifiers: string[],
              public srcSpan: SrcSpan | null) {
    mustExist(x);
  }
}

class AssignStmt implements GStmt {
  category: string = "AssignStmt";

  constructor(public lhs: GExpr, public rhs: GExpr) {
  }
}

class ExprStmt implements GStmt {
  category: string = "ExprStmt";

  constructor(public expr: GExpr, public isReturn: boolean) {
  }
}

class IfStmt implements GStmt {
  category: string = "IfStmt";

  constructor(public cond: GExpr, public branch1: GStmt, public branch2: GStmt) {
  }
}

class WhileStmt implements GStmt {
  category: string = "WhileStmt";

  constructor(public cond: GExpr, public body: GStmt) {
  }
}

type ImportStmt = ImportSingle | ImportDefault | ImportModule

class ImportSingle {
  category: "ImportSingle" = "ImportSingle";

  constructor(public oldName: string, public newName: string, public path: string) {
  }
}

class ImportDefault {
  category: "ImportDefault" = "ImportDefault";

  constructor(public newName: string, public path: string) {
  }
}

class ImportModule {
  category: "ImportModule" = "ImportModule";

  constructor(public newName: string, public path: string) {
  }
}

type ExportStmt = ExportSingle | ExportDefault | ExportModule

class ExportSingle {
  category: "ExportSingle" = "ExportSingle";

  constructor(public oldName: string, public newName: string, public from: string | null) {
  }
}

class ExportDefault {
  category: "ExportDefault" = "ExportDefault";

  constructor(public newName: string | null, public from: string | null) {
  }
}

class ExportModule {
  category: "ExportModule" = "ExportModule";

  constructor(public from: string) {
  }
}


class NamespaceAliasStmt implements GStmt {
  category: string = "NamespaceAliasStmt";

  constructor(public name: string, public rhs: string) {
  }
}

class TypeAliasStmt implements GStmt {
  category: string = "TypeAliasStmt";

  constructor(public name: string, public tyVars: string[], public type: GType,
              public modifiers: string[], public superTypes: string[]) {
    mustExist(name);
    mustExist(tyVars);
    mustExist(type);
    mustExist(modifiers);
  }
}

class CommentStmt implements GStmt {
  category: string = "CommentStmt";

  constructor(public text: string) {
    mustExist(text);
  }
}

class BlockStmt implements GStmt {
  category: string = "BlockStmt";

  constructor(public stmts: GStmt[]) {
  }
}

class NamespaceStmt implements GStmt {
  category: string = "NamespaceStmt";

  constructor(public name: string, public block: BlockStmt, public modifiers: string[]) {
  }
}


class FuncDef implements GStmt {
  category: string = "FuncDef";

  constructor(public name: string,
              public args: NamedValue<[GMark, SrcSpan]>[],
              public returnType: [GMark, SrcSpan | null],
              public body: GStmt, public modifiers: string[], public tyVars: string[]) {
    mustExist(name);
  }
}

class Constructor extends FuncDef {
  constructor(name: string,
              args: NamedValue<[GMark, SrcSpan | null]>[],
              returnType: GMark,
              body: GStmt, modifiers: string[], tyVars: string[],
              public publicVars: string[]) {
    super(name, args, [returnType, null], body, modifiers, tyVars);
    mustExist(publicVars);
  }
}

class ClassDef implements GStmt {
  category: string = "ClassDef";

  constructor(public name: string, public constr: Constructor | null,
              public instanceLambdas: FuncDef[],
              public staticLambdas: FuncDef[],
              public vars: NamedValue<[GMark, GExpr | null, boolean, SrcSpan]>[],
              public funcDefs: [FuncDef, boolean][],
              public superTypes: string[], public modifiers: string[],
              public tyVars: string[]) {
  }
}

type SupportedExpression =
  ts.Identifier |
  ts.ThisExpression |
  ts.SuperExpression |
  ts.CallExpression |
  ts.NewExpression |
  ts.ObjectLiteralExpression |
  ts.PropertyAccessExpression |
  ts.ElementAccessExpression |
  ts.ConditionalExpression |
  ts.ParenthesizedExpression |
  LiteralExpression |
  ts.BinaryExpression |
  ts.PrefixUnaryExpression | ts.PostfixUnaryExpression |
  ts.FunctionExpression | ts.ArrowFunction |
  ts.ImportExpression |
  SpecialExpressions


type LiteralExpression =
  ts.NumericLiteral |
  ts.StringLiteral |
  ts.RegularExpressionLiteral |
  ts.BooleanLiteral |
  ts.NullLiteral |
  ts.VoidExpression |
  ts.ArrayLiteralExpression |
  ts.NoSubstitutionTemplateLiteral

type JsxExpressions = ts.JsxElement | ts.JsxSelfClosingElement

type SpecialExpressions =
  ts.SpreadElement |
  ts.TypeOfExpression |
  ts.TemplateExpression |
  ts.TaggedTemplateExpression |
  ts.DeleteExpression |
  ts.YieldExpression |
  ts.AsExpression |
  ts.TypeAssertion |
  ts.AwaitExpression |
  ts.NonNullExpression |
  ts.ClassExpression |
  ts.OmittedExpression |
  ts.MetaProperty |
  JsxExpressions

export function parseExpr(node: ts.Expression,
                          allocateLambda: (f: ts.FunctionLikeDeclaration) => Var,
                          checker: ts.TypeChecker): GExpr {

  function rec(node: ts.Expression): GExpr {
    const n = node as SupportedExpression;
    mustExist(n);

    function infer() {
      return parseGMark(undefined, node, checker);
    }

    switch (n.kind) {
      case SyntaxKind.Identifier: {
        let name = n.text;
        return new Var(name);
      }
      case SyntaxKind.ThisKeyword:
        return SpecialVars.THIS;
      case SyntaxKind.SuperKeyword:
        return SpecialVars.SUPER;
      case SyntaxKind.CallExpression: {
        let f = rec(n.expression);
        let args = n.arguments.map(rec);
        return new FuncCall(f, args, infer());
      }
      case SyntaxKind.NewExpression: {
        let args = n.arguments ? n.arguments.map(rec) : [];
        let f = new Access(rec(n.expression), "CONSTRUCTOR", "missing");
        return new FuncCall(f, args, infer());
      }
      case SyntaxKind.ObjectLiteralExpression: {
        const fields = flatMap(n.properties, (p: ts.ObjectLiteralElementLike) => {
          if (p.kind == SyntaxKind.PropertyAssignment ||
            p.kind == SyntaxKind.ShorthandPropertyAssignment) {
            return [parseObjectLiteralElementLike(p)];
          } else {
            return []; //todo: other cases
          }
        });
        return new ObjLiteral(fields, infer());
      }
      case SyntaxKind.PropertyAccessExpression: {
        let lhs = rec(n.expression);
        return new Access(lhs, n.name.text, infer());
      }
      case ts.SyntaxKind.ElementAccessExpression: {
        let thing = rec(n.expression);
        let index = rec(n.argumentExpression);
        return new FuncCall(new Access(thing, "access", "missing"), [index], infer());
      }
      case ts.SyntaxKind.ConditionalExpression: {
        let cond = rec(n.condition);
        let e1 = rec(n.whenTrue);
        let e2 = rec(n.whenFalse);
        return new IfExpr(cond, e1, e2, infer());
      }
      case ts.SyntaxKind.ParenthesizedExpression: {
        return rec(n.expression);
      }

      // constants
      case SyntaxKind.NumericLiteral:
        return constExpr("number");
      case SyntaxKind.StringLiteral:
        return constExpr("string");
      case SyntaxKind.RegularExpressionLiteral:
        return constExpr("RegExp");
      case SyntaxKind.TrueKeyword:
      case SyntaxKind.FalseKeyword:
        return constExpr("boolean");
      case SyntaxKind.NullKeyword:
        return constExpr(anyType.name, "null");
      case SyntaxKind.VoidExpression: {
        return constExpr("void", "void");
      }

      case SyntaxKind.ArrayLiteralExpression: {
        const a = node as ts.ArrayLiteralExpression;
        const exs = a.elements.map(rec);
        return new FuncCall(new Var("Array"), exs, infer());
      }

      // operators
      case ts.SyntaxKind.BinaryExpression: {
        let l = rec(n.left);
        let r = rec(n.right);
        let opp = n.operatorToken.kind;

        return new FuncCall(new Var(ts.SyntaxKind[opp]), [l, r], infer());
      }
      case SyntaxKind.PrefixUnaryExpression:
      case SyntaxKind.PostfixUnaryExpression: {
        let opName = ts.SyntaxKind[n["operator"]];
        let fixity = (node.kind == SyntaxKind.PrefixUnaryExpression) ? "" : "POST_";
        let arg = rec(n["operand"]);
        return new FuncCall(new Var(fixity + opName), [arg], infer());
      }
      case SyntaxKind.ArrowFunction:
      case SyntaxKind.FunctionExpression: {
        try {
          return allocateLambda(n);
        } catch (e) {
          return undefinedValue;
        }
      }

      // Special treatments:
      case SyntaxKind.SpreadElement: {
        const n1 = (n as ts.SpreadElement).expression
        return new FuncCall(SpecialVars.spread, [rec(n1)], infer());
      }
      case SyntaxKind.TypeOfExpression: {
        return new FuncCall(SpecialVars.typeOf, [rec(n.expression)], infer());
      }
      case SyntaxKind.TaggedTemplateExpression: {
        const tagE = rec(n.tag);
        const temp = rec(n.template);
        return new FuncCall(tagE, [temp], infer());
      }
      case SyntaxKind.TemplateExpression: {
        const spans = n.templateSpans.map(sp => rec(sp.expression));
        return new FuncCall(SpecialVars.Template, spans, infer());
      }
      case SyntaxKind.NoSubstitutionTemplateLiteral:
        return constExpr("string");
      case SyntaxKind.DeleteExpression: {
        return new FuncCall(SpecialVars.DELETE, [rec(n.expression)], infer());
      }
      case SyntaxKind.YieldExpression: {
        return new FuncCall(SpecialVars.YIELD, [rec(mustExist(n.expression))], infer());
      }
      case SyntaxKind.AwaitExpression: {
        return new FuncCall(SpecialVars.AWAIT, [rec(n.expression)], infer());
      }
      case SyntaxKind.NonNullExpression: {
        return rec(n.expression);
      }
      case SyntaxKind.JsxElement:
      case SyntaxKind.JsxSelfClosingElement: {
        return undefinedValue;
      }
      case SyntaxKind.TypeAssertionExpression:
      case SyntaxKind.AsExpression: {
        const e = rec(n.expression);
        const t = parseTypeNode(n.type);
        return new Cast(e, t);
      }
      // type assertions are ignored
      case SyntaxKind.OmittedExpression:
      case SyntaxKind.ImportKeyword:
      case SyntaxKind.MetaProperty:
      case SyntaxKind.ClassExpression: {
        return undefinedValue; //todo: properly handle
      }

      default: {
        throw new Error("Unknown expression category: " + ts.SyntaxKind[node.kind]
          + ". Text: " + node.getText());
      }
    }

    function constExpr(typeName: string, value?: string): Const {
      // let v = (<ts.LiteralLikeNode>node).text;
      const v = value ? value : "???";
      return new Const(v, new TVar(typeName), getLineNumber(n));
    }

    function parseObjectLiteralElementLike(p: ts.PropertyAssignment | ts.ShorthandPropertyAssignment): NamedValue<GExpr> {
      //todo: properly handle other cases like accessors
      const fieldName = p.name.getText();
      const rhs = (p.kind == SyntaxKind.PropertyAssignment) ? rec(p.initializer) : new Var(fieldName);
      return new NamedValue<GExpr>(fieldName, rhs);
    }
  }

  return rec(node);
}

export const undefinedValue = new Var("undefined");

export function parseGMark(tyNode: ts.TypeNode | undefined,
                           node: ts.Node | undefined,
                           checker: ts.TypeChecker): GMark {
  if (!tyNode) {
    if (node) {
      const ty = checker.getTypeAtLocation(node);
      const n = checker.typeToTypeNode(ty);
      const t = n ? parseTypeNode(n) : anyType;
      if (t.category == "AnyType") {
        return "missing";
      } else {
        return new Inferred(t);
      }
    } else {
      return "missing";
    }
  } else {
    return new UserAnnot(parseTypeNode(tyNode));
  }
}

export class StmtParser {
  public nLambda: [number] = [0];

  constructor(public checker: ts.TypeChecker) {
  }

  parseStmt(node: ts.Node): GStmt[] {
    const checker = this.checker;

    function parseMark(tyNode: ts.TypeNode | undefined,
                       node: ts.Node | undefined) {
      return parseGMark(tyNode, node, checker);
    }

    let getNLambda = this.nLambda;

    class StmtsHolder {
      constructor(public stmts: GStmt[]) {
      }
    }

    class ExprProcessor {
      lambdaDefs: FuncDef[] = [];

      processExpr(e: ts.Expression): GExpr {
        let lambdas = this.lambdaDefs;

        function allocateLambda(f: ts.FunctionLikeDeclaration): Var {
          let n0 = (f as ts.FunctionExpression).name;

          let name: string;
          if (n0) {
            name = n0.getText();
          } else {
            name = "$Lambda" + getNLambda[0];
            getNLambda[0] += 1;
          }
          const srcSpan = n0 ? getSrcSpan(n0) : null
          lambdas.push(parseFunction(name, f, parseModifiers(f.modifiers), srcSpan));
          return new Var(name);
        }

        return parseExpr(e, allocateLambda, checker);
      }

      alongWith(...stmts: GStmt[]): StmtsHolder {
        return new StmtsHolder((<GStmt[]>this.lambdaDefs).concat(stmts));
      }

      alongWithMany(stmts: GStmt[]): StmtsHolder {
        return new StmtsHolder((<GStmt[]>this.lambdaDefs).concat(stmts));
      }
    }

    /**
     * returns the parsed FuncDef along with arguments that are marked
     * with 'public' (for constructors)
     */
    function parseFunction(name: string,
                           n: ts.FunctionLikeDeclaration | ts.IndexSignatureDeclaration,
                           modifiers: string[],
                           returnSrcSpan: SrcSpan | null): FuncDef {
      function inferRetType(): GMark {
        if (n.type) {
          return parseMark(n.type, undefined);
        }

        const tNode = checker.typeToTypeNode(checker.getTypeAtLocation(n));
        if (tNode) {
          const t = parseTypeNode(tNode);
          if (t.category == "FuncType") {
            return new Inferred(t.to);
          }
        }
        return "missing";
      }

      const isConstructor = ts.isConstructorDeclaration(n);
      const retType = inferRetType();

      let publicArgs: string[] = [];

      let bindingInArgs: boolean = false;
      let args = n.parameters.map(p => {
        let name: string;
        if (p.name.kind == SyntaxKind.Identifier) {
          name = p.name.text;
        } else {
          name = "_";
          bindingInArgs = true;
        }

        if (parseModifiers(p.modifiers).includes("public")) {
          publicArgs.push(name);
        }
        return new NamedValue<[GMark, SrcSpan]>(
          name,
          [parseMark(p.type, undefined), getSrcSpan(p.name)]);
      });

      let body: StmtsHolder;
      if (n.kind != SyntaxKind.IndexSignature && n.body && !bindingInArgs) {
        if (n.body.kind == SyntaxKind.Block) {
          body = rec(n.body as ts.Statement);
        } else {
          let ep = new ExprProcessor();
          // try to parse the body as a ConciseFunction body
          body = ep.alongWith(new ExprStmt(ep.processExpr((n.body as ts.Expression)), true));
        }
      } else {
        body = new ExprProcessor().alongWithMany([]);
      }

      let type_params = n.typeParameters;
      let t_vars: string[];
      if (type_params) {
        t_vars = type_params.map(n => n.name.text);
      } else {
        t_vars = [];
      }

      return isConstructor ?
        new Constructor(name, args, retType, flattenBlock(body.stmts), modifiers, t_vars, publicArgs) :
        new FuncDef(name, args, [retType, returnSrcSpan], flattenBlock(body.stmts), modifiers, t_vars);
    }


    function rec(node: ts.Node): StmtsHolder {
      return handleError(node, () => {
        mustExist(node);

        let EP = new ExprProcessor();

        function parseVarDecList(node: ts.VariableDeclarationList, modifiers: string[], rhs?: GExpr): VarDef[] {
          return handleError(node, () => {
            let isConst = (node.flags & ts.NodeFlags.Const) != 0;

            function parseVarDec(dec: ts.VariableDeclaration, rhs?: GExpr): VarDef[] {
              const rhs1 = rhs ? rhs : (dec.initializer ? EP.processExpr(dec.initializer) : null);
              return parseBindingName(dec.name, rhs1, dec.type);
            }

            function parseBindingName(lhs: ts.BindingName, rhs: GExpr | null, ty?: ts.TypeNode): VarDef[] {
              switch (lhs.kind) {
                case SyntaxKind.Identifier:
                  const vd = new VarDef(
                    lhs.text,
                    parseMark(ty, lhs),
                    rhs,
                    isConst,
                    modifiers,
                    getSrcSpan(lhs),
                  )
                  return [vd];
                case SyntaxKind.ObjectBindingPattern:
                  return flatMap(lhs.elements, (e: ts.BindingElement) => {
                    const fieldName = e.propertyName ? e.propertyName : e.name;
                    let fName: string;
                    switch (fieldName.kind) {
                      case SyntaxKind.Identifier:
                      case SyntaxKind.StringLiteral:
                      case SyntaxKind.ComputedPropertyName:
                      case SyntaxKind.NumericLiteral:
                        fName = parsePropertyName(fieldName);
                        break;
                      default:
                        fName = SpecialVars.UNKNOWN;
                        break;
                    }

                    const access = rhs ? new Access(rhs, fName, "missing") : null;
                    return parseBindingName(e.name, access);
                  });
                case SyntaxKind.ArrayBindingPattern: {
                  let arrayAccessed = rhs ? new FuncCall(SpecialVars.ArrayAccess, [rhs], "missing") : null;
                  return flatMap(lhs.elements, (e: ts.ArrayBindingElement) => {
                    if (e.kind == SyntaxKind.OmittedExpression) {
                      return [];
                    } else {
                      return parseBindingName((e as ts.BindingElement).name, arrayAccessed);
                    }
                  });
                }
              }
            }

            let dec = node.declarations;
            return flatMap(dec, (x: ts.VariableDeclaration) => parseVarDec(x, rhs));
          });
        }

        function isStatic(n: ts.ClassElement): boolean {
          return parseModifiers(n.modifiers).includes("static");
        }

        switch (node.kind) {
          case SyntaxKind.ThrowStatement:
          case SyntaxKind.ExpressionStatement: {
            let n = <ts.ExpressionStatement>node;
            if (n.expression.kind == SyntaxKind.BinaryExpression) {
              let e = n.expression as ts.BinaryExpression;
              if (e.operatorToken.kind == ts.SyntaxKind.FirstAssignment) {
                let l = EP.processExpr(e.left);
                let r = EP.processExpr(e.right);
                return EP.alongWith(new AssignStmt(l, r));
              }
            }
            let shouldReturn = n.expression.kind == SyntaxKind.YieldExpression;
            return EP.alongWith(new ExprStmt(EP.processExpr(n.expression), shouldReturn));
          }
          case SyntaxKind.ReturnStatement: {
            let n = <ts.ReturnStatement>node;
            return n.expression ?
              EP.alongWith(new ExprStmt(EP.processExpr(n.expression), true))
              : EP.alongWith(new CommentStmt("return;"));
          }
          case SyntaxKind.VariableStatement: {
            let n = node as ts.VariableStatement;
            let ms = parseModifiers(n.modifiers);
            let list = n.declarationList;
            return EP.alongWithMany(parseVarDecList(list, ms));
          }
          case SyntaxKind.IfStatement: {
            let n = node as ts.IfStatement;
            let cond = EP.processExpr(n.expression);
            let then = flattenBlock(rec(n.thenStatement).stmts);
            let otherwise: GStmt[];
            if (n.elseStatement == undefined) {
              otherwise = [new BlockStmt([])];
            } else {
              otherwise = rec(n.elseStatement).stmts;
            }
            return EP.alongWith(new IfStmt(cond, then, flattenBlock(otherwise)));
          }
          case SyntaxKind.DoStatement: // simply treat do as while
          case SyntaxKind.WhileStatement: {
            const n = node as ts.WhileStatement | ts.DoStatement;
            let cond = EP.processExpr(n.expression);
            let body = flattenBlock(rec(n.statement).stmts);
            return EP.alongWith(new WhileStmt(cond, body));
          }
          case SyntaxKind.Block: {
            let n = node as ts.Block;
            let stmts = flatMap(n.statements, (x: ts.Node) => rec(x).stmts);
            return EP.alongWith(new BlockStmt(stmts));
          }

          case SyntaxKind.ForOfStatement:
          case SyntaxKind.ForInStatement:
          case SyntaxKind.ForStatement: {
            let n = node as ts.ForStatement | ts.ForInOrOfStatement;
            let cond: GExpr = new Const("true", new TVar("boolean"), getLineNumber(n));
            let incr: GStmt[] = [];
            let expression: GExpr | undefined = undefined;
            if (n.kind == SyntaxKind.ForStatement) {
              if (n.condition) {
                cond = EP.processExpr(n.condition);
              }
              if (n.incrementor) {
                incr = [new ExprStmt(EP.processExpr(n.incrementor), false)];
              }
            } else {
              const rhs = EP.processExpr(n.expression);
              expression = new FuncCall(SpecialVars.ArrayAccess, [rhs], "missing");
            }
            let init = n.initializer;
            let outerBlock = new BlockStmt([]);

            if (init && ts.isVariableDeclarationList(init)) {
              outerBlock.stmts = parseVarDecList(init, [], expression);
            } else if (init) {
              outerBlock.stmts.push(new ExprStmt(EP.processExpr(init as ts.Expression), false));
            }
            let bodyStmts: GStmt[] = rec(n.statement).stmts.concat(incr);
            outerBlock.stmts.push(new WhileStmt(cond, flattenBlock(bodyStmts)));
            return EP.alongWith(outerBlock);
          }
          case SyntaxKind.FunctionDeclaration:
          case SyntaxKind.MethodDeclaration:
          case SyntaxKind.GetAccessor:
          case SyntaxKind.SetAccessor:
          case SyntaxKind.Constructor: {
            let name = (node.kind == SyntaxKind.Constructor) ? "Constructor" :
              useOrElse((node as any).name, (x: any) => parsePropertyName(x), "defaultFunc");
            let n = <ts.FunctionLikeDeclaration>node;
            const modifiers = parseModifiers(n.modifiers);
            if (node.kind == SyntaxKind.SetAccessor) {
              modifiers.push("set");
            } else if (node.kind == SyntaxKind.GetAccessor) {
              modifiers.push("get");
            }
            const srcSpan = n.name ? getSrcSpan(n.name) : null
            return EP.alongWith(parseFunction(name, n, modifiers, srcSpan));
          }

          case SyntaxKind.ClassDeclaration: {
            let n = node as ts.ClassDeclaration;

            const name = n.name ? n.name.text : "DefaultClass";

            let superTypes: string[] = [];
            if (n.heritageClauses) {
              let clauses = n.heritageClauses;
              for (const c of clauses) {
                superTypes.push(c.types[0].expression.getText());
              }
            }

            let vars: NamedValue<[GMark, GExpr | null, boolean, SrcSpan]>[] = [];

            let funcDefs: [FuncDef, boolean][] = [];
            let constructor: Constructor | null = null;

            // let isAbstract = n.modifiers && n.modifiers.map(x => x.kind).includes(SyntaxKind.AbstractKeyword);
            const instanceEp = new ExprProcessor();
            const staticEp = new ExprProcessor();

            for (const v of n.members) {
              const staticQ = isStatic(v);
              const ep = staticQ ? staticEp : instanceEp;
              if (ts.isPropertyDeclaration(v)) {
                let v1 = v as ts.PropertyDeclaration;
                const init = v1.initializer ? ep.processExpr(v1.initializer) : null;
                vars.push(new NamedValue(
                  parsePropertyName(v1.name),
                  [parseMark(v1.type, v1), init, staticQ, getSrcSpan(v1.name)]
                ));
              } else if (ts.isMethodDeclaration(v) || ts.isAccessor(v)) {
                funcDefs.push([getSingleton(rec(v).stmts) as FuncDef, staticQ]);
              } else if (ts.isConstructorDeclaration(v)) {
                const c = getSingleton(rec(v).stmts) as Constructor;
                c.args
                  .filter(v => c.publicVars.includes(v.name))
                  .forEach(p => vars.push(
                    new NamedValue<[GMark, GExpr | null, boolean, SrcSpan]>(
                      p.name, [p.value[0], null, false, p.value[1]])));
                constructor = c;
              } else if (ts.isIndexSignatureDeclaration(v)) {
                const n = v as ts.IndexSignatureDeclaration;
                const srcSpan = n.type ? getSrcSpan(n.type) : null
                parseFunction("access", n, parseModifiers(n.modifiers), srcSpan);
              } else if (ts.isSemicolonClassElement(v)) {
                // ignore
              } else {
                throw new Error("Unknown statements in class definitions: " + SyntaxKind[v.kind]);
              }
            }

            let classModifiers = parseModifiers(n.modifiers);

            let tVars = parseTVars(n);

            let classStmt = new ClassDef(name, constructor,
              instanceEp.lambdaDefs, staticEp.lambdaDefs,
              vars, funcDefs,
              superTypes, classModifiers, tVars);

            return EP.alongWith(classStmt);
          }
          case SyntaxKind.SwitchStatement: {
            let n = node as ts.SwitchStatement;

            let switchCall = new FuncCall(SpecialVars.SWITCH, [EP.processExpr(n.expression)], "missing");

            let clauses = flatMap(
              n.caseBlock.clauses,
              (c: ts.CaseOrDefaultClause) => {
                let body = flatMap(c.statements, (s: ts.Statement) => rec(s).stmts);
                switch (c.kind) {
                  case SyntaxKind.CaseClause:
                    let f = new FuncCall(SpecialVars.CASE, [EP.processExpr((c as ts.CaseClause).expression)], "missing");
                    let all = [new ExprStmt(f, false) as GStmt].concat(body);
                    return EP.alongWithMany(all).stmts;
                  case SyntaxKind.DefaultClause:
                    return EP.alongWithMany(body).stmts;
                }
              });
            return EP.alongWithMany([new ExprStmt(switchCall, false) as GStmt].concat(clauses));
          }

          case SyntaxKind.ImportEqualsDeclaration: {
            const n = node as ts.ImportEqualsDeclaration;
            const rhs = n.moduleReference;
            if (rhs.kind == SyntaxKind.ExternalModuleReference) {
              const newName = n.name.text;
              if (rhs.expression.kind == SyntaxKind.StringLiteral) {
                const path = (rhs.expression as ts.StringLiteral).text;
                return EP.alongWith(new ImportSingle("$ExportEquals", newName, path));
              } else {
                throw new Error(`Unknown import equals: ${n.getText()}`);
              }
            } else {
              return EP.alongWith(new NamespaceAliasStmt(n.name.getText(), rhs.getText()));
            }
          }
          case SyntaxKind.ImportDeclaration: {
            const n = node as ts.ImportDeclaration;
            const path = (n.moduleSpecifier as ts.StringLiteral).text;
            if (n.importClause) {
              if (n.importClause.name) {
                return EP.alongWith(new ImportDefault(n.importClause.name.text, path));
              }
              if (n.importClause.namedBindings) {
                const bindings = n.importClause.namedBindings;
                if (bindings.kind == SyntaxKind.NamespaceImport) {
                  return EP.alongWith(new ImportModule(bindings.name.text, path));
                } else {
                  const imports = bindings.elements.map(s => {
                    const newName = s.name.text;
                    if (s.propertyName) {
                      return new ImportSingle(s.propertyName.text, newName, path);
                    } else {
                      return new ImportSingle(newName, newName, path);
                    }
                  });
                  return EP.alongWithMany(imports);
                }
              }
            }
            return EP.alongWith();
          }
          case SyntaxKind.ExportAssignment: {
            const n = node as ts.ExportAssignment;
            const e = EP.processExpr(n.expression);
            if (n.isExportEquals == true) {
              const alias = new NamespaceAliasStmt("$ExportEquals", n.expression.getText());
              return EP.alongWith(alias);
              // return EP.alongWith(new VarDef("$ExportEquals", null, e, true,
              //   ["export"]));
            } else if (e.category == "Var") {
              return EP.alongWith(new ExportDefault((e as Var).name, null));
            } else {
              return EP.alongWith(
                new VarDef("defaultVar", parseMark(undefined, n.expression), e, true,
                ["export", "default"], null));
            }
          }
          case SyntaxKind.NamespaceExportDeclaration: {
            const n = node as ts.NamespaceExportDeclaration;
            //todo: check if this is the right way
            const name = n.name.text;
            return EP.alongWith(new ExportSingle(name, name, null));
          }
          case SyntaxKind.ExportDeclaration: {
            const n = node as ts.ExportDeclaration;
            const path = n.moduleSpecifier ? (n.moduleSpecifier as ts.StringLiteral).text : null;
            if (n.exportClause) {
              const clause: ts.NamedExports = n.exportClause
              const exports = clause.elements.map((s: ExportSpecifier) => {
                const newName = s.name.text;
                if (s.propertyName) {
                  return new ExportSingle(s.propertyName.text, newName, path);
                } else {
                  return new ExportSingle(newName, newName, path);
                }
              });
              return EP.alongWithMany(exports);
            } else {
              return EP.alongWith(new ExportModule(path!));
            }
          }
          case SyntaxKind.EnumDeclaration: {
            const enumEquiv = new TVar("number");
            const n = node as ts.EnumDeclaration;
            const vars = n.members.map(member => {
              let vName = member.name.getText();
              return new NamedValue(vName,
                new Const("ENUM", enumEquiv, getLineNumber(n)));
            });
            const rhs = new ObjLiteral(vars, "missing");
            const mds = parseModifiers(n.modifiers);
            return EP.alongWithMany([
              new VarDef(n.name.text, "missing", rhs,
                true, mds, getSrcSpan(n.name)),
              new TypeAliasStmt(n.name.text, [], enumEquiv, mds, [])
            ]);
          }
          case SyntaxKind.InterfaceDeclaration: {
            let n = node as ts.InterfaceDeclaration;

            let superTypes: string[] = [];
            if (n.heritageClauses) {
              let clauses = n.heritageClauses;
              for (const c of clauses) {
                superTypes.push(c.types[0].expression.getText());
              }
            }

            let tVars = parseTVars(n);
            let members = n.members.map(parseTypeMember);
            let objT = new ObjectType(members);
            return EP.alongWith(
              new TypeAliasStmt(n.name.text, tVars, objT, parseModifiers(n.modifiers), superTypes));
          }
          case SyntaxKind.TypeAliasDeclaration: {
            let n = node as ts.TypeAliasDeclaration;
            let tVars = parseTVars(n);
            return EP.alongWith(
              new TypeAliasStmt(
                n.name.text,
                tVars,
                parseTypeNode(n.type),
                parseModifiers(n.modifiers),
                []));
          }
          case SyntaxKind.TryStatement: {
            let n = node as ts.TryStatement;

            let tryPart = rec(n.tryBlock).stmts;
            let finallyPart = n.finallyBlock ? rec(n.finallyBlock).stmts : [];
            return EP.alongWithMany(tryPart.concat(finallyPart));
          }

          case SyntaxKind.ModuleDeclaration: {
            const n = node as ts.ModuleDeclaration;
            const name = n.name.text;
            const body = n.body;
            if (body) {
              switch (body.kind) {
                case ts.SyntaxKind.ModuleBlock: {
                  const stmts = flatMap(body.statements, (x: ts.Node) => rec(x).stmts);
                  const modifiers = parseModifiers(n.modifiers);
                  const r = new NamespaceStmt(name, new BlockStmt(stmts), modifiers);
                  return EP.alongWith(r);
                }
                case ts.SyntaxKind.ModuleDeclaration: {
                  const modifiers = parseModifiers(n.modifiers);
                  const r = new NamespaceStmt(name, new BlockStmt(rec(body).stmts), modifiers);
                  return EP.alongWith(r);
                }
                default:
                  throw new Error("Module declare body? Text: \n" + body.getText());
              }
            }
            return EP.alongWith();
          }
          case SyntaxKind.LabeledStatement: {
            const n = node as ts.LabeledStatement;
            return rec(n.statement);
          }

          // ignored statements:
          case SyntaxKind.DebuggerStatement:
          case SyntaxKind.BreakStatement:
          case SyntaxKind.ContinueStatement:
            return EP.alongWith(new CommentStmt(node.getText()));

          case SyntaxKind.EmptyStatement:
            return EP.alongWithMany([]);

          default:
            throw new Error("Unknown stmt category: " + ts.SyntaxKind[node.kind]);
        }
      });
    }

    function parsePropertyName(name: ts.PropertyName): string {
      switch (name.kind) {
        case ts.SyntaxKind.Identifier:
          return name.text;
        case ts.SyntaxKind.ComputedPropertyName:
          return SpecialVars.ComputedPropertyName;
        case ts.SyntaxKind.NumericLiteral:
          return name.getText();
        case ts.SyntaxKind.StringLiteral:
          return name.text;
      }
    }

    return rec(node).stmts;
  }
}

function parseModifiers(modifiersNode?: ts.ModifiersArray): string[] {
  if (modifiersNode) {
    return flatMap(modifiersNode, (m: ts.Modifier) => {
      switch (m.kind) {
        case SyntaxKind.ExportKeyword:
          return ["export"];
        case SyntaxKind.DefaultKeyword:
          return ["default"];
        case SyntaxKind.ConstKeyword:
          return ["const"];
        case SyntaxKind.StaticKeyword:
          return ["static"];
        case SyntaxKind.PublicKeyword:
          return ["public"];
        case SyntaxKind.AsyncKeyword:
          return ["async"];
        default:
          return [];
      }
    })
  }
  return [];
}

export function flattenBlock(stmts: GStmt[]): GStmt {
  if (stmts.length == 1) {
    return stmts[0];
  } else {
    return new BlockStmt(stmts);
  }
}

export function getSingleton<A>(xs: A[]): A {
  if (xs.length != 1) {
    throw new Error("Expect a singleton collection, but get: " + xs);
  }
  return xs[0];
}


class SpecialVars {
  static spread = new Var("$Spread");
  static typeOf = new Var("$TypeOf");
  static THIS = new Var("this");
  static SUPER = new Var("super");
  static CASE = new Var("$Case");
  static SWITCH = new Var("$Switch");
  static DELETE = new Var("$Delete");
  static ArrayAccess = new Var("$ArrayAccess");
  static YIELD = new Var("$Yield");
  static AWAIT = new Var("$Await");
  static Template = new Var("$Template");

  static ComputedPropertyName = "$ComputedPropertyName";
  static UNKNOWN = "$UNKNOWN";
}

// utilities
export function flatMap<A, B>(xs: any, f: (x: A) => B[]): B[] {
  return xs.reduce((acc: any, x: A) => acc.concat(f(x)), []);
}

export function forNode<T>(node: ts.Node, action: () => T): T {
  try {
    return action();
  } catch (e) {
    console.debug("Error occurred when processing node: " + node.getText());
    throw e;
  }
}

export function getLineNumber(node: ts.Node): number {
  const src = node.getSourceFile();
  let {line} = src.getLineAndCharacterOfPosition(node.getStart());
  return line + 1;
}

export function getSrcSpan(node: ts.Node): SrcSpan {
  const src = node.getSourceFile();
  const start = src.getLineAndCharacterOfPosition(node.getStart());
  const end = src.getLineAndCharacterOfPosition(node.getEnd());
  return [[start.line, start.character], [end.line, end.character]]
}

export function parseFiles(sources: string[], libraryFiles: string[]): [GModule[], string[]] {
  let program = ts.createProgram(libraryFiles, {
    target: ts.ScriptTarget.ES2015,
    module: ts.ModuleKind.CommonJS
  });
  program.getSemanticDiagnostics(undefined, undefined); //call this to store type info into nodes

  const checker = program.getTypeChecker(); // must call this to link source files to nodes

  let warnnings: string[] = [];
  const sFiles: ts.SourceFile[] = sources
    .map(file => mustExist(program.getSourceFile(file),
      "getSourceFile failed for: " + file))
    .filter(sc => {
      const noError = program.getSyntacticDiagnostics(sc).length == 0;
      if (!noError) {
        warnnings.push(`file ${sc.fileName} has syntactic error, skipped.`);
      }
      return noError;
    });
  mustExist(sFiles);

  let parser = new StmtParser(checker);

  return [sFiles.map((src, index) => {
    let stmts: GStmt[] = [];
    src.statements.forEach(s => {
      try {
        let r = parser.parseStmt(s);
        r.forEach(s => stmts.push(s));
      } catch (e) {
        console.error("Parsing failed for file: " + src.fileName);
        throw e;
      }
    });
    return new GModule(sources[index], stmts);
  }), warnnings];
}

function handleError<T>(node: ts.Node, thunk: () => T): T {
  // return thunk();
  try {
    return thunk();
  } catch (e) {
    const line = getLineNumber(node);
    console.log(`Failure occurred at line ${line}: ${node.getText()}`);
    console.log(`Error message: ${e.message}`);
    throw e;
  }
}

function useOrElse<T, G>(v: T | undefined | null, f: (_: T) => G, backup: G): G {
  if (v) {
    return f(v);
  } else {
    return backup;
  }
}
