import * as ts from "typescript";
import {SyntaxKind} from "typescript";


export class GModule {
  constructor(public name: string, public stmts: GStmt[]) {
  }
}

export function mustExist<T>(v: T, msg: string = null): T {
  if (v == null) {
    if (msg) {
      throw new Error("should not be " + v + "! Message: " + msg);
    } else {
      throw new Error("should not be " + v + "!");
    }
  }
  return v
}

// ASTs
type GMark = GType | null;

interface GType {
  category: string;
}

class TVar implements GType {
  public category = "TVar";

  constructor(public name: string) {
    mustExist(name)
  }
}

class AnyType implements GType {
  public category = "AnyType";
  public name: string = "any";

  private constructor() {
  }

  static instance = new AnyType();
}

class FuncType implements GType {
  public category = "FuncType";

  constructor(public fro: GMark[], public to: GMark) {

  }
}

class ObjectType implements GType {
  public category = "ObjectType";

  constructor(public fields: { [k: string]: GMark }) {

  }
}

export const anyType = AnyType.instance;

function parseFuncType(s: string): FuncType {
  var stack: number = 1, i: number;
  var tybuff: string;

  var fr: GType[] = [];

  for (i = 1; stack > 0 && i < s.length;) {
    var j: number, k: number;
    for (j = i; s.charAt(j) != ':'; j++) {
      if (s.charAt(j) == '(') stack++;
      else if (s.charAt(j) == ')') stack--;
    }

    for (k = j + 1; s.charAt(k) != ',' && stack > 0; k++) {
      if (s.charAt(k + 1) == '(') stack++;
      else if (s.charAt(k + 1) == ')') stack--;
    }

    tybuff = s.substring(j + 2, k);

    fr.push(parseMark_s(tybuff.trim()));

    i = k;
  }

  let to: GMark = parseMark_s(s.substring(i + 4).trim());

  return new FuncType(fr, to);

}

function parseObjectType(s: string): ObjectType {
  var stack: number = 1, i: number;
  var argbuff: string, tybuff: string;

  var fields: { [k: string]: GMark } = {};

  for (i = 1; stack > 0 && i < s.length;) {
    var j: number, k: number;
    for (j = i; s.charAt(j) != ':'; j++) {
      if (s.charAt(j) == '{') stack++;
      else if (s.charAt(j) == '}') stack--;
    }

    argbuff = s.substring(i, j).trim();

    for (k = j + 1; s.charAt(k) != ',' && stack > 0; k++) {
      if (s.charAt(k + 1) == '{') stack++;
      else if (s.charAt(k + 1) == '}') stack--;
    }

    tybuff = s.substring(j + 2, k).trim();
    let type: GMark = parseMark_s(tybuff);
    fields[argbuff] = type;

    i = k;
  }

  return new ObjectType(fields);
}

function parseMark_s(s: string): GMark {
  if (s == "any") return anyType;
  else if (s.charAt(0) == '(') return parseFuncType(s);
  else if (s.charAt(0) == '{') return parseObjectType(s);
  else return new TVar(s);
}

export function parseMark(node: ts.TypeNode, checker: ts.TypeChecker): GMark {
  if (!node) return null;

  // let symbol = checker.getTypeFromTypeNode(node)();
  // if(!symbol) throw new Error("unresolved type: " + node.getText());
  let string = checker.typeToString(checker.getTypeFromTypeNode(node));
  //todo: handle other cases
  //todo: deal with type errors (currently, unresolved types are marked as any)
  return parseMark_s(string);
}


class NamedValue<V> {
  constructor(public name: String, public value: V) {
  }
}

interface GExpr {
  category: string
}

class Var implements GExpr {
  category: string = "Var";

  constructor(public name: string) {
    mustExist(name);
  }
}

class Const implements GExpr {
  category: string = "Const";

  constructor(public value: string, public ty: GType) {
    mustExist(value);
  }
}

class FuncCall implements GExpr {
  category: string = "FuncCall";

  constructor(public f: GExpr, public args: GExpr[]) {
  }
}

class ObjLiteral implements GExpr {
  category: string = "ObjLiteral";

  constructor(public fields: NamedValue<GExpr>[]) {
  }
}

class Access implements GExpr {
  category: string = "Access";

  constructor(public expr: GExpr, public field: string) {
    mustExist(field);
  }
}

class IfExpr implements GExpr {
  category: string = "IfExpr";

  constructor(public cond: GExpr, public e1: GExpr, public e2: GExpr) {
  }
}


export interface GStmt {
  category: string
}

class VarDef implements GStmt {
  category: string = "VarDef";

  constructor(public x: string, public mark: GMark,
              public init: GExpr, public isConst: boolean, public modifiers: string[]) {
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

class ImportStmt implements GStmt {
  category: string = "ImportStmt";

  constructor(public text: string) {
    mustExist(text);
  }
}

class ExportStmt implements GStmt {
  category: string = "ExportStmt";

  constructor(public text: string) {
    mustExist(text);
  }
}

class TypeAliasStmt implements GStmt {
  category: string = "TypeAliasStmt";

  constructor(public name: string, public tVars: string[], public type: GType) {
    mustExist(name);
    mustExist(tVars);
    mustExist(type);
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

class FuncDef implements GStmt {
  category: string = "FuncDef";

  constructor(public name: string, public args: NamedValue<GMark>[], public returnType: GMark,
              public body: GStmt, public modifiers: string[], public tyVars: string[]) {
    mustExist(name);
    if ((name == "Constructor") && returnType && (returnType as TVar).name != 'void') {
      throw new Error("Wrong return type for constructor. Got: " + returnType)
    }
  }
}

class ClassDef implements GStmt {
  category: string = "ClassDef";

  constructor(public name: string, public constructor: FuncDef, public vars: Object,
              public funcDefs: FuncDef[], public superType: string, public modifiers: string[],
              public tyVars: string[]) {
  }
}

function tryFullyQualifiedName(node: ts.Node, checker: ts.TypeChecker): string {
  // let symbol = checker.getSymbolAtLocation(node);
  // let name: string = symbol ? checker.getFullyQualifiedName(symbol) : (<any>node)["text"]; fixme: not working
  let name = (<any>node).text;
  mustExist(name);
  return name;
}


export function parseExpr(node: ts.Node, checker: ts.TypeChecker,
                          allocateLambda: (f: ts.FunctionLikeDeclaration) => Var): GExpr {

  function rec(node: ts.Node): GExpr {
    mustExist(node);
    switch (node.kind) {
      case SyntaxKind.Identifier:
        let name = tryFullyQualifiedName(node, checker);
        return new Var(name);
      case SyntaxKind.ThisKeyword:
        return SpecialVars.THIS;
      case SyntaxKind.SuperKeyword:
        return SpecialVars.SUPER;
      case SyntaxKind.CallExpression: {
        let n = (<ts.CallExpression>node);
        let f = rec(n.expression);
        let args = n.arguments.map(rec);
        return new FuncCall(f, args);
      }
      case SyntaxKind.NewExpression: {
        let n = (<ts.NewExpression>node);
        let fName = (<ts.Identifier>n.expression).text + "-NEW";
        let args = n.arguments.map(rec);
        return new FuncCall(new Var(fName), args);
      }
      case SyntaxKind.ObjectLiteralExpression: {
        let n = (<ts.ObjectLiteralExpression>node);
        let fields = n.properties.map((p: ts.ObjectLiteralElementLike) => {
          return parseObjectLiteralElementLike(p);
        });
        return new ObjLiteral(fields);
      }
      case SyntaxKind.PropertyAccessExpression: {
        let n = node as ts.PropertyAccessExpression;
        let lhs = rec(n.expression);
        return new Access(lhs, n.name.text);
      }
      case ts.SyntaxKind.ElementAccessExpression: {
        let n = node as ts.ElementAccessExpression;
        let thing = rec(n.expression);
        let index = rec(n.argumentExpression);
        return new FuncCall(new Access(thing, "access"), [index]);
      }
      case ts.SyntaxKind.ConditionalExpression: {
        let n = node as ts.ConditionalExpression;
        let cond = rec(n.condition);
        let e1 = rec(n.whenTrue);
        let e2 = rec(n.whenFalse);
        return new IfExpr(cond, e1, e2);
      }
      case ts.SyntaxKind.ParenthesizedExpression: {
        let n = node as ts.ParenthesizedExpression;
        return rec(n.expression);
      }

      // constants
      case SyntaxKind.NumericLiteral:
        return constExpr("number");
      case SyntaxKind.StringLiteral:
        return constExpr("string");
      case SyntaxKind.TrueKeyword:
      case SyntaxKind.FalseKeyword:
        return constExpr("bool");
      case SyntaxKind.NullKeyword:
        return new Const("null", anyType);
      case SyntaxKind.ArrayLiteralExpression:
        return constExpr("array"); //todo: might need to distinguish array types

      // operators
      case ts.SyntaxKind.BinaryExpression: {
        let n = node as ts.BinaryExpression;
        let l = rec(n.left);
        let r = rec(n.right);
        let opp = n.operatorToken.kind;

        return new FuncCall(new Access(l, ts.SyntaxKind[opp]), [r]);
      }
      case SyntaxKind.PrefixUnaryExpression:
      case SyntaxKind.PostfixUnaryExpression: {
        let n = <any>node;
        let opName = ts.SyntaxKind[n["operator"]];
        let fixity = (node.kind == SyntaxKind.PrefixUnaryExpression) ? "" : "POST_";
        let arg = rec(n["operand"]);
        return new FuncCall(new Var(fixity + opName), [arg]);
      }
      case SyntaxKind.ArrowFunction:
      case SyntaxKind.FunctionExpression: {
        let n = node as ts.FunctionLikeDeclaration;
        return allocateLambda(n);
      }

      // Special treatments:
      case SyntaxKind.SpreadElement: {
        let n = node as ts.SpreadElement;
        return new FuncCall(SpecialVars.spread, [rec(n.expression)]);
      }
      case SyntaxKind.TypeOfExpression: {
        let n = node as ts.TypeOfExpression;
        return new FuncCall(SpecialVars.typeOf, [rec(n.expression)]);
      }
      case SyntaxKind.TemplateExpression: {
        return constExpr("string");
      }
      case SyntaxKind.DeleteExpression: {
        let n = node as ts.DeleteExpression;
        return new FuncCall(SpecialVars.DELETE, [rec(n.expression)]);
      }

      default:
        throw new Error("Unknown expression category: " + ts.SyntaxKind[node.kind]
          + ". Text: " + node.getText());
    }

    function constExpr(typeName: string) {
      // let v = (<ts.LiteralLikeNode>node).text;
      return new Const("CONST", new TVar(typeName));
    }

    function parseObjectLiteralElementLike(p: ts.ObjectLiteralElementLike): NamedValue<GExpr> {
      let a = (<ts.PropertyAssignment>p);
      return new NamedValue<GExpr>((<ts.StringLiteral>a.name).text, rec(a.initializer));
    }
  }

  return rec(node);
}

export const notDefinedValue = new Const("undefined", anyType);

export class StmtParser {
  public nLambda: [number] = [0];

  parseStmt(node: ts.Node, checker: ts.TypeChecker): GStmt[] {
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
          let name = "$Lambda" + getNLambda[0];
          getNLambda[0] += 1;
          lambdas.push(parseFunction(name, f, []));
          return new Var(name);
        }

        return parseExpr(e, checker, allocateLambda)
      }

      alongWith(...stmts: GStmt[]): StmtsHolder {
        return new StmtsHolder((<GStmt[]>this.lambdaDefs).concat(stmts));
      }

      alongWithMany(stmts: GStmt[]): StmtsHolder {
        return new StmtsHolder((<GStmt[]>this.lambdaDefs).concat(stmts));
      }
    }

    function parseFunction(name: string, n: ts.FunctionLikeDeclaration, modifiers: string[]): FuncDef {
      let retType = (node.kind == SyntaxKind.Constructor) ? new TVar("void") :
        parseMark(n.type, checker);
      let args = n.parameters.map(p => {
        return new NamedValue((<ts.Identifier>p.name).text, parseMark(p.type, checker))
      });

      let body: StmtsHolder;
      try {
        let ep = new ExprProcessor();
        // try to parse the body as a ConciseFunction body
        body = ep.alongWith(new ExprStmt(ep.processExpr((n.body as ts.Expression)), true))
      } catch (_) {
        body = rec(n.body);
      }

      let type_params = n.typeParameters;
      let t_vars: string[];
      if (type_params)
        t_vars = type_params.map(n => n.name.text);
      else
        t_vars = null;

      return new FuncDef(name, args, retType, flattenBlock(body.stmts), modifiers, t_vars);
    }


    function rec(node: ts.Node): StmtsHolder {
      let EP = new ExprProcessor();

      function parseVarDecList(node: ts.VariableDeclarationList, modifiers: string[]): VarDef[] {

        return forNode(node, () => {
          let isConst = (node.flags & ts.NodeFlags.Const) != 0;

          function parseBinding(x: ts.BindingElement | ts.VariableDeclaration): VarDef[] {
            let initExpr = x.initializer ? EP.processExpr(x.initializer) : notDefinedValue;
            let lhs = x.name;
            switch (lhs.kind) {
              case SyntaxKind.Identifier:
                return [new VarDef(
                  (<ts.Identifier>lhs).text,
                  parseMark((<any>x).type, checker),
                  initExpr,
                  isConst,
                  modifiers)];
              case SyntaxKind.ObjectBindingPattern:
                return flatMap((lhs as ts.ObjectBindingPattern).elements, (e: ts.BindingElement) => {
                  return parseBinding(e)
                });
              default:
                throw new Error("Unsupported binding pattern: " + x.getText())
            }
          }

          let dec = node.declarations;
          return flatMap(dec, (x: ts.VariableDeclaration) => parseBinding(x));
        });
      }

      switch (node.kind) {
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
          return EP.alongWith(new ExprStmt(EP.processExpr(n.expression), false));
        }
        case SyntaxKind.ReturnStatement: {
          let n = <ts.ReturnStatement>node;
          return EP.alongWith(new ExprStmt(EP.processExpr(n.expression), true));
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
          if (n.elseStatement == undefined) otherwise = [new BlockStmt([])];
          else otherwise = rec(n.elseStatement).stmts;
          return EP.alongWith(new IfStmt(cond, then, flattenBlock(otherwise)));
        }
        case SyntaxKind.WhileStatement: {
          let n = node as ts.WhileStatement;
          let cond = EP.processExpr(n.expression);
          let body = flattenBlock(rec(n.statement).stmts);
          return EP.alongWith(new WhileStmt(cond, body));
        }
        case SyntaxKind.Block: {
          let n = node as ts.Block;
          let stmts = flatMap(n.statements, (x: ts.Node) => rec(x).stmts);
          return EP.alongWith(new BlockStmt(stmts));
        }
        case ts.SyntaxKind.ForStatement: {
          let n = node as ts.ForStatement;
          let cond = n.condition;
          let init = n.initializer;
          let outerBlock = new BlockStmt([]);

          if (init && ts.isVariableDeclarationList(init)) {
            outerBlock.stmts = parseVarDecList(init, []);
          } else if (init) {
            outerBlock.stmts.push(new ExprStmt(EP.processExpr(init as ts.Expression), false));
          }

          let incr = new ExprStmt(EP.processExpr(n.incrementor), false);
          let bodyStmts: GStmt[] = rec(n.statement).stmts.concat([incr]);

          outerBlock.stmts.push(new WhileStmt(
            EP.processExpr(cond),
            flattenBlock(bodyStmts)
          ));
          return EP.alongWith(outerBlock);
        }
        case SyntaxKind.FunctionDeclaration:
        case SyntaxKind.MethodDeclaration:
        case SyntaxKind.Constructor: {
          let name = (node.kind == SyntaxKind.Constructor) ? "Constructor" :
            tryFullyQualifiedName((node as any).name, checker);
          let n = <ts.FunctionLikeDeclaration>node;
          return EP.alongWith(parseFunction(name, n, parseModifiers(n.modifiers)));
        }

        case SyntaxKind.ClassDeclaration: {
          let n = node as ts.ClassDeclaration;

          let name = tryFullyQualifiedName(n.name, checker);

          let superType: string | null = null;
          if (n.heritageClauses != undefined) {
            let clauses = n.heritageClauses;
            for (const c of clauses) {
              if (c.token == ts.SyntaxKind.ExtendsKeyword) {
                superType = c.types[0].expression.getText(); //todo: handle more cases
                // superType = mustExist((c.types[0].expression as any)["name"]) as string;
              }
            }
          }

          let vars: NamedValue<GMark>[] = [];
          let funcDefs: FuncDef[] = [];
          let constructor: FuncDef | null = null;

          for (const v of n.members) {
            if (ts.isPropertyDeclaration(v)) {
              let v1 = v as ts.PropertyDeclaration;
              vars.push(new NamedValue(getPropertyName(v1.name), parseMark(v1.type, checker)));
            } else if (ts.isMethodDeclaration(v)) {
              funcDefs.push(getSingleton(rec(v).stmts) as FuncDef)
            } else if (ts.isConstructorDeclaration(v)) {
              constructor = getSingleton(rec(v).stmts) as FuncDef;
            } else {
              throw new Error("Unknown statements in class definitions: " + v);
            }
          }


          let type_params = n.typeParameters;
          let t_vars: string[];

          if (type_params)
            t_vars = type_params.map(x => x.name.text);
          else
            t_vars = null;

          return EP.alongWith(new ClassDef(name, constructor, vars, funcDefs,
            superType, parseModifiers(n.modifiers), t_vars));
        }
        case SyntaxKind.SwitchStatement: {
          let n = node as ts.SwitchStatement;

          let switchCall = new FuncCall(SpecialVars.SWITCH, [EP.processExpr(n.expression)]);

          let clauses = flatMap(
            n.caseBlock.clauses,
            (c: ts.CaseOrDefaultClause) => {
              let body = flatMap(c.statements, (s: ts.Statement) => rec(s).stmts);
              switch (c.kind) {
                case SyntaxKind.CaseClause:
                  let f = new FuncCall(SpecialVars.CASE, [EP.processExpr((c as ts.CaseClause).expression)]);
                  let all = [new ExprStmt(f, false) as GStmt].concat(body);
                  return EP.alongWithMany(all).stmts;
                case SyntaxKind.DefaultClause:
                  return EP.alongWithMany(body).stmts;
              }
            });
          return EP.alongWithMany([new ExprStmt(switchCall, false) as GStmt].concat(clauses));
        }

        case SyntaxKind.ImportDeclaration:
          return EP.alongWith(new ImportStmt(node.getText()));
        case SyntaxKind.ExportDeclaration:
          return EP.alongWith(new ExportStmt(node.getText()));
        case SyntaxKind.EnumDeclaration: {
          let n = node as ts.EnumDeclaration;
          let vars = n.members.map(member => {
            let vName = member.name.getText();
            return new NamedValue(vName, new Const("ENUM", new TVar("number")));
          });
          let rhs = new ObjLiteral(vars);

          return EP.alongWith(new VarDef(n.name.text, null, rhs,
            true, parseModifiers(n.modifiers)));
        }
        case SyntaxKind.TypeAliasDeclaration: {
          let n = node as ts.TypeAliasDeclaration;
          let tVars = n.typeParameters.map(p => p.name.text);
          return EP.alongWith(new TypeAliasStmt(n.name.text, tVars, parseMark(n.type, checker)));
        }

        // ignored statements:
        case SyntaxKind.BreakStatement:
          return EP.alongWith(new CommentStmt("break;"));
        //todo: support the followings

        default:
          throw new Error("Unknown stmt category: " + ts.SyntaxKind[node.kind]);
      }
    }

    function getPropertyName(name: ts.PropertyName): string {
      return mustExist(name.getText());
    }

    return rec(node).stmts;
  }
}

function parseModifiers(modifiersNode: ts.ModifiersArray): string[] {
  let modifiers: string[] = [];
  if (modifiersNode) {
    modifiersNode.forEach(m => {
      switch (m.kind) {
        case SyntaxKind.ExportKeyword:
          modifiers.push("export");
          break;
        case SyntaxKind.DefaultKeyword:
          modifiers.push("default");
          break;
        case SyntaxKind.ConstKeyword:
          modifiers.push("const");
          break;
        default:
      }
    })
  }
  return modifiers;
}

export function flattenBlock(stmts: GStmt[]): GStmt {
  if (stmts.length == 1) return stmts[0];
  else return new BlockStmt(stmts);
}

export function getSingleton<A>(xs: A[]): A {
  if (xs.length != 1)
    throw new Error("Expect a singleton collection, but get: " + xs);
  return xs[0];
}


class SpecialVars {
  static spread = new Var("$Spread");
  static typeOf = new Var("$TypeOf");
  static THIS = new Var("this");
  static SUPER = new Var("super");
  static CASE = new Var("$Case");
  static SWITCH = new Var("$Switch");
  static DELETE = new Var("$Delete")
}

// utilities
export function flatMap<A, B>(xs: any, f: (x: A) => B[]): B[] {
  return xs.reduce((acc: any, x: A) => acc.concat(f(x)), []);
}

export function forNode<T>(node: ts.Node, action: () => T): T {
  try {
    return action()
  } catch (e) {
    console.debug("Error occurred when processing node: " + node.getText())
    throw e
  }
}

export function parseFiles(sources: string[], libraryFiles: string[]): GModule[] {
  let program = ts.createProgram(libraryFiles, {
    target: ts.ScriptTarget.ES2018,
    module: ts.ModuleKind.CommonJS
  });
  let checker = program.getTypeChecker();

  let sFiles = sources.map(file => mustExist(program.getSourceFile(file),
    "getSourceFile failed for: " + file));
  mustExist(sFiles);

  let parser = new StmtParser();

  return sFiles.map((src, index) => {
    let stmts: GStmt[] = [];
    src.statements.forEach(s => {
      try {
        let r = parser.parseStmt(s, checker);
        r.forEach(s => stmts.push(s));
      } catch (e) {
        console.debug("Parsing failed for file: " + src.fileName);
        console.debug("Failure occurred at line: " + s.getText());
        throw e
      }

    });
    return new GModule(sources[index], stmts);
  });
}