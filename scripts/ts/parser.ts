import {readFileSync} from "fs";
import * as ts from "typescript";
import {SyntaxKind} from "typescript";

/** The output format for the Scala side to parse */
class JNode {
    constructor(public nodeKind: string, public children: JNode[]) {
    }
}

function assertNotNull(v: any, msg: string = null) {
    if (v == null) {
        if(msg){
            throw new Error("should not be null! Message: " + msg);
        }else{
            throw new Error("should not be null!");
        }
    }
}

// ASTs
interface GType{
    category: string;
}

class TVar implements GType{
    public category = "TVar";
    constructor(public name: string){
        assertNotNull(name)
    }
}

class AnyType implements GType{
    public category = "AnyType";
    public name: string = "any";
    private constructor(){}
    static instance = new AnyType();
}

export const anyType = AnyType.instance;

export function parseType(node: ts.TypeNode, checker: ts.TypeChecker): GType {
    //todo: handle other cases
    let string = checker.typeToString(checker.getTypeFromTypeNode(node));
    return new TVar(string);
}


interface NamedExpr{
    name: String;
    expr: GExpr;
}
/*
 *  e :=                         ([[GExpr]])
 *     | x                       ([[Var]])
 *     | c                       ([[Const]])
 *     | e(e,...,e)              ([[FuncCall]])
 *     | e[t]                    ([[Cast]])
 *     | { l: e, ..., l: e }     ([[Constructor]])
 *     | e.l                     ([[Access]])
 *     | if[α] e then e else e   ([[IfExpr]])
 *
 *  where x, l are [[Symbol]],
 *        t is [[GType]],
 *        α is [[GTMark]]
 */
interface GExpr {
    category: string
}

class Var implements GExpr {
    category: string = "Var";

    constructor(public name: string) {
        assertNotNull(name);
    }
}

class Const implements GExpr {
    category: string = "Const";

    constructor(public value: string, public ty: GType) {
        assertNotNull(value);
    }
}

class FuncCall implements GExpr {
    category: string = "FuncCall";

    constructor(public f: GExpr, public args: GExpr[]) {
    }
}

class ObjLiteral implements GExpr{
    category: string = "ObjLiteral";
    constructor(public fields: NamedExpr[]) {
    }
}

class Access implements GExpr {
    category: string = "Access";

    constructor(public expr: GExpr, public field: string) {
        assertNotNull(field);
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

    constructor(public x: string, public type: GType,
                public init: GExpr, public isConst: boolean) {
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

class BlockStmt implements GStmt {
    category: string = "BlockStmt";

    constructor(public stmts: GStmt[]) {
    }
}

class FuncDef implements GStmt {
    category: string = "FuncDef";

    constructor(public name: string, public args: [string, string][], public returnType: string,
                public body: GStmt) {
    }
}

class ClassDef implements GStmt {
    category: string = "ClassDef";

    constructor(public name: string, public constructor: FuncDef, public vars: Object,
                public funcDefs: FuncDef[], public superType: string = null) {
    }
}

function tryFullyQualifiedName(node: ts.Node, checker: ts.TypeChecker): string {
    let symbol = checker.getSymbolAtLocation(node);
    let name: string = symbol ? checker.getFullyQualifiedName(symbol) : (<any>node)["text"];
    return name;
}


export function parseExpr(node: ts.Node, checker: ts.TypeChecker): GExpr {

    function rec(node: ts.Node): GExpr{
        /*
     *  e :=                         ([[GExpr]])
     *     | x                       ([[Var]])
     *     | c                       ([[Const]])
     *     | e(e,...,e)              ([[FuncCall]])
     *     | e[t]                    ([[Cast]])
     *     | { l: e, ..., l: e }     ([[ObjLiteral]])
     *     | e.l                     ([[Access]])
     *     | if[α] e then e else e   ([[IfExpr]])
     *
     *  where x, l are [[Symbol]],
     *        t is [[GType]],
     *        α is [[GTMark]]
     */
        switch (node.kind){
            case SyntaxKind.Identifier:
                let name = tryFullyQualifiedName(node, checker);
                return new Var(name);
            case SyntaxKind.CallExpression: {
                let n = (<ts.CallExpression>node);
                let f = rec(n.expression);
                let args = n.arguments.map(rec);
                return new FuncCall(f, args);
            }
            case SyntaxKind.NewExpression: {
                let n = (<ts.NewExpression>node);
                let fName = "NEW-" + (<ts.Identifier>n.expression).text;
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
            case ts.SyntaxKind.ParenthesizedExpression: {
                let n = node as ts.ParenthesizedExpression;
                return rec(n.expression);
            }

            // constants
            case SyntaxKind.NumericLiteral:
                return constExpr("number");
            case SyntaxKind.StringLiteral:
                return constExpr("string");
            case SyntaxKind.TrueKeyword: case SyntaxKind.FalseKeyword:
                return constExpr("bool");
            case SyntaxKind.NullKeyword:
                return new Const("null", anyType);
            case SyntaxKind.ArrayLiteralExpression:
                return constExpr("array"); //todo: might need to distinguish array types

            // miscellaneous
            case ts.SyntaxKind.BinaryExpression: {
                let n = node as ts.BinaryExpression;
                let l = rec(n.left);
                let r = rec(n.right);
                let opp = n.operatorToken.kind;

                // if (opp == ts.SyntaxKind.FirstAssignment) {
                //     return [new AssignStmt(l, r)]
                // } fixme
                return new FuncCall(new Access(l, ts.SyntaxKind[opp]), [r]);
            }
            case ts.SyntaxKind.ConditionalExpression: {
                let n = node as ts.ConditionalExpression;
                let cond = parseExpr(n.condition, checker);
                let e1 = parseExpr(n.whenTrue, checker);
                let e2 = parseExpr(n.whenFalse, checker);
                return new IfExpr(cond, e1, e2);
            }
            case ts.SyntaxKind.PrefixUnaryExpression:
            case ts.SyntaxKind.PostfixUnaryExpression:
                let n = <any>node;
                let op = ts.SyntaxKind[n["operator"]];
                let arg = parseExpr(n["operand"], checker);
                return new FuncCall(new Var(op), [arg]);

            default:
                throw new Error("Unknown category: " + ts.SyntaxKind[node.kind]);
        }

        function constExpr(typeName: string){
            // let v = (<ts.LiteralLikeNode>node).text;
            return new Const("CONST", new TVar(typeName));
        }

        function parseObjectLiteralElementLike(p: ts.ObjectLiteralElementLike): NamedExpr {
            let a = (<ts.PropertyAssignment>p);
            return {name: (<ts.StringLiteral>a.name).text, expr: rec(a.initializer)};
        }
    }

    return rec(node);
}

export function parseStmt(node: ts.Node, checker: ts.TypeChecker): GStmt[] {
    function rec(node: ts.Node): GStmt[] {
        switch (node.kind) {
            case SyntaxKind.ExpressionStatement: {
                let n = <ts.ExpressionStatement>node;
                if(n.expression.kind == SyntaxKind.BinaryExpression){
                    let e = n.expression as ts.BinaryExpression;
                    if(e.operatorToken.kind == ts.SyntaxKind.FirstAssignment){
                        let l = parseExpr(e.left, checker);
                        let r = parseExpr(e.right, checker);
                        return [new AssignStmt(l, r)]
                    }
                }
                return [new ExprStmt(parseExpr(n.expression, checker), false)];
            }
            case SyntaxKind.ReturnStatement: {
                let n = <ts.ReturnStatement>node;
                return [new ExprStmt(parseExpr(n.expression, checker), true)];
            }
            case SyntaxKind.VariableStatement: {
                let list = (node as ts.VariableStatement).declarationList;
                return parseVarDecList(list);
            }
        }
    }

    function parseVarDecList(node: ts.VariableDeclarationList): VarDef[] {
        let isConst = (node.flags & ts.NodeFlags.Const) != 0;

        let dec = node.declarations;

        return dec.map(x => new VarDef(
            (<ts.Identifier>x.name).text,
            parseType(x.type, checker),
            parseExpr(x.initializer, checker),
            isConst));
    }

    return rec(node);
}

export function parseFile(src: string, envFiles: string[]): GExpr {
    let program = ts.createProgram(envFiles, {
        target: ts.ScriptTarget.ES2018,
        module: ts.ModuleKind.CommonJS
    });

    let checker = program.getTypeChecker();
    let source_files = program.getSourceFiles().filter(f => f.fileName == src && !f.isDeclarationFile);
    return parseExpr(source_files[0], checker);
}

