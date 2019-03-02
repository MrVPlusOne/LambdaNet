import * as parser from "../parser";
import * as ts from "typescript";
import 'source-map-support/register';
import {GStmt} from "../parser";

let content = `
3;
"abc";
true;
false;
null;
[1,2,3];
f(a);
new A(1,2);
`.trim();

let source = ts.createSourceFile("test.ts",content,ts.ScriptTarget.ES5, true,ts.ScriptKind.TS);
let program = ts.createProgram([],{});

let checker = program.getTypeChecker();
for(let s of source.statements){
    if(!parser.parseStmt(s, checker)){
        throw new Error("failed for: " + s);
    }
}

let stmts = `
let o = {a: 1, b:{c:true}};
`.trim();

let source2 = ts.createSourceFile("test.ts",stmts,ts.ScriptTarget.ES5, true,ts.ScriptKind.TS);
let program2 = ts.createProgram([],{});
let checker2 = program2.getTypeChecker();

for(let s of source2.statements){
    console.log(JSON.stringify(parser.parseStmt(s, checker2)));
}
