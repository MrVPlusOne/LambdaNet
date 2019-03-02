import * as parser from "../parser";
import * as ts from "typescript";
import 'source-map-support/register';
import {GStmt} from "../parser";

let content = process.argv[2];

let source = ts.createSourceFile("temp.ts",content,ts.ScriptTarget.ES5, true,ts.ScriptKind.TS);
let program = ts.createProgram([],{});
let checker = program.getTypeChecker();

let out: GStmt[] = [];

source.statements.forEach(s => {
    let r = parser.parseStmt(s, checker);
    if(!r){
        throw new Error("failed for: " + s.getFullText(source));
    }
    r.forEach(s => out.push(s));
});


console.log(JSON.stringify(out));
