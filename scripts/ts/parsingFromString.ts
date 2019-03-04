import * as parsing from "./parsing";
import * as ts from "typescript";
import 'source-map-support/register';
import {flattenBlock, GStmt} from "./parsing";

let content = process.argv[2];

let source = ts.createSourceFile("temp.ts",content,ts.ScriptTarget.ES5, true,ts.ScriptKind.TS);
let program = ts.createProgram([],{});
let checker = program.getTypeChecker();

let out: GStmt[] = [];

let parser = new parsing.StmtParser();

source.statements.forEach(s => {
    let r = parser.parseStmt(s, checker);
    if(!r){
        throw new Error("failed for: " + s.getFullText(source));
    }
    out.push(flattenBlock(r));
});


console.log(JSON.stringify(out));
