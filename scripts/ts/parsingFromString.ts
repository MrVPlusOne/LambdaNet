import * as parsing from "./parsing";
import * as ts from "typescript";
import 'source-map-support/register';
import {flattenBlock, GStmt} from "./parsing";

let content = process.argv[2];

let source = ts.createSourceFile("temp.ts", content,
  ts.ScriptTarget.ES2018, true, ts.ScriptKind.TS);
let program = ts.createProgram([], {
  target: ts.ScriptTarget.ES2018,
  module: ts.ModuleKind.CommonJS
});

const checker = program.getTypeChecker();
program.getSemanticDiagnostics();

let out: GStmt[] = [];

let parser = new parsing.StmtParser(checker);

source.statements.forEach(s => {
  let r = parser.parseStmt(s);
  if (!r) {
    throw new Error("failed for: " + s.getFullText(source));
  }
  out.push(flattenBlock(r));
});


console.log(JSON.stringify(out));
