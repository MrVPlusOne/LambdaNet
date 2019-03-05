import * as parsing from "./parsing";
import * as ts from "typescript";
import 'source-map-support/register';
import {GStmt, GModule, mustExist} from "./parsing";

function parseFiles(sources: string[], libraryFiles: string[]): GModule[] {
  let program = ts.createProgram(libraryFiles, {
    target: ts.ScriptTarget.ES2018,
    module: ts.ModuleKind.CommonJS
  });
  let checker = program.getTypeChecker();

  let sFiles = sources.map(file => mustExist(program.getSourceFile(file),
    "getSourceFile failed for: " + file));
  mustExist(sFiles);

  let parser = new parsing.StmtParser();

  let parsed = sFiles.map(src => {
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
    return new GModule(src.fileName, stmts);
  });

  return parsed;
}

let args = process.argv;
let numSrc = parseInt(args[2]);
let src = args.slice(3, 3 + numSrc);
let files = args.slice(3 + numSrc);

let result = parseFiles(src, files);
console.log(JSON.stringify(result));