import * as parser from "./parser";
import * as ts from "typescript";
import 'source-map-support/register';
import {GStmt, GModule, mustExist} from "./parser";

function parseFiles(sources: string[], libraryFiles: string[]): GModule[] {
    let program = ts.createProgram(libraryFiles, {
        target: ts.ScriptTarget.ES2018,
        module: ts.ModuleKind.CommonJS
    });
    let checker = program.getTypeChecker();

    let sFiles = sources.map(program.getSourceFile);
    mustExist(sFiles);


    let parsed = sFiles.map(src => {
        let stmts: GStmt[] = [];
        src.statements.forEach(s => {
            let r = parser.parseStmt(s, checker);
            if (!r) {
                throw new Error("failed for: " + s.getFullText(src));
            }
            r.forEach(s => stmts.push(s));
        });
        return new GModule(src.fileName, stmts);
    });

    return parsed;
}

let args = process.argv;
let numSrc = parseInt(args[2]);
let src = args.slice(3, 3 + numSrc);
let files = args.slice(3+numSrc);

let result = parseFiles(src, files);
console.log(JSON.stringify(result));