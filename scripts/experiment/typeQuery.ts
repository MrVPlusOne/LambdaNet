import * as ts from "typescript";


function main(){
  let program = ts.createProgram(["test1.ts"],{
    target: ts.ScriptTarget.ES2018,
    module: ts.ModuleKind.CommonJS
  });

  let src = program.getSourceFile("test1.ts");

  let diag = program.getSemanticDiagnostics(null, null);
  // diag.forEach(console.log);

  console.log(src["locals"]);

}

main();
console.log("Finished.");
