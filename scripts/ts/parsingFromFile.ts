import 'source-map-support/register';
import {parseFiles} from "./parsing";
import fs = require("fs");

import * as yargs from 'yargs'
import {Argv} from "yargs";

let argv = yargs
  .command('print', "Parse and print the resulted JSON.", (yargs: Argv) => {
    return yargs
      .option('src', {
        describe: "A list of source files to parse and output",
      })
      .option('lib', {
        // alias: 'v',
        default: [],
      })
      .option('out',{
        describe: "If set, write the result json into the specified file instead of printing it.",
        default: null
      })
  })
  .array("src")
  .array("lib")
  .argv;

let result = parseFiles(argv.src as string[], argv.lib as string[]);
let json = JSON.stringify(result);
if(argv.out){
  fs.writeFileSync(argv.out as string, json)
}else{
  console.log(json);
}
