import {foo as fool} from "./file1";
import {C1 as C, C1} from "./dir/file3";

export var result = fool(5,3);
export function g(){
  return new C();
}