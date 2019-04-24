import {ITypedArray} from "../ITypedArray";
export interface IObjectArrayProperty {
    name: string;
    type: new (size:number) => ITypedArray;
}