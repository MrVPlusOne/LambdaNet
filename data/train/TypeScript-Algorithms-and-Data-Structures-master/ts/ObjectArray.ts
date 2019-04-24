import {IObjectArrayProperty} from "./Interfaces/IObjectArrayProperty";
import {ITypedArray} from "./ITypedArray";
import {Utils} from "./Utils";
export class ObjectArray {
    private columns:ITypedArray[] = [];
    private nameToColumn:{[key: string]: number} = {};
    private freePointer:number = 0;
    private nextFreeList:Float64Array;
    private properties: IObjectArrayProperty[];

    constructor (size:number, properties: IObjectArrayProperty[]) {
        this.properties = properties;
        this.columns = properties.map(property => {
            var TypedArrayConstructor = property.type;
            return new TypedArrayConstructor(size);
        });
        properties.forEach((property, i) => this.nameToColumn[property.name] = i);
        this.nextFreeList = new Float64Array(Utils.range(1, size + 1));
        this.nextFreeList[size - 1] = -1;
    }

    public add(obj:number[]) {
        if(this.freePointer === -1) {
            throw new RangeError('Array size exceeded.');
        }
        var index:number = this.freePointer;
        this.freePointer = this.nextFreeList[this.freePointer];
        this.nextFreeList[index] = -1;
        obj.forEach((value, columnIndex) => this.columns[columnIndex][index] = value);
        return index;
    }

    public remove(index:number) {
        var currentPointer:number = this.freePointer;
        this.freePointer = index;
        this.nextFreeList[this.freePointer] = currentPointer;
    }

    public get(index:number) {
        if (this.nextFreeList[index] !== -1) {
            return null;
        }
        return this.columns.map(column => column[index]);
    }

    public getHash(index:number):any {
        var output:any = {};
        if (this.nextFreeList[index] !== -1) {
            return null;
        }
        this.columns.map((column, columnIndex) => output[this.properties[columnIndex].name] = column[index]);
        return output;
    }
}