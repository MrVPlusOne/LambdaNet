interface ITypedArray {
    BYTES_PER_ELEMENT: number;
    buffer: ArrayBuffer;
    byteLength: number;
    byteOffset: number;
    copyWithin(target: number, start: number, end?: number): ITypedArray;
    every(callbackfn: (value: number, index: number, array: ITypedArray) => boolean, thisArg?: any): boolean;
    fill(value: number, start?: number, end?: number): ITypedArray;
    filter(callbackfn: (value: number, index: number, array: ITypedArray) => boolean, thisArg?: any): ITypedArray;
    find(predicate: (value: number, index: number, obj: ITypedArray) => boolean, thisArg?: any): number;
    findIndex(predicate: (value: number) => boolean, thisArg?: any): number;
    forEach(callbackfn: (value: number, index: number, array: ITypedArray) => void, thisArg?: any): void;
    indexOf(searchElement: number, fromIndex?: number): number;
    join(separator?: string): string;
    lastIndexOf(searchElement: number, fromIndex?: number): number;
    length: number;
    map(callbackfn: (value: number, index: number, array: ITypedArray) => number, thisArg?: any): ITypedArray;
    reduce(callbackfn: (previousValue: number, currentValue: number, currentIndex: number, array: ITypedArray) => number, initialValue?: number): number;
    reduce<U>(callbackfn: (previousValue: U, currentValue: number, currentIndex: number, array: ITypedArray) => U, initialValue: U): U;
    reduceRight(callbackfn: (previousValue: number, currentValue: number, currentIndex: number, array: ITypedArray) => number, initialValue?: number): number;
    reduceRight<U>(callbackfn: (previousValue: U, currentValue: number, currentIndex: number, array: ITypedArray) => U, initialValue: U): U;
    reverse(): ITypedArray;
    set(array: ArrayLike<number>, offset?: number): void;
    slice(start?: number, end?: number): ITypedArray;
    some(callbackfn: (value: number, index: number, array: ITypedArray) => boolean, thisArg?: any): boolean;
    sort(compareFn?: (a: number, b: number) => number): ITypedArray;
    subarray(begin: number, end?: number): ITypedArray;
    toLocaleString(): string;
    toString(): string;

    [index: number]: number;
}

export  {ITypedArray};