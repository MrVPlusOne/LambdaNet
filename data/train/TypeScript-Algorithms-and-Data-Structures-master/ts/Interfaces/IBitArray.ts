export interface IBitArray {
    count(): number;
    get(index: number): boolean;
    getIndexes(): number[];
    reset(): IBitArray;
    resize(newSize: number): IBitArray;
    size(): number;
    set(index: number, value: boolean): IBitArray;
}