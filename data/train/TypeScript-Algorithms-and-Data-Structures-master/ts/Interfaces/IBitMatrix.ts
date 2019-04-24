export interface IBitMatrix {
    clone(): IBitMatrix;
    count(): number;
    get(rowIndex: number, colIndex: number): boolean;
    getIndexes(resultPerColumn?: boolean): number[][];
    getRowIndexes(row: number): number[];
    getColIndexes(column: number): number[];
    reset(): IBitMatrix;
    resize(newRowCount: number, newColCount: number): IBitMatrix;
    size(): number[];
    set(rowIndex: number, colIndex: number, value: boolean): IBitMatrix;
    setBuffer(newBuffer: Uint32Array): IBitMatrix;
    spliceColumn(startIndex: number, deleteCount: number): IBitMatrix;
    spliceRow(startIndex: number, deleteCount: number): IBitMatrix;
}