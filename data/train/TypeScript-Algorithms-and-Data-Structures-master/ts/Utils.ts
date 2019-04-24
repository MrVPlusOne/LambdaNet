import SortingComparator from "./CustomTypes/SortingComparator";
export class Utils {
    public static range(start: number, end: number, step: number = 1): number[] {
        let i = start;
        let output: number[] = [];
        while (i < end) {
            output.push(i);
            i = i + step;
        }

        return output;
    }

    public static swapValues <T>(array: T[], from: number, to: number) {
        let temp = array[from];
        array[from] = array[to];
        array[to] = temp;
    }

    public static lt <T>(a: T, b: T): boolean {
        return a < b
    }

    public static gt <T>(a: T, b: T): boolean {
        return a > b
    }

    public static minIndex <T>(array: T[], from: number = 0, to: number = array.length, comparator: SortingComparator<T> = Utils.gt) {
        const arraySize: number = to;
        let lowestIndex: number = from;
        for (let i = from; i < arraySize; i++) {
            if (comparator(array[lowestIndex], array[i])) {
                lowestIndex = i;
            }
        }
        return lowestIndex;
    }

    public static findIndexBy <T>(array: T[], comparator: (el: T, index?: number, array?: T[]) => boolean) {
        let lowestIndex: number = -1;
        array.some((entry, index) => {
            if (comparator(entry, index, array)) {
                lowestIndex = index;
                return true;
            }
            return false;
        });
        return lowestIndex;
    }
}