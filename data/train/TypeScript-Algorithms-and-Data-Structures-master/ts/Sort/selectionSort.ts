import {Utils} from "../Utils";
import SortingComparator from "../CustomTypes/SortingComparator";
function selectionSort<T>(array: T[], comparator: SortingComparator<T> = Utils.gt) {
    if (!Array.isArray(array) || array.length < 2) {
        return array;
    }

    let position: number = 0;
    const arraySize: number = array.length;
    while (position < arraySize) {
        const lowestIndex: number = Utils.minIndex(array, position, arraySize, comparator);
        Utils.swapValues(array, position, lowestIndex);
        position++;
    }

    return array;
}

export default selectionSort;