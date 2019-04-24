import SortingComparator from "../CustomTypes/SortingComparator";
import {Utils} from "../Utils";
import insertionSort from "./insertionSort";
function quickSort<T>(array: T[], comparator: SortingComparator<T> = Utils.gt, leftIndex: number = 0, rightIndex: number = -1): T[] {
    if (!Array.isArray(array) || array.length < 2) {
        return array;
    }

    if (rightIndex === -1) {
        rightIndex = array.length - 1;
    }
    if (leftIndex >= rightIndex) {
        return array;
    }

    if (leftIndex > rightIndex - 10) {
        return insertionSort(array, comparator, leftIndex, rightIndex + 1);
    }

    const pivot: number = partition(array, comparator, leftIndex, rightIndex);
    quickSort(array, comparator, leftIndex, pivot - 1);
    return quickSort(array, comparator, pivot + 1, rightIndex);
}

function partition <T>(array: T[], comparator: SortingComparator<T>, leftIndex: number, rightIndex: number) {
    const pivotIndex: number = leftIndex + Math.floor(Math.random() * (rightIndex - leftIndex));
    Utils.swapValues(array, rightIndex, pivotIndex);
    const pivot: T = array[rightIndex];
    let border: number = leftIndex - 1;
    for (let j = leftIndex; j < rightIndex; j++) {
        if (!comparator(array[j], pivot)) {
            border++;
            Utils.swapValues(array, j, border);
        }
    }
    Utils.swapValues(array, border + 1, rightIndex);
    return border + 1;
}

export default quickSort;