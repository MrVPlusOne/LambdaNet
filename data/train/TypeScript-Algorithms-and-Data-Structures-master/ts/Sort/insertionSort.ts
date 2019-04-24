import SortingComparator from "../CustomTypes/SortingComparator";
import {Utils} from "../Utils";
function insertionSort<T> (array: T[], comparator: SortingComparator<T> = Utils.gt, startIndex: number = 0, endIndex: number = -1) {
    if (!Array.isArray(array)) {
        return array;
    }

    if (endIndex === -1) {
        endIndex = array.length;
    }

    if ((endIndex - startIndex) < 2) {
        return array;
    }
    for (let j = startIndex + 1, length = endIndex; j < length; j++) {
        let key = array[j];
        let index = j - 1;
        while (index >= startIndex && comparator(array[index], key) ) {
            array[index + 1] = array[index];
            index--;
        }
        array[index + 1] = key;
    }

    return array;
}

export default insertionSort;