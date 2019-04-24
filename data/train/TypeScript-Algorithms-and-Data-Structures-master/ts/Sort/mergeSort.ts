function mergeSort (array: number[], reverse = false, startIndex?: number, endIndex?: number) {
    if (!Array.isArray(array)) {
        return array;
    }
    startIndex = startIndex === void 0 ? 0 : startIndex;
    endIndex = endIndex === void 0 ? array.length : endIndex;
    if (array.length < 2 || endIndex - startIndex < 2) {
        return array;
    }
    const middleIndex: number = startIndex + Math.floor((endIndex - startIndex) / 2);

    mergeSort(array, reverse, startIndex, middleIndex);
    mergeSort(array, reverse, middleIndex, endIndex);
    merge(array, reverse, startIndex, middleIndex, endIndex);
    return array;
}

function merge(array: number[], reverse: boolean, startIndex: number, middleIndex: number, endIndex: number) {
    const left: number[] = array.slice(startIndex, middleIndex);
    const right: number[] = array.slice(middleIndex, endIndex);
    left.push(reverse ? -Infinity : Infinity);
    right.push(reverse ? -Infinity : Infinity);
    let i: number = 0;
    let j: number = 0;
    for (let k: number = startIndex; k < endIndex; k++) {
        if ((reverse && left[i] >= right[j] ) || (!reverse && left[i] <= right[j])) {
            array[k] = left[i];
            i++;
        } else {
            array[k] = right[j];
            j++;
        }
    }
}

export default mergeSort;