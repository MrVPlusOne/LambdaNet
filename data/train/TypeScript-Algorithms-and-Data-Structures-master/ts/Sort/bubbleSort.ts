function bubbleSort<T>(array: T[], comparator: (a: T, b: T) => boolean = (a, b) => a > b) {
    if (!Array.isArray(array) || array.length < 2) {
        return array;
    }
    let isNotSorted: boolean = true;
    let size: number = array.length - 1;
    let temp: T;
    while (isNotSorted) {
        isNotSorted = false;
        for (let i = 0; i < size; i++) {
            if (comparator(array[i], array[i + 1])) {
                temp = array[i + 1];
                array[i + 1] = array[i];
                array[i] = temp;
                isNotSorted = true;
            }
        }
    }
    return array;
}

export default bubbleSort;