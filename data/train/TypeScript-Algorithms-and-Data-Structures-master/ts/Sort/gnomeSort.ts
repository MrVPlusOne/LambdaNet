function gnomeSort<T>(array: T[], comparator: (a: T, b: T) => boolean = (a, b) => a > b) {
    if (!Array.isArray(array) || array.length < 2) {
        return array;
    }

    let position: number = 0;
    let length: number = array.length;
    let temp: T = null;
    while (position < length) {
        if (position === 0 || !comparator(array[position - 1], array[position])) {
            position++;
        } else {
            temp = array[position - 1];
            array[position - 1] = array[position];
            array[position] = temp;
            position--;
        }
    }
    return array;
}

export default gnomeSort;