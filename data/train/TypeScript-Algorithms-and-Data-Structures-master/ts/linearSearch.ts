function linearSearch (array:number[], value:number) {
    let index:number = null;
    let counter = 0;
    const arraySize = array.length;
    while (counter < arraySize && index === null) {
        if (value === array[counter]) {
            index = counter;
        }
        counter++;
    }
    return index;
}

export default linearSearch;