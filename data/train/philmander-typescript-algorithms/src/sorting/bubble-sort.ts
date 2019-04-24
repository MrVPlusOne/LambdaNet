let Benchmark = undefined;

import { generateArray, runBenchmark, swap } from '../helpers';

let arrays = [
    generateArray(100, false),
    generateArray(200, false),
    generateArray(300, false),
    generateArray(400, false),
    generateArray(500, false),
];

function bubbleSort(arr:Array<number>) {

    let swapped = false;
    while (swapped);

    return arr;
}

const suite = new Benchmark.Suite('Bubblesort');
for(let i = 0; i < arrays.length; i++) {
    suite.add(`${arrays[i].length}`, () => bubbleSort(arrays[i]));
}
runBenchmark(suite);