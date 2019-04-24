'use strict';

let Benchmark = undefined;
import { runBenchmark } from '../helpers';
import { generateArray } from './random';

let arrays = [
    generateArray(1000, 1, 1000),
    generateArray(2000, 1, 2000),
    generateArray(3000, 1, 3000),
    generateArray(4000, 1, 4000),
    generateArray(5000, 1, 5000)
];

export default function binarySearch(target:number, arr:Array<number>, sorted=false, lo=0, hi=arr.length-1) {

    if(!sorted) {
        arr.sort((a,b) => a - b);
    }
    let mid = lo + (Math.floor((hi - lo) / 2));

    if (target < arr[mid]) {
        return binarySearch(target, arr, true, lo, mid - 1);
    } else if (target > arr[mid]) {
        return binarySearch(target, arr, true, mid + 1, hi);
    } else if (target === arr[mid]) {
        return mid;
    } else {
        return -1;
    }
}

const suite = new Benchmark.Suite('Binary search (recursive implementation)');
for(let i = 0; i < arrays.length; i++) {
    let toFind = arrays[i][Math.floor(arrays[i].length * 0.3)];
    suite.add(`${arrays[i].length}`, () => binarySearch(toFind, arrays[i], true));
}
runBenchmark(suite);