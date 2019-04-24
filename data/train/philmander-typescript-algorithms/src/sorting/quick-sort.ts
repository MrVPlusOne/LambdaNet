
let Benchmark = undefined;
import { generateArray, runBenchmark, swap } from '../helpers';

let arrays = [
    generateArray(100, false),
    generateArray(200, false),
    generateArray(300, false),
    generateArray(400, false),
    generateArray(500, false),
];

function quicksort(arr:Array<number>, lo = 0, hi = arr.length - 1) {

    if(lo < hi) {
        const p = partition(arr, lo, hi);
        quicksort(arr, lo, p -1);
        quicksort(arr, p + 1, hi);
    }
}

function partition(arr:Array<number>, lo:number, hi:number):number {
    const pivot = arr[hi];
    let i = lo;
    for(let j = lo; j < hi; j++) {
        if(arr[j] <= pivot) {
            swap(arr, i, j);
            i++;
        }
    }
    swap(arr, i, hi);
    return i
}

const suite = new Benchmark.Suite('Quicksort');
for(let i = 0; i < arrays.length; i++) {
    suite.add(`${arrays[i].length}`, () => quicksort(arrays[i]));
}
runBenchmark(suite);
