let Benchmark = undefined;
import { generateArray, runBenchmark } from '../helpers';

let arrays = [
    generateArray(100),
    generateArray(200),
    generateArray(300),
    generateArray(400),
    generateArray(500),
    generateArray(600),
    generateArray(700),
    generateArray(800),
    generateArray(900),
    generateArray(1000)
];
//O(n)
function search(numbers:Array<number>, toFind:number): number {
    for(let i of numbers) {
        if (toFind === i) {
            return i;
        }
    }
    return -1;
}

const suite = new Benchmark.Suite('O(n) will perform linearly in proportion to the size of the data set:');
for(let i = 0; i < arrays.length; i++) {
    suite.add(`${arrays[i].length}`, () => search(arrays[i], arrays[i][arrays[i].length-1]));
}
runBenchmark(suite);