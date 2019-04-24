let Benchmark = undefined;
import { generateArray, runBenchmark } from '../helpers';

let arrays = [
    generateArray(10),
    generateArray(20),
    generateArray(30),
    generateArray(40),
    generateArray(50),
    generateArray(60),
    generateArray(70),
    generateArray(80),
    generateArray(90),
    generateArray(100)
];

//O(1)
function biggerThan(numbers:Array<number>, biggerThan:number): boolean {
    return numbers.length > biggerThan;
}

const suite = new Benchmark.Suite('O(1) will always execute in the same time regardless of the input size:');
for(let i = 0; i < arrays.length; i++) {
    suite.add(`${arrays[i].length}`, () => biggerThan(arrays[i], 1));
}
runBenchmark(suite);