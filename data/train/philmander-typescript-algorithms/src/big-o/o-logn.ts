let Benchmark = undefined;
import { generateArray, runBenchmark } from '../helpers';
import  binarySeach  from '../binary-search/iterative';

let arrays = [
    generateArray(1000),
    generateArray(2000),
    generateArray(3000),
    generateArray(4000),
    generateArray(5000),
    generateArray(6000),
    generateArray(7000),
    generateArray(8000),
    generateArray(9000),
    generateArray(10000)
];
//O(log(2, n)
const suite = new Benchmark.Suite('O(2, n) will perform logarithmically in proportion to the size of the data set:');
for(let i = 0; i < arrays.length; i++) {
    let toFind = arrays[i][Math.round(arrays[i].length * 0.3)];
    suite.add(`${arrays[i].length}`, () => binarySeach(toFind, arrays[i]));
}
runBenchmark(suite);