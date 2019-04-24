let Benchmark = undefined;
import { runBenchmark } from '../helpers';

let arrays = [
    1, 2, 3, 4, 5, 6
];

//O(2n)
function fibonacci(num:number):number {
    if (num <= 1) {
        return num;
    }
    return fibonacci(num - 2) + fibonacci(num - 1);
}

const suite = new Benchmark.Suite('O(2^n) will perform in exponentially in proportion to size of the data set');
for(let i = 0; i < arrays.length; i++) {
    suite.add(`${arrays[i]}`, () => fibonacci(arrays[i]));
}

runBenchmark(suite);