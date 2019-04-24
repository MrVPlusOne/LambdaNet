let Benchmark = undefined;
import { generateArray, runBenchmark } from '../helpers';

let arrays = [
    generateArray(11),
    generateArray(21),
    generateArray(31),
    generateArray(41),
    generateArray(51),
    generateArray(61),
    generateArray(71),
    generateArray(81),
    generateArray(91)
];

//O(n^2)
function searchForProduct(numbers:Array<number>, moreNumbers:Array<number>, toFind:number):Array<number> {
    for(let i of numbers) {
        for(let j of moreNumbers) {
            if(i * j === toFind) {
                return [i, j];
            }
        }
    }
    return [-1, -1];
}

const suite = new Benchmark.Suite('O(n^2) will perform in proportion to the square of the data set');
for(let i = 0; i < arrays.length; i++) {
    let toFind = arrays[i][arrays[i].length-1] * arrays[i][arrays[i].length-1];
    suite.add(`${arrays[i].length}`, () => searchForProduct(arrays[i], arrays[i].slice(0), toFind));
}
runBenchmark(suite);