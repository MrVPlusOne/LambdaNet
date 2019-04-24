/**
 * Should return a function that when provided with index will return solution for diophantine equation
 * @param {number} a
 * @param {number} b
 * @param {number} c
 */
import { greatestCommonDivisor } from "./greatestCommonDivisor";
import { extendedEuclidesAlgorithm } from "./extendedEuclidesAlgorithm";
import { IExtendedEuclidesAlgorithmResult } from "../Interfaces/IExtendedEuclidesAlgorithmResult";

export function diophantineEquationSolver(a: number, b: number, c: number): (index: number) => number[] {
    if (a + b === 0) {
        return null;
    }
    const gcd = greatestCommonDivisor(a, b);
    const ratio: number = c / gcd;
    if (!Number.isInteger(ratio)) {
        return null;
    }
    const extendedEuclid: IExtendedEuclidesAlgorithmResult = extendedEuclidesAlgorithm(a, b);
    const x0: number = ratio * extendedEuclid.x;
    const y0: number = ratio * extendedEuclid.y;
    const p: number = a / gcd;
    const q: number = b / gcd;
    return (index: number) => {
        return [x0 + (q * index), y0 - (p * index)];
    }
}