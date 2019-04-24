/**
 * Solves diophantine equation with 2 parameters.
 * @param {number} a
 * @param {number} b
 * @param {number} c
 */
import { greatestCommonDivisor } from "./greatestCommonDivisor";
import { extendedEuclidesAlgorithm } from "./extendedEuclidesAlgorithm";
import { IExtendedEuclidesAlgorithmResult } from "../Interfaces/IExtendedEuclidesAlgorithmResult";

export function diophantineEquation(a: number, b: number, c: number): number[] {
    if (a + b === 0) {
        return null;
    }
    const gcd = greatestCommonDivisor(a, b);
    const ratio: number = c / gcd;
    if (!Number.isInteger(ratio)) {
        return null;
    }
    const extendedEuclid: IExtendedEuclidesAlgorithmResult = extendedEuclidesAlgorithm(a, b);
    return [ratio * extendedEuclid.x, ratio * extendedEuclid.y];
}