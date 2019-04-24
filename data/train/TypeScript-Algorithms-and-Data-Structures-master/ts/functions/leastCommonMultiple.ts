import { greatestCommonDivisor } from "./greatestCommonDivisor";

/**
 * Least common multiple algorithm. If a and b are 0 it is a special cases and 0 will be returned. If one of the number is 0 the other one will be returned.
 * @param {number} a
 * @param {number} b
 */
export function leastCommonMultiple(a: number, b: number): number {
    if (a === 0 && b === 0) {
        return 0;
    }
    if (a === 0) {
        return b;
    }
    if (b === 0) {
        return Math.abs(a);
    }
    return Math.abs(a * b) / greatestCommonDivisor(Math.abs(a), Math.abs(b))
}