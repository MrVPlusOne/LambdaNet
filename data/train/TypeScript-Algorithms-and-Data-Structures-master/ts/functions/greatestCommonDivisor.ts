/**
 * Euclid's algorithm for finding the greatest common divisor of two numbers greater than 0
 * @param {number} a greater than 0
 * @param {number} b greater than 0
 */
export function greatestCommonDivisor(a: number, b: number): number {
    if (!Number.isInteger(a) || !Number.isInteger(b)) {
        return NaN;
    }
    if (a === 0 && b === 0) {
        return NaN;
    }
    if (a < 0) {
        a = Math.abs(a);
    }
    if (b < 0) {
        b = Math.abs(b);
    }
    if (a === b) {
        return a;
    } else if (a < b) {
        return greatestCommonDivisorInternal(b, a);
    } else {
        return  greatestCommonDivisorInternal(a, b);
    }
}

function greatestCommonDivisorInternal(a: number, b: number): number {
    return  b > 0 ? greatestCommonDivisor(b , a % b) : a;
}