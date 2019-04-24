import { IExtendedEuclidesAlgorithmResult } from "../Interfaces/IExtendedEuclidesAlgorithmResult";

/**
 * Extended Euclid's algorithm for finding the greatest common divisor of two numbers greater than 0 and its polynomial prepresentation
 * @param {number} a greater than 0
 * @param {number} b greater than 0
 */
export function extendedEuclidesAlgorithm(a: number, b: number): IExtendedEuclidesAlgorithmResult {
    if (!Number.isInteger(a) || !Number.isInteger(b)) {
        throw new Error("Euclide's extended algorithm works only for positive integers");
    }
    if (a === 0 && b === 0) {
        throw new Error("Euclide's extended algorithm works only for positive integers");
    }
    const aSign = a < 0 ? -1 : 1;
    const bSign = b < 0 ? -1 : 1;
    a = Math.abs(a);
    b = Math.abs(b);
    if (a === b) {
        return {
            gcd: a,
            x: 1,
            y: 0
        };
    } else if (a < b) {
        const result = extendedEuclidesAlgorithmInternal(b, a);
        return {
            gcd: result.gcd,
            x: result.y * aSign,
            y: result.x * bSign
        }
    } else {
        const result = extendedEuclidesAlgorithmInternal(a, b);
        return {
            gcd: result.gcd,
            x: result.x * aSign,
            y: result.y * bSign
        }
    }
}

function extendedEuclidesAlgorithmInternal(a: number, b: number): IExtendedEuclidesAlgorithmResult {
    const result: IExtendedEuclidesAlgorithmResult = {
        gcd: a,
        x: 1,
        y: 0
    };
    if (b !== 0) {
        const recursiveResult = extendedEuclidesAlgorithmInternal(b, a % b);
        result.gcd = recursiveResult.gcd;
        result.x = recursiveResult.y;
        result.y = recursiveResult.x - recursiveResult.y * Math.floor(a / b);
    }
    return  result;
}