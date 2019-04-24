import { extendedEuclidesAlgorithm } from "../../ts/functions/extendedEuclidesAlgorithm";

describe("extendedEuclidesAlgorithm", () => {
    it("should be able to find common divisor for the same number", () => {
        expect(extendedEuclidesAlgorithm(17, 17).gcd).toEqual(17);
    });
    it("should return 1 when no common divisor", () => {
        expect(extendedEuclidesAlgorithm(17, 6).gcd).toEqual(1);
    });
    it("should return first if it is multiple of second", () => {
        expect(extendedEuclidesAlgorithm(15, 35).gcd).toEqual(5);
    });
    it("should throw error if both parameters are 0", () => {
        expect(() => extendedEuclidesAlgorithm(0, 0)).toThrow();
    });
    it("should return second parameter if first is 0", () => {
        expect(extendedEuclidesAlgorithm(3, 0).gcd).toEqual(3);
    });
    it("should return first parameter if second is 0", () => {
        expect(extendedEuclidesAlgorithm(0, 4).gcd).toEqual(4);
    });
    it("should rthrow error if first parameter is not an integer", () => {
        expect(() => extendedEuclidesAlgorithm(1.3, 4)).toThrow();
    });
    it("should throw error if second parameter is not an integer", () => {
        expect(() => extendedEuclidesAlgorithm(1, 4.1)).toThrow();
    });
    it("should throw error if both parameters are not an integer", () => {
        expect(() => extendedEuclidesAlgorithm(1.1, 4.1)).toThrow();
    });
    it("should return first number if they are equal", () => {
        expect(extendedEuclidesAlgorithm(5, 5).gcd).toEqual(5);
    });
    it("should calculate proper gcd components if parameters are equal", () => {
        expect(extendedEuclidesAlgorithm(5, 5)).toEqual({
            gcd: 5,
            x: 1,
            y: 0
        });
    });
    it("should calculate proper gcd components if first parameter is 0", () => {
        expect(extendedEuclidesAlgorithm(0, 5)).toEqual({
            gcd: 5,
            x: 0,
            y: 1
        });
    });
    it("should calculate proper gcd components if second parameter is 0", () => {
        expect(extendedEuclidesAlgorithm(5, 0)).toEqual({
            gcd: 5,
            x: 1,
            y: 0
        });
    });
    it("should calculate proper gcd components when numbers have common divisor greater than 1", () => {
        expect(extendedEuclidesAlgorithm(10, 6)).toEqual({
            gcd: 2,
            x: -1,
            y: 2
        });
    });
    it("should calculate proper gcd components when first number is less than 0", () => {
        expect(extendedEuclidesAlgorithm(-10, 6)).toEqual({
            gcd: 2,
            x: 1,
            y: 2
        });
    });
    it("should calculate proper gcd components when second number is less than 0", () => {
        expect(extendedEuclidesAlgorithm(10, -6)).toEqual({
            gcd: 2,
            x: -1,
            y: -2
        });
    });
    it("should calculate proper gcd components when both numbers are less than 0", () => {
        expect(extendedEuclidesAlgorithm(-10, -6)).toEqual({
            gcd: 2,
            x: 1,
            y: -2
        });
    });
    it("should calculate proper gcd components when numbers have common divisor equal to 1", () => {
        expect(extendedEuclidesAlgorithm(7, 5)).toEqual({
            gcd: 1,
            x: -2,
            y: 3
        });
    });
    it("should calculate proper gcd components when numbers are more complicated", () => {
        expect(extendedEuclidesAlgorithm(391, 299)).toEqual({
            gcd: 23,
            x: -3,
            y: 4
        });
    });
});