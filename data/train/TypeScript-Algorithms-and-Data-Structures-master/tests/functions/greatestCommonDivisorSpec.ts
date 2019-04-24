import { greatestCommonDivisor } from "../../ts/functions/greatestCommonDivisor";

describe("greatestCommonDivisor", () => {
    it("should be able to find common divisor for the same number", () => {
        expect(greatestCommonDivisor(17, 17)).toEqual(17);
    });
    it("should return 1 when no common divisor", () => {
        expect(greatestCommonDivisor(17, 6)).toEqual(1);
    });
    it("should return first if it is multiple of second", () => {
        expect(greatestCommonDivisor(15, 35)).toEqual(5);
    });
    it("should work if first parameter is negative numbers", () => {
        expect(greatestCommonDivisor(-15, 35)).toEqual(5);
    });
    it("should work if second parameter is less than 0", () => {
        expect(greatestCommonDivisor(15, -35)).toEqual(5);
    });
    it("should work if both parameter are less than 0", () => {
        expect(greatestCommonDivisor(-15, -35)).toEqual(5);
    });
    it("should return NaN if both parameters are 0", () => {
        expect(greatestCommonDivisor(0, 0)).toBeNaN();
    });
    it("should return second parameter if first is 0", () => {
        expect(greatestCommonDivisor(3, 0)).toEqual(3);
    });
    it("should return first parameter if second is 0", () => {
        expect(greatestCommonDivisor(0, 4)).toEqual(4);
    });
    it("should return NaN if first parameter is not an integer", () => {
        expect(greatestCommonDivisor(1.3, 4)).toBeNaN();
    });
    it("should return NaN if second parameter is not an integer", () => {
        expect(greatestCommonDivisor(1, 4.1)).toBeNaN();
    });
    it("should return NaN if both parameters are not an integer", () => {
        expect(greatestCommonDivisor(1.1, 4.1)).toBeNaN();
    });
    it("should return first number if they are equal", () => {
        expect(greatestCommonDivisor(5, 5)).toEqual(5);
    });
});