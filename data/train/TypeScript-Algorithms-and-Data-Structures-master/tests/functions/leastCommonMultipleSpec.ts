import { leastCommonMultiple } from "../../ts/functions/leastCommonMultiple";

describe('leastCommonMultiple', function () {
    it('should calculate the least common multiple of two co-prime numbers', function () {
        expect(leastCommonMultiple(3, 5)).toEqual(15);
    });
    it('should return 0 if both numbers are 0', function () {
        expect(leastCommonMultiple(0, 0)).toEqual(0);
    });
    it('should return first number if second is 0', function () {
        expect(leastCommonMultiple(3, 0)).toEqual(3);
    });
    it('should return the second if it is divisible by first', function () {
        expect(leastCommonMultiple(15, 30)).toEqual(30);
    });
    it('should return the first if it is divisible by second', function () {
        expect(leastCommonMultiple(6, 3)).toEqual(6);
    });
    it('should work for negative numbers too', function () {
        expect(leastCommonMultiple(-5, 3)).toEqual(15);
    });
});