import { diophantineEquation } from "../../ts/functions/diophantineEquation";

describe("diophantineEquation", () => {
    it("should return NaN if both parameters are 0", () => {
        expect(diophantineEquation(0, 0, 0)).toBeNull();
    });
    it("should return NaN if gcd(a, b) foes not divide c", () => {
        expect(diophantineEquation(15, 20, 3)).toBeNull();
    });
    it("should return result for Diophantine equation when one exist", () => {
        expect(diophantineEquation(10, 6, 14)).toEqual([-7, 14]);
    });
    it("should return result for Diophantine equation when result is negative", () => {
        expect(diophantineEquation(391, 299, -69)).toEqual([9, -12]);
    });
});