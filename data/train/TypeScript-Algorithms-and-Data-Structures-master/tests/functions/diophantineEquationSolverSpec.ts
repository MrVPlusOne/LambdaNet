import { diophantineEquationSolver } from "../../ts/functions/diophantineEquationSolver";

describe("diophantineEquationSolver", () => {
    it("should return NaN if both parameters are 0", () => {
        expect(diophantineEquationSolver(0, 0, 0)).toBeNull();
    });
    it("should return NaN if gcd(a, b) foes not divide c", () => {
        expect(diophantineEquationSolver(15, 20, 3)).toBeNull();
    });
    it("should return result for Diophantine equation when one exist", () => {
        const solver = diophantineEquationSolver(10, 6, 14);
        expect(solver(0)).toEqual([-7, 14]);
    });
    it("should return result for Diophantine equation when result is negative", () => {
        const solver = diophantineEquationSolver(391, 299, -69);
        expect(solver(0)).toEqual([9, -12]);
    });
    it("should return proper solution for greater than zero index", () => {
        const solver = diophantineEquationSolver(98, 35, 14);
        expect(solver(1)).toEqual([3, -8]);
    });
    it("should return proper solution for greater than zero index", () => {
        const solver = diophantineEquationSolver(3, 5, 22);
        expect(solver(0)).toEqual([44, -22]);
    });
    it("should return proper solution for greater than zero index", () => {
        const solver = diophantineEquationSolver(3, 5, 22);
        expect(solver(1)).toEqual([49, -25]);
    });
    it("should return proper solution for greater than one index", () => {
        const solver = diophantineEquationSolver(3, 5, 22);
        expect(solver(2)).toEqual([54, -28]);
    });
});