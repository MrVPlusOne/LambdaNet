import {BitArray} from "../ts/BitArray";

describe("BitArray", function () {
    it("be able to set bits", function () {
        const bs = new BitArray(100);
        bs.set(1, true);
        bs.set(99, true);
        expect(bs.get(0)).toEqual(false);
        expect(bs.get(1)).toEqual(true);
        expect(bs.get(54)).toEqual(false);
        expect(bs.get(99)).toEqual(true);
    });

    it("be able to get set bits count", function () {
        const bs = new BitArray(100);
        bs.set(1, true);
        bs.set(99, true);
        expect(bs.count()).toEqual(2);
        bs.set(99, false);
        expect(bs.count()).toEqual(1);
    });

    it("be able to get set bits count", function () {
        const bs = new BitArray(100);
        bs.set(1, true);
        bs.set(99, true);
        expect(bs.getIndexes()).toEqual([1, 99]);
        bs.set(29, true);
        expect(bs.getIndexes()).toEqual([1, 29, 99]);
        bs.set(29, false);
        expect(bs.getIndexes()).toEqual([1, 99]);
    });

    it("should throw error if trying to set bit out of bounds", function () {
        const bs = new BitArray(100);
        expect(() => bs.set(1, true)).not.toThrowError();
        expect(() => bs.set(100, true)).toThrowError();
        expect(() => bs.set(101, true)).toThrowError();
        expect(() => bs.set(-1, true)).toThrowError();
    });

    it("should be able to zero all values", function () {
        const bs = new BitArray(100);
        bs.set(1, true);
        bs.set(99, true);
        expect(bs.get(0)).toEqual(false);
        expect(bs.get(1)).toEqual(true);
        expect(bs.get(54)).toEqual(false);
        expect(bs.get(99)).toEqual(true);
        bs.reset();
        expect(bs.get(0)).toEqual(false);
        expect(bs.get(1)).toEqual(false);
        expect(bs.get(54)).toEqual(false);
        expect(bs.get(99)).toEqual(false);
    });

    describe("resize", function () {
        it("should be able to extend the BitArray", function () {
            const bs = new BitArray(100);
            bs.set(1, true);
            bs.set(2, true);
            bs.set(99, true);
            bs.resize(302);
            expect(bs.get(300)).toEqual(false);
            expect(bs.get(301)).toEqual(false);
            bs.set(300, true);
            bs.set(301, true);
            expect(bs.get(300)).toEqual(true);
            expect(bs.get(301)).toEqual(true);
        });

        it("should be able to shrink the BitArray", function () {
            const bs = new BitArray(100);
            bs.set(1, true);
            bs.set(2, true);
            bs.set(99, true);
            bs.resize(51);
            expect(bs.get(49)).toEqual(false);
            expect(bs.get(50)).toEqual(false);
            bs.set(50, true);
            expect(bs.get(49)).toEqual(false);
            expect(bs.get(50)).toEqual(true);
        });

        it("nothing should be affected if the size doesn't change", function () {
            const bs = new BitArray(100);
            bs.set(1, true);
            bs.set(2, true);
            bs.set(99, true);
            bs.resize(100);
            expect(bs.get(0)).toEqual(false);
            expect(bs.get(1)).toEqual(true);
            expect(bs.get(54)).toEqual(false);
            expect(bs.get(99)).toEqual(true);
        });

        it("should throw error if index is invalid", function () {
            const bs = new BitArray(100);
            expect(() => bs.resize(-1)).toThrowError();
        });
    });

    describe("splice", function () {
        it("should remove number of elements", function () {
            const bs = new BitArray(100);
            bs.set(1, true);
            bs.set(2, true);
            bs.set(5, true);
            bs.set(6, true);
            bs.set(99, true);
            bs.splice(2, 3);
            expect(bs.size()).toEqual(97);
            expect(bs.get(1)).toEqual(true);
            expect(bs.get(2)).toEqual(true);
            expect(bs.get(3)).toEqual(true);
            expect(bs.get(5)).toEqual(false);
            expect(bs.get(96)).toEqual(true);
        });

        it("should be able to provide negative start index", function () {
            const bs = new BitArray(100);
            bs.set(1, true);
            bs.set(2, true);
            bs.set(5, true);
            bs.set(6, true);
            bs.set(99, true);
            bs.splice(-98, 3);
            expect(bs.size()).toEqual(97);
            expect(bs.get(1)).toEqual(true);
            expect(bs.get(2)).toEqual(true);
            expect(bs.get(3)).toEqual(true);
            expect(bs.get(5)).toEqual(false);
            expect(bs.get(96)).toEqual(true);
        });

        it("delete is NaN or less than 1 do nothing", function () {
            const bs = new BitArray(100);
            bs.splice(-98, -3);
            expect(bs.size()).toEqual(100);
            bs.splice(-98, Number.NaN);
            expect(bs.size()).toEqual(100);
        });

        it("should throw error if start index is greater than array length", function () {
            const bs = new BitArray(100);
            expect(() => bs.splice(101, 2)).toThrowError();
        });

        it("should splice from 0 if negative value passed and it is greater than array length by abs value", function () {
            const bs = new BitArray(100);
            expect(() => bs.splice(-101, 2)).toThrowError();
        });
    });
});