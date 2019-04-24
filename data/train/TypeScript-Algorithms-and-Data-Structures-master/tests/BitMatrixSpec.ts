import {BitMatrix} from "../ts/BitMatrix";

describe("BitMatrix", function () {
    it("be able to set bits", function () {
        const bm = new BitMatrix(100, 100);
        bm.set(1, 1, true);
        bm.set(99, 0, true);
        bm.set(99, 1, true);
        bm.set(99, 99, true);
        expect(bm.get(0, 1)).toEqual(false);
        expect(bm.get(1, 1)).toEqual(true);
        expect(bm.get(54, 34)).toEqual(false);
        expect(bm.get(99, 0)).toEqual(true);
        expect(bm.get(99, 1)).toEqual(true);
        expect(bm.get(99, 99)).toEqual(true);
    });

    it("be able to get set bits count", function () {
        const bm = new BitMatrix(100, 100);
        bm.set(1, 1, true);
        bm.set(99, 1, true);
        expect(bm.count()).toEqual(2);
        bm.set(99, 1, false);
        expect(bm.count()).toEqual(1);
    });

    it("be able to get set bits per row", function () {
        const bm = new BitMatrix(100, 100);
        bm.set(1, 2, true);
        bm.set(99, 3, true);
        expect(bm.getIndexes()[0]).toEqual([]);
        expect(bm.getIndexes()[1]).toEqual([2]);
        expect(bm.getIndexes()[99]).toEqual([3]);
        bm.set(29, 4, true);
        expect(bm.getIndexes()[0]).toEqual([]);
        expect(bm.getIndexes()[1]).toEqual([2]);
        expect(bm.getIndexes()[29]).toEqual([4]);
        expect(bm.getIndexes()[99]).toEqual([3]);
        bm.set(29, 4, false);
        expect(bm.getIndexes()[0]).toEqual([]);
        expect(bm.getIndexes()[1]).toEqual([2]);
        expect(bm.getIndexes()[29]).toEqual([]);
        expect(bm.getIndexes()[99]).toEqual([3]);
    });

    it("be able to get set bits per column", function () {
        const bm = new BitMatrix(100, 100);
        bm.set(2, 1, true);
        bm.set(3, 99, true);
        expect(bm.getIndexes(true)[0]).toEqual([]);
        expect(bm.getIndexes(true)[1]).toEqual([2]);
        expect(bm.getIndexes(true)[99]).toEqual([3]);
        bm.set(4, 29, true);
        expect(bm.getIndexes(true)[0]).toEqual([]);
        expect(bm.getIndexes(true)[1]).toEqual([2]);
        expect(bm.getIndexes(true)[29]).toEqual([4]);
        expect(bm.getIndexes(true)[99]).toEqual([3]);
        bm.set(4, 29, false);
        expect(bm.getIndexes(true)[0]).toEqual([]);
        expect(bm.getIndexes(true)[1]).toEqual([2]);
        expect(bm.getIndexes(true)[29]).toEqual([]);
        expect(bm.getIndexes(true)[99]).toEqual([3]);
    });

    it("should throw error if trying to set bit out of bounds", function () {
        const bm = new BitMatrix(100, 100);
        expect(() => bm.set(1, 1, true)).not.toThrowError();
        expect(() => bm.set(100, 100, true)).toThrowError();
        expect(() => bm.set(100, 1, true)).toThrowError();
        expect(() => bm.set(101, 0, true)).toThrowError();
        expect(() => bm.set(101, 0, true)).toThrowError();
        expect(() => bm.set(-1, 1, true)).toThrowError();
        expect(() => bm.set(-1, -1, true)).toThrowError();
        expect(() => bm.set(1, -1, true)).toThrowError();
    });

    it("should be able to zero all values", function () {
        const bs = new BitMatrix(100, 100);
        bs.set(0, 1, true);
        bs.set(0, 99, true);
        bs.set(57, 1, true);
        bs.set(57, 99, true);
        bs.set(99, 1, true);
        bs.set(99, 99, true);
        expect(bs.get(0, 0)).toEqual(false);
        expect(bs.get(0, 1)).toEqual(true);
        expect(bs.get(0, 54)).toEqual(false);
        expect(bs.get(0, 99)).toEqual(true);
        expect(bs.get(57, 0)).toEqual(false);
        expect(bs.get(57, 1)).toEqual(true);
        expect(bs.get(57, 54)).toEqual(false);
        expect(bs.get(57, 99)).toEqual(true);
        expect(bs.get(99, 0)).toEqual(false);
        expect(bs.get(99, 1)).toEqual(true);
        expect(bs.get(99, 54)).toEqual(false);
        expect(bs.get(99, 99)).toEqual(true);
        bs.reset();
        expect(bs.get(0, 0)).toEqual(false);
        expect(bs.get(0, 1)).toEqual(false);
        expect(bs.get(0, 54)).toEqual(false);
        expect(bs.get(0, 99)).toEqual(false);
        expect(bs.get(57, 0)).toEqual(false);
        expect(bs.get(57, 1)).toEqual(false);
        expect(bs.get(57, 54)).toEqual(false);
        expect(bs.get(57, 99)).toEqual(false);
        expect(bs.get(99, 0)).toEqual(false);
        expect(bs.get(99, 1)).toEqual(false);
        expect(bs.get(99, 54)).toEqual(false);
        expect(bs.get(99, 99)).toEqual(false);
    });

    describe("resize", function () {
        it("should be able to extend the BitMatrix", function () {
            const bm = new BitMatrix(100, 110);
            bm.set(1, 1, true);
            bm.set(1, 2, true);
            bm.set(1, 99, true);
            bm.resize(300, 302);
            expect(bm.get(1, 300)).toEqual(false);
            expect(bm.get(1, 301)).toEqual(false);
            bm.set(151, 300, true);
            bm.set(151, 301, true);
            expect(bm.get(151, 300)).toEqual(true);
            expect(bm.get(151, 301)).toEqual(true);
        });

        it("should be able to shrink the BitMatrix", function () {
            const bm = new BitMatrix(100, 100);
            bm.set(23, 1, true);
            bm.set(23, 2, true);
            bm.set(23, 99, true);
            bm.resize(55, 51);
            expect(bm.get(23, 49)).toEqual(false);
            expect(bm.get(23, 50)).toEqual(false);
            bm.set(23, 50, true);
            expect(bm.get(23, 49)).toEqual(false);
            expect(bm.get(23, 50)).toEqual(true);
        });

        it("nothing should be affected if the size doesn't change", function () {
            const bm = new BitMatrix(100, 100);
            bm.set(1, 1, true);
            bm.set(2, 1, true);
            bm.set(99, 1, true);
            bm.resize(100, 100);
            expect(bm.get(0, 1)).toEqual(false);
            expect(bm.get(1, 1)).toEqual(true);
            expect(bm.get(54, 1)).toEqual(false);
            expect(bm.get(99, 1)).toEqual(true);
        });

        it("should throw error if index is invalid", function () {
            const bm = new BitMatrix(1, 1);
            expect(() => bm.resize(-1, 10)).toThrowError();
            expect(() => bm.resize(-1, -1)).toThrowError();
            expect(() => bm.resize(11, -1)).toThrowError();
        });
    });

    describe("splice column", function () {
        it("should remove number of elements", function () {
            const bs = new BitMatrix(70, 100);
            bs.set(1, 1, true);
            bs.set(1, 2, true);
            bs.set(1, 5, true);
            bs.set(1, 6, true);
            bs.set(1, 99, true);
            bs.spliceColumn(2, 3);
            expect(bs.size()).toEqual([70, 97]);
            expect(bs.get(1, 1)).toEqual(true);
            expect(bs.get(1, 2)).toEqual(true);
            expect(bs.get(1, 3)).toEqual(true);
            expect(bs.get(1, 5)).toEqual(false);
            expect(bs.get(1, 96)).toEqual(true);
        });

        it("should be able to provide negative start index", function () {
            const bs = new BitMatrix(60, 100);
            bs.set(1, 1, true);
            bs.set(1, 2, true);
            bs.set(1, 5, true);
            bs.set(1, 6, true);
            bs.set(1, 99, true);
            bs.spliceColumn(-98, 3);
            expect(bs.size()).toEqual([60, 97]);
            expect(bs.get(1, 1)).toEqual(true);
            expect(bs.get(1, 2)).toEqual(true);
            expect(bs.get(1, 3)).toEqual(true);
            expect(bs.get(1, 5)).toEqual(false);
            expect(bs.get(1, 96)).toEqual(true);
        });

        it("delete is NaN or less than 1 do nothing", function () {
            const bm = new BitMatrix(100, 100);
            bm.spliceColumn(-98, -3);
            expect(bm.size()).toEqual([100, 100]);
            bm.spliceColumn(-98, Number.NaN);
            expect(bm.size()).toEqual([100, 100]);
        });

        it("should throw error if start index is greater than array length", function () {
            const bm = new BitMatrix(100, 100);
            expect(() => bm.spliceColumn(101, 2)).toThrowError();
        });

        it("should splice from 0 if negative value passed and it is greater than array length by abs value", function () {
            const bm = new BitMatrix(100, 100);
            expect(() => bm.spliceColumn(-101, 2)).toThrowError();
        });
    });


    describe("splice row", function () {
        it("should remove number of elements", function () {
            const bs = new BitMatrix(100, 100);
            bs.set(1, 1, true);
            bs.set(2, 1, true);
            bs.set(5, 1, true);
            bs.set(6, 1, true);
            bs.set(99, 1,  true);
            bs.spliceRow(2, 3);
            expect(bs.size()).toEqual([97, 100]);
            expect(bs.get( 1, 1)).toEqual(true);
            expect(bs.get( 2, 1)).toEqual(true);
            expect(bs.get( 3, 1)).toEqual(true);
            expect(bs.get( 5, 1)).toEqual(false);
            expect(bs.get( 96, 1)).toEqual(true);
        });

        it("should be able to provide negative start index", function () {
            const bs = new BitMatrix(100, 100);
            bs.set(1, 1, true);
            bs.set(2, 1, true);
            bs.set(5, 1, true);
            bs.set(6, 1, true);
            bs.set(99, 1, true);
            bs.spliceRow(-98, 3);
            expect(bs.size()).toEqual([97, 100]);
            expect(bs.get( 1, 1)).toEqual(true);
            expect(bs.get( 2, 1)).toEqual(true);
            expect(bs.get( 3, 1)).toEqual(true);
            expect(bs.get( 5, 1)).toEqual(false);
            expect(bs.get( 96, 1)).toEqual(true);
        });

        it("delete is NaN or less than 1 do nothing", function () {
            const bm = new BitMatrix(100, 100);
            bm.spliceRow(-98, -3);
            expect(bm.size()).toEqual([100, 100]);
            bm.spliceRow(-98, Number.NaN);
            expect(bm.size()).toEqual([100, 100]);
        });

        it("should throw error if start index is greater than array length", function () {
            const bm = new BitMatrix(100, 100);
            expect(() => bm.spliceRow(101, 2)).toThrowError();
        });

        it("should splice from 0 if negative value passed and it is greater than array length by abs value", function () {
            const bm = new BitMatrix(100, 100);
            expect(() => bm.spliceRow(-101, 2)).toThrowError();
        });
    });

    it("getColIndexes should return set indexes for column", function () {
        const bs = new BitMatrix(100, 70);
        bs.set(1, 1, true);
        bs.set(2, 1, true);
        bs.set(5, 1, true);
        bs.set(6, 1, true);
        bs.set(99, 1,  true);
        bs.set(6, 31, true);
        bs.set(31, 31,  true);
        bs.set(6, 32, true);
        bs.set(32, 32,  true);
        expect(bs.getColIndexes(1)).toEqual([1, 2, 5, 6, 99]);
        expect(bs.getColIndexes(31)).toEqual([6, 31]);
        expect(bs.getColIndexes(32)).toEqual([6, 32]);
    });

    it("getRowIndexes should return set indexes for column", function () {
        const bs = new BitMatrix(70, 100);
        bs.set(1, 1, true);
        bs.set(1, 2, true);
        bs.set(1, 5, true);
        bs.set(1, 6, true);
        bs.set(1, 99,  true);
        bs.set(31, 6, true);
        bs.set(31, 31, true);
        bs.set(32, 6, true);
        bs.set(32, 32, true);
        expect(bs.getRowIndexes(1)).toEqual([1, 2, 5, 6, 99]);
        expect(bs.getRowIndexes(31)).toEqual([6, 31]);
        expect(bs.getRowIndexes(32)).toEqual([6, 32]);
    });
    it("be able to get set bits per column when column count < row count", function () {
        const bm = new BitMatrix(100, 10);
        bm.set(2, 1, true);
        bm.set(3, 9, true);
        expect(bm.getIndexes(true)[0]).toEqual([]);
        expect(bm.getIndexes(true)[1]).toEqual([2]);
        expect(bm.getIndexes(true)[9]).toEqual([3]);
        bm.set(4, 8, true);
        expect(bm.getIndexes(true)[0]).toEqual([]);
        expect(bm.getIndexes(true)[1]).toEqual([2]);
        expect(bm.getIndexes(true)[8]).toEqual([4]);
        expect(bm.getIndexes(true)[9]).toEqual([3]);
        bm.set(4, 8, false);
        expect(bm.getIndexes(true)[0]).toEqual([]);
        expect(bm.getIndexes(true)[1]).toEqual([2]);
        expect(bm.getIndexes(true)[8]).toEqual([]);
        expect(bm.getIndexes(true)[9]).toEqual([3]);
    });
});