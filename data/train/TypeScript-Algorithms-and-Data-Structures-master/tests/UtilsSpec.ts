import {Utils} from "../ts/Utils";
describe("Utils", function () {
    describe("minIndex", function () {
        it("Should find the lowest index in part of array", function () {
            const ar = [3, 4, 5, 1, 2, 3, 1, 2];
            expect(Utils.minIndex(ar)).toEqual(3);
            expect(Utils.minIndex(ar, 0, 4)).toEqual(3);
            expect(Utils.minIndex(ar, 4)).toEqual(6);
            expect(Utils.minIndex([1, 4], 0, 2, Utils.gt)).toEqual(0);
            expect(Utils.minIndex([1, 4], 1, 2, Utils.gt)).toEqual(1);
        });
    });

    describe("swapValues", function () {
        it("Should swap elements in array", function () {
            const ar = [3, 4, 5, 1, 2, 3, 1, 2];
            Utils.swapValues(ar, 0, 1);
            expect(ar).toEqual([4, 3, 5, 1, 2, 3, 1, 2]);
            Utils.swapValues(ar, 3, 5);
            expect(ar).toEqual([4, 3, 5, 3, 2, 1, 1, 2]);
        });
    });
});