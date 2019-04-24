import quickSort from "../../ts/Sort/quickSort";
import {Utils} from "../../ts/Utils";
describe("quicksort sort", function () {
    it("should sort", function () {
        expect(quickSort([4, 1])).toEqual([1, 4]);
        expect(quickSort([4, 1, 3])).toEqual([1, 3, 4]);
        expect(quickSort([1, 4, 2, 3])).toEqual([1, 2, 3, 4]);
        expect(quickSort([11, 4, 2, 3])).toEqual([2, 3, 4, 11]);
        expect(quickSort([11, 4, 2])).toEqual([2, 4, 11]);
        expect(quickSort([11, 4, 2, 3, 5])).toEqual([2, 3, 4, 5, 11]);
        expect(quickSort([5, 2, 4, 6, 1, 3])).toEqual([1, 2, 3, 4, 5, 6]);
    });

    it("should return empty array if empty or one element array is passed", function () {
        expect(quickSort([], Utils.gt)).toEqual([]);
        expect(quickSort([1], Utils.gt)).toEqual([1]);
    });

    it("should reverse  sort", function () {
        expect(quickSort([1, 4, 2, 3], Utils.lt)).toEqual([4, 3, 2, 1]);
        expect(quickSort([11, 4, 2, 3], Utils.lt)).toEqual([11, 4, 3, 2]);
        expect(quickSort([5, 2, 4, 6, 1, 3], Utils.lt)).toEqual([6, 5, 4, 3, 2, 1]);
    });

    it("should work for any kind of objects", function () {
        expect(quickSort(["a", "b", "a", "C", "d"], Utils.gt)).toEqual(["C", "a", "a", "b", "d"]);
        expect(quickSort([{id: 5}, {id: 4}, {id: 3}, {id: 2}], (a, b) => a.id > b.id)).toEqual([{id: 2}, {id: 3}, {id: 4}, {id: 5}]);
    });
});