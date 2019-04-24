import mergeSort from "../../ts/Sort/mergeSort";
describe("Merge sort", function () {
    it("should sort", function () {
        expect(mergeSort([1, 4, 2, 3])).toEqual([1, 2, 3, 4]);
        expect(mergeSort([11, 4, 2, 3])).toEqual([2, 3, 4, 11]);
        expect(mergeSort([11, 4, 2])).toEqual([2, 4, 11]);
        expect(mergeSort([11, 4, 2, 3, 5])).toEqual([2, 3, 4, 5, 11]);
        expect(mergeSort([5, 2, 4, 6, 1, 3])).toEqual([1, 2, 3, 4, 5, 6]);
    });

    it("should return empty array if empty or one element array is passed", function () {
        expect(mergeSort([])).toEqual([]);
        expect(mergeSort([1])).toEqual([1]);
    });

    it("should reverse  sort", function () {
        expect(mergeSort([1, 4, 2, 3], true)).toEqual([4, 3, 2, 1]);
        expect(mergeSort([11, 4, 2, 3], true)).toEqual([11, 4, 3, 2]);
        expect(mergeSort([5, 2, 4, 6, 1, 3], true)).toEqual([6, 5, 4, 3, 2, 1]);
    });
});