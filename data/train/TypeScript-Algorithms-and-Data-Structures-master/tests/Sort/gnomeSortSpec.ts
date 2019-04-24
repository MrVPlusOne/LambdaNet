import gnomeSort from "../../ts/Sort/gnomeSort";
describe("Gnome/Simple sort", function () {
    it("should sort", function () {
        expect(gnomeSort([1, 4, 2, 3])).toEqual([1, 2, 3, 4]);
        expect(gnomeSort([11, 4, 2, 3])).toEqual([2, 3, 4, 11]);
        expect(gnomeSort([11, 4, 2])).toEqual([2, 4, 11]);
        expect(gnomeSort([11, 4, 2, 3, 5])).toEqual([2, 3, 4, 5, 11]);
        expect(gnomeSort([5, 2, 4, 6, 1, 3])).toEqual([1, 2, 3, 4, 5, 6]);
    });

    it("should return empty array if empty or one element array is passed", function () {
        expect(gnomeSort([])).toEqual([]);
        expect(gnomeSort([1])).toEqual([1]);
    });

    it("should reverse  sort", function () {
        expect(gnomeSort([1, 4, 2, 3], (a, b) => a < b)).toEqual([4, 3, 2, 1]);
        expect(gnomeSort([11, 4, 2, 3], (a, b) => a < b)).toEqual([11, 4, 3, 2]);
        expect(gnomeSort([5, 2, 4, 6, 1, 3], (a, b) => a < b)).toEqual([6, 5, 4, 3, 2, 1]);
    });

    it("should work for any kind of objects", function () {
        expect(gnomeSort(["a", "b", "a", "C", "d"], (a, b) => a > b)).toEqual(["C", "a", "a", "b", "d"]);
        expect(gnomeSort([{id: 5}, {id: 4}, {id: 3}, {id: 2}], (a, b) => a.id > b.id)).toEqual([{id: 2}, {id: 3}, {id: 4}, {id: 5}]);
    });
});