import FindMaximumSubarray from "../ts/FindMaximumSubarray";
describe(`Find maximum subarray`, function () {
    it(`Find maximum subarray`, function () {
        let [leftIndex, rightIndex, sum] = FindMaximumSubarray([-1, 3, 2, 5, -1]);
        expect(leftIndex).toEqual(1);
        expect(rightIndex).toEqual(3);
        expect(sum).toEqual(10);
        [leftIndex, rightIndex, sum] = FindMaximumSubarray([-1, 1]);
        expect(leftIndex).toEqual(1);
        expect(rightIndex).toEqual(1);
        expect(sum).toEqual(1);
        [leftIndex, rightIndex, sum] = FindMaximumSubarray([-1, 1, -1]);
        expect(leftIndex).toEqual(1);
        expect(rightIndex).toEqual(1);
        expect(sum).toEqual(1);
        [leftIndex, rightIndex, sum] = FindMaximumSubarray([-1]);
        expect(leftIndex).toEqual(0);
        expect(rightIndex).toEqual(0);
        expect(sum).toEqual(0);
    });
});