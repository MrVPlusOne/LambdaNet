import binarySum from '../ts/binarySum';
describe('Binary sum', function () {
    it('should sum to arrays', function () {
        expect(binarySum([1,1], [0,1])).toEqual([1,0,0]); //3 + 1 = 4
        expect(binarySum([1,0], [0,1])).toEqual([0,1,1]); //2 + 1 = 3
        expect(binarySum([0,1,1,0], [1,1,1,1])).toEqual([1,0,1,0,1]); //6 + 15 = 3
    });
});