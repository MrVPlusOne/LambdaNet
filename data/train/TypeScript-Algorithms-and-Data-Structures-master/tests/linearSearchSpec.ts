import linearSearch from "../ts/linearSearch";
describe('Linear search', function () {
    it('should return null when value is not found', function () {
        expect(linearSearch([1,4,2,3], 5)).toBeNull();
    });

    it('should return index of value', function () {
        expect(linearSearch([1,4,2,3], 1)).toEqual(0);
    });
});