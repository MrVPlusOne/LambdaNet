import {ObjectArray} from "../ts/ObjectArray";
describe('ObjectArray', function () {
    
    it('should be able to add items', function () {
        var list = new ObjectArray(200, [{name:'prev', type:Float64Array}, {name:'next', type:Float64Array}, {name:'value', type:Float64Array}]);
        var index = list.add([1,2,3]);
        expect(list.get(index)).toEqual([1,2,3]);
        index = list.add([2,3,4]);
        expect(list.get(index)).toEqual([2,3,4]);
        index = list.add([3,4,5]);
        expect(list.get(index)).toEqual([3,4,5]);
    });

    it('should throw an error if we exceed array max size', function () {
        var list = new ObjectArray(2, [{name:'prev', type:Float64Array}, {name:'next', type:Float64Array}, {name:'value', type:Float64Array}]);
        list.add([1,2,3]);
        list.add([1,2,3]);
        expect(list.add.bind(list)).toThrowError(RangeError);
    });

    it('should be able to remove items', function () {
        var list = new ObjectArray(200, [{name:'prev', type:Float64Array}, {name:'next', type:Float64Array}, {name:'value', type:Float64Array}]);
        var index = list.add([1,2,3]);
        expect(list.get(index)).toEqual([1,2,3]);
        list.remove(index);
        expect(list.get(index)).toEqual(null);
    });
});