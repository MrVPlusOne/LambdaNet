import {LinkedList} from "../ts/LinkedList";
describe('LinkedList', function () {

    it('should be able to push items', function () {
        var list = new LinkedList();
        expect(list.length).toEqual(0);
        list.push(1);
        expect(list.length).toEqual(1);
        list.push(1);
        expect(list.length).toEqual(2);
        list.pop();
        expect(list.length).toEqual(1);
        list.pop();
        expect(list.length).toEqual(0);
    });

    it('should be able to go through all elements using forEach', function () {
        var list = new LinkedList();
        list.push(1);
        list.push(2);
        list.push(3);
        list.push(4);
        list.forEach((el, index) => {
            expect(el).toEqual(index + 1);
        });
    });

    it('should be able to unshift items', function () {
        var list = new LinkedList();
        expect(list.length).toEqual(0);
        list.unshift(1);
        expect(list.length).toEqual(1);
        list.unshift(1);
        expect(list.length).toEqual(2);
        list.shift();
        expect(list.length).toEqual(1);
        list.shift();
        expect(list.length).toEqual(0);
    });

    it('should be able to pop items', function () {
        var list = new LinkedList();
        expect(list.length).toEqual(0);
        list.push(1);
        expect(list.length).toEqual(1);
        list.push(2);
        expect(list.length).toEqual(2);
        expect(list.pop()).toEqual(2);
        expect(list.length).toEqual(1);
        expect(list.pop()).toEqual(1);
        expect(list.length).toEqual(0);
    });

    it('pop should throw error if the list is empty', function () {
        var list = new LinkedList();
        expect(list.pop).toThrow();
        list.push(1);
        expect(list.pop()).toEqual(1);
        expect(list.pop).toThrow();
    });

    it('isEmpty should return true when there are no items in the list', function () {
        var list = new LinkedList();
        expect(list.isEmpty()).toEqual(true);
        list.push(1);
        expect(list.length).toEqual(1);
        list.push(2);
        expect(list.length).toEqual(2);
        expect(list.pop()).toEqual(2);
        expect(list.length).toEqual(1);
        expect(list.pop()).toEqual(1);
        expect(list.isEmpty()).toEqual(true);
    });

    it('remove should remove item from the list', function () {
        var list = new LinkedList();
        list.push(1);
        list.push(2);
        list.push(3);
        expect(list.length).toEqual(3);
        list.remove(3);
        expect(list.length).toEqual(2);
        list.remove(1);
        expect(list.length).toEqual(1);
        list.remove(2);
        expect(list.length).toEqual(0);
    });

    it('remove should throw if not existing item is being removed', function () {
        var list = new LinkedList();
        expect(function () { list.remove(5);}).toThrow();
        list.push(1);
        list.remove(1);
        expect(function () { list.remove(5);}).toThrow();
    });
});