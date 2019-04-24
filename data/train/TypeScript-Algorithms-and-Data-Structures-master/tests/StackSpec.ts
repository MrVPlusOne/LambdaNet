import {Stack} from "../ts/Stack";
describe('Stack', function () {
    describe('clear', function () {
        it('should remove all elements from the stack', function () {
            var s = new Stack();
            s.push(1);
            s.push(2);
            expect(s.size()).toEqual(2);
            s.push(3);
            expect(s.size()).toEqual(3);
            s.clear();
            expect(s.size()).toEqual(0);
            expect(s.isEmpty()).toEqual(true);
        });
    });

    describe('empty', function () {
        it('should return true if stack is empty', function () {
            var s = new Stack();
            expect(s.isEmpty()).toEqual(true);
            s.push(1);
            expect(s.isEmpty()).toEqual(false);
            s.clear();
            expect(s.isEmpty()).toEqual(true);
        });
    });

    describe('search', function () {
        it('should return the index from the top of the element in the stack', function () {
            var s = new Stack();
            s.push(1);
            expect(s.search(1)).toEqual(0);
            s.push(2);
            expect(s.search(1)).toEqual(1);
            expect(s.search(2)).toEqual(0);
            s.clear();
            expect(s.search(1)).toEqual(-1);
        });
    });

    describe('peek', function () {
        it('should return the last element added to the stack', function () {
            var s = new Stack();
            s.push(1);
            expect(s.peek()).toEqual(1);
            s.push(2);
            expect(s.peek()).toEqual(2);
            s.pop();
            expect(s.peek()).toEqual(1);
            s.pop();
            expect(s.peek).toThrow();
        });
    });

    describe('pop', function () {
        it('should pop the last element added to the stack', function () {
            var s = new Stack();
            s.push(1);
            expect(s.pop()).toEqual(1);
            s.push(2);
            s.push(3);
            expect(s.pop()).toEqual(3);
            expect(s.pop()).toEqual(2);
            expect(s.pop).toThrow();
        });
    });

    describe('push', function () {
        it('should add element to the stack', function () {
            var s = new Stack();
            s.push(1);
            expect(s.size()).toEqual(1);
            s.push(1);
            expect(s.size()).toEqual(2);
        });
    });

    describe('size', function () {
        it('should return the number of items on the stack', function () {
            var s = new Stack();
            s.push(1);
            expect(s.size()).toEqual(1);
            s.push(1);
            expect(s.size()).toEqual(2);
            s.push(1);
            expect(s.size()).toEqual(3);
            s.pop();
            expect(s.size()).toEqual(2);
            s.clear();
            expect(s.size()).toEqual(0);
        });
    });
});