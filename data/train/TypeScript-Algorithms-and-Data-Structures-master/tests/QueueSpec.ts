import {Queue} from "../ts/Queue";
describe('Queue', function () {

    describe('enqueue', function () {
        it('should add elements to the stack', function () {
            var q = new Queue();
            q.enqueue(1);
            expect(q.size()).toEqual(1);
            q.enqueue(2);
            expect(q.size()).toEqual(2);
            q.dequeue();
            expect(q.size()).toEqual(1);
        });
    });
});