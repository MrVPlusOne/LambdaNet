import {ITypedArray} from "./ITypedArray";
export class TypedQueue {
    protected queue: ITypedArray;
    protected head:number = 0;
    protected tail:number = 0;
    protected memoryLength:number = 0;

    constructor (queue:ITypedArray) {
        this.queue = queue;
        this.memoryLength = queue.length;
    }

    public enqueue (element:number) {
        if (this.size() === this.memoryLength - 1) {
            throw new RangeError('The queue is full');
        }
        this.queue[this.tail] = element;
        this.tail++;
        if (this.tail === this.memoryLength) {
            this.tail = 0;
        }
    }

    public dequeue ():number {
        if (this.isEmpty()) {
            throw new Error('The queue is empty');
        }
        var  element = this.queue[this.head];
        this.head++;
        if (this.head === this.memoryLength) {
            this.head = 0;
        }
        return element;
    }

    public isEmpty () {
        return this.tail === this.head;
    }

    public peek ():number {
        if (this.isEmpty()) {
            throw new Error('The stack is empty');
        }
        return this.queue[this.head];
    }

    public size () {
        return (this.tail < this.head) ? this.tail + this.memoryLength - this.head : this.tail - this.head;
    }
}