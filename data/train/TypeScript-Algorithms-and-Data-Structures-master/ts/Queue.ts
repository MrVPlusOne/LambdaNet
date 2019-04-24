export class Queue<T> {
    protected queue: T[] = [];
    protected head: number = 0;
    protected  MAX_QUEUE_SIZE = 1000;

    constructor(MAX_QUEUE_SIZE?: number) {
        this.MAX_QUEUE_SIZE = MAX_QUEUE_SIZE;
    }

    public enqueue (element: T) {
        this.queue.push(element);
    }

    public dequeue (): T {
        const output = this.queue[this.head];
        this.queue[this.head] = null;
        this.head++;
        if (this.head >= this.MAX_QUEUE_SIZE) {
            this.queue.splice(0, this.head);
            this.head = 0;
        }

        return output;
    }

    public size () {
        return this.queue.length - this.head;
    }
}