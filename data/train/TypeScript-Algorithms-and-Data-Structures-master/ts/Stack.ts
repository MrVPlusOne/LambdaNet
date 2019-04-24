export class Stack<T> {
    protected stack:T[] = [];

    public clear ():void {
        this.stack.length = 0;
    }

    public isEmpty ():boolean {
        return this.stack.length === 0;
    }

    public search (element:T):number {
        const index = this.stack.lastIndexOf(element);
        return (index < 0) ? -1 : this.stack.length - 1 - index;
    }

    public peek ():T {
        if (this.isEmpty()) {
            throw new Error('The stack is empty');
        }
        return this.stack[this.stack.length - 1];
    }

    public pop ():T {
        if (this.isEmpty()) {
            throw new Error('The stack is empty');
        }
        return this.stack.pop();
    }

    public push (element:T):Stack<T> {
        this.stack.push(element);
        return this;
    }

    public size ():number {
        return this.stack.length;
    }
}