import {ITypedArray} from './ITypedArray';
export class TypedStack {

    protected stack:ITypedArray;
    protected top:number = -1;
    protected max:number = -1;

    constructor (TypedArray: {new(size:number): ITypedArray}, size:number) {
        this.stack = new TypedArray(size);
        this.max = size;
    }

    public clear ():void {
        this.top = -1;
    }

    public isEmpty ():boolean {
        return this.top === -1;
    }

    public search (element:number):number {
        const index = this.stack.lastIndexOf(element);
        return (index < 0) ? -1 : this.top - index;
    }

    public peek ():number {
        if (this.isEmpty()) {
            throw new Error('The stack is empty');
        }
        return this.stack[this.top];
    }

    public pop ():number {
        if (this.top === -1) {
            throw new RangeError('The stack is empty.');
        }
        var index = this.top;
        this.top--;
        return this.stack[index];
    }

    public push (element:number) {
        if (this.top >= this.max) {
            throw new RangeError('You exceeded the buffer size.');
        }
        this.top++;
        this.stack[this.top] = element;
        return this;
    }

    public size ():number {
        return this.top + 1;
    }
}