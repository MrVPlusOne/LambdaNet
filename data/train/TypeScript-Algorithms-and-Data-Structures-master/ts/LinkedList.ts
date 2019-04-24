import {ILinkedListItem} from "./Interfaces/ILinkedListItem";
export class LinkedList<T> {
    public head:ILinkedListItem<T> = LinkedList.emptyListItem<T>();
    public length:number = 0;
    public tail:ILinkedListItem<T> = LinkedList.emptyListItem<T>();

    public static emptyListItem<T> ():ILinkedListItem<T> {
        return <ILinkedListItem<T>>({ prev:null, value:null, next:null });
    }

    public static newItem<T> (prev:ILinkedListItem<T>, next:ILinkedListItem<T>, value: T):ILinkedListItem<T> {
        return <ILinkedListItem<T>>({ prev: prev, next: next, value: value });
    }

    constructor () {
        this.tail.prev = this.head;
        this.head.next = this.tail;
    }

    public forEach(callback: (item: T, index:number, list:LinkedList<T>) => void, thisArg:any = null): void {
        var currentItem = this.head.next;
        var counter = 0;
        while(currentItem !== this.tail) {
            callback.call(thisArg, currentItem.value, counter, this);
            counter++;
            currentItem = currentItem.next;
        }
    }

    public isEmpty():boolean {
        return this.head.next === this.tail;
    }

    public push(value:T): void {
        this.addAfter(value, this.tail.prev);
    }

    public pop(): T{
        var currentItem = this.tail.prev;
        if(this.isEmpty()) {
            throw new Error(`The linked list is empty.`);
        }
        this.removeItem(currentItem);
        return currentItem.value;
    }

    public remove(value:T): void{
        var currentItem = this.search(value);
        if (currentItem) {
            this.removeItem(currentItem);
        } else {
            throw new Error(`Cannot remove the value ${value}, it's not present in the linked list.`);
        }
    }

    public shift(): T{
        var item = this.head.next;
        if(this.isEmpty()) {
            throw new Error(`The linked list is empty.`);
        }
        this.removeItem(item);
        return item.value;
    }

    public unshift(value:T): void{
        this.addAfter(value, this.head);
    }

    private addAfter(value:T, itemAfter:ILinkedListItem<T>): void{
        var newItem = LinkedList.newItem(itemAfter, itemAfter.next, value);
        itemAfter.next.prev = newItem;
        itemAfter.next = newItem;
        this.length++;
    }

    private removeItem(item:ILinkedListItem<T>): void{
        item.prev.next = item.next;
        item.next.prev = item.prev;
        this.length--;
    }

    private search(value:T): ILinkedListItem<T>{
        var currentItem = this.head.next;
        while(currentItem !== this.tail) {
            if (value === currentItem.value) {
                return currentItem;
            }
            currentItem = currentItem.next;
        }
        return null;
    }
}