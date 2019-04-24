class Node<T> {
    constructor(public data: T, public next: Node<T> = null) {}
}

export default class LinkedList<T> {
    private head: Node<T> = null;
    public length = 0;

    addFirst(data:T) {
        this.head = new Node(data, this.head);
        this.length++;
    }

    addLast(data:T) {
        if (!this.head) {
            this.addFirst(data)
        } else {
            this.lastNode.next = new Node(data);
            this.length++;
        }
    }

    delete(index:number):void {
        if(index === 0) {
            this.head = this.head.next
        } else {
            const before = this.getNode(index - 1);
            before.next  = before.next.next;
        }
        this.length--;
    }

    get(index:number) {
        return this.getNode(index).data;
    }

    get first() {
        return this.get(0);
    }

    get last() {
        return this.get(this.length -1);
    }


    private get lastNode() {
        return this.getNode(this.length - 1);
    }

    private getNode(index:number) {

        if(index > this.length - 1) {
            throw new Error('Out of bounds');
        }

        let tmp = this.head, count = 0;
        while(tmp.next && index > count++) {
            tmp = tmp.next;
        }
        return tmp;
    }

    // *[Symbol.iterator]() {
    //
    //     if(!this.length) {
    //         return
    //     }
    //
    //     let tmp = this.head;
    //     yield tmp.data;
    //     while(tmp = tmp.next) {
    //         yield tmp.data;
    //     }
    // }

    static fromArray<T>(arr: Array<T>) {
        const list = new LinkedList<T>();
        for(let x of arr) {
            list.addLast(x);
        }
        return list;
    }
}

const list = LinkedList.fromArray<string>(['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L']);
for(let x of list) {
    console.log(x);
}