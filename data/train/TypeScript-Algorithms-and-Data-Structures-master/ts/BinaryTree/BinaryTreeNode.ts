import {IBinaryTreeNode} from "../Interfaces/IBinaryTreeNode";
export class BinaryTreeNode<T> implements IBinaryTreeNode<T>{
    public left:BinaryTreeNode<T> = null;
    public right:BinaryTreeNode<T> = null;
    public value:T;
    public parent:BinaryTreeNode<T> = null;

    constructor(value:T, parent: BinaryTreeNode<T>) {
        this.value = value;
        this.parent = parent;
    }

    public hasLeftChild ():boolean {
        return this.left !== null;
    }

    public hasRightChild ():boolean {
        return this.right !== null;
    }

    public isLeftChild ():boolean {
        return this.parent && this.parent.left === this;
    }

    public isRightChild ():boolean {
        return this.parent && this.parent.right === this;
    }

    public isRoot ():boolean {
        return this.parent === null;
    }

    public min ():BinaryTreeNode<T> {
        if (this.left !== null) {
            return this.left.min();
        }
        return this;
    }

    public max ():BinaryTreeNode<T> {
        if (this.right !== null) {
            return this.right.max();
        }
        return this;
    }
}