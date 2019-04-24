import {IBinaryTreeNode} from "../Interfaces/IBinaryTreeNode";
class RedBlackTreeNode<T> implements IBinaryTreeNode<T> {
    public static sentinel: RedBlackTreeNode<any> = new RedBlackTreeNode(null, null, true);

    // Red: false, Black: true
    public color: boolean = false;
    public left: RedBlackTreeNode<T> = RedBlackTreeNode.sentinel;
    public right: RedBlackTreeNode<T> = RedBlackTreeNode.sentinel;
    public value: T;
    public parent: RedBlackTreeNode<T> = RedBlackTreeNode.sentinel;

    constructor(value: T, parent: RedBlackTreeNode<T>, color: boolean) {
        this.value = value;
        this.parent = parent;
        this.color = color;
    }

    public hasLeftChild (): boolean {
        return this.left !== RedBlackTreeNode.sentinel;
    }

    public isBlack (): boolean {
        return this.color === true;
    }

    public hasRightChild (): boolean {
        return this.right !== RedBlackTreeNode.sentinel;
    }

    public isLeftChild (): boolean {
        return this.parent && this.parent.left === this;
    }

    public isRed (): boolean {
        return this.color === false;
    }

    public isRightChild (): boolean {
        return this.parent && this.parent.right === this;
    }

    public isSentinel (): boolean {
        return this === RedBlackTreeNode.sentinel;
    }

    public isNotSentinel (): boolean {
        return this !== RedBlackTreeNode.sentinel;
    }

    public isRoot (): boolean {
        return this.parent === RedBlackTreeNode.sentinel;
    }

    public setRed (): void {
        this.color = false;
    }

    public setBlack (): void {
        this.color = true;
    }

    public min (): RedBlackTreeNode<T> {
        if (this.left !== RedBlackTreeNode.sentinel) {
            return this.left.min();
        }
        return this;
    }

    public max (): RedBlackTreeNode<T> {
        if (this.right !== RedBlackTreeNode.sentinel) {
            return this.right.max();
        }
        return this;
    }
}

export default RedBlackTreeNode;