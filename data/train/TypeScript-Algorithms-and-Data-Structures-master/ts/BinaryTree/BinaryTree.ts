import IBinaryTree from "../Interfaces/IBinaryTree";
import {IBinaryTreeNode} from "../Interfaces/IBinaryTreeNode";
import {BinaryTreeNode} from "./BinaryTreeNode";
export class BinaryTree<T> implements IBinaryTree<T, IBinaryTreeNode<T>> {

    public comparator: (a: T, b: T) => boolean;
    public root: IBinaryTreeNode<T> = null;

    constructor(comparator: (a: T, b: T) => boolean = (a, b) => a > b) {
        this.comparator = comparator;
    }

    public add(value: T): void {
        let currentNode: IBinaryTreeNode<T> = this.root;
        let futureParent: IBinaryTreeNode<T> = null;
        const newNode: IBinaryTreeNode<T> = new BinaryTreeNode(value, null);
        while (currentNode !== null) {
            futureParent = currentNode;
            if (futureParent !== null) {
                if (this.comparator(futureParent.value, value)) {
                    currentNode = futureParent.left;
                } else {
                    currentNode = futureParent.right;
                }
            }
        }

        newNode.parent = futureParent;
        if (futureParent === null) {
            this.root = newNode;
        } else if (this.comparator(futureParent.value, value)) {
            futureParent.left = newNode;
        } else {
            futureParent.right = newNode;
        }
    }

    public remove(value: T) {
        let node: IBinaryTreeNode<T> = this.search(value);
        if (node === null) {
            return false;
        }

        if (!node.hasLeftChild()) {
            this.transplant(node, node.right);
        } else if (!node.hasRightChild()) {
            this.transplant(node, node.left);
        } else {
            let successor = node.right.min();
            if (successor !== node.right) {
                this.transplant(successor, successor.right);
                successor.right = node.right;
                successor.right.parent = successor;
            }
            this.transplant(node, successor);
        }
    }

    public inOrderTreeWalk(callback: (pv: any, cv: T) => any, initialValue: any) {
        return this.inorderNodeWalk(this.root, callback, initialValue);
    }

    public isEmpty() {
        return this.root === null;
    }

    public max(): IBinaryTreeNode<T> {
        const currentNode: IBinaryTreeNode<T> = this.root;
        if (this.isEmpty()) {
            return null;
        }
        return currentNode.max();
    }

    public min(): IBinaryTreeNode<T> {
        const currentNode: IBinaryTreeNode<T> = this.root;
        if (this.isEmpty()) {
             return null;
        }
        return currentNode.min();
    }

    public reverseTreeWalk(callback: (pv: any, cv: T) => any, initialValue: any) {
        return this.reverseNodeWalk(this.root, callback, initialValue);
    }

    public search(value: T) {
        let currentNode: IBinaryTreeNode<T> = this.root;
        while (currentNode && currentNode.value !== value) {
            if (this.comparator(currentNode.value, value)) {
                currentNode = currentNode.left;
            } else {
                currentNode = currentNode.right;
            }
        }

        return currentNode;
    }

    public successor (value: T) {
        let node = this.search(value);
        let ancestor: IBinaryTreeNode<T>;
        if (node === null || value === null) {
            return null;
        }
        if (node.right !== null) {
            return node.right.min();
        }
        ancestor = node.parent;
        while (ancestor !== null && ancestor.right === node) {
            node = ancestor;
            ancestor = node.parent;
        }
        return ancestor;
    }

    private inorderNodeWalk(node: IBinaryTreeNode<T>, callback: (pv: any, cv: T) => any, previousValue: any) {
        if (node !== null) {
            previousValue = this.inorderNodeWalk(node.left, callback, previousValue);
            previousValue = callback(previousValue, node.value);
            previousValue = this.inorderNodeWalk(node.right, callback, previousValue);
            return previousValue;
        } else {
            return previousValue;
        }
    }

    private reverseNodeWalk(node: IBinaryTreeNode<T>, callback: (pv: any, cv: T) => any, previousValue: any) {
        if (node !== null) {
            previousValue = this.reverseNodeWalk(node.right, callback, previousValue);
            previousValue = callback(previousValue, node.value);
            previousValue = this.reverseNodeWalk(node.left, callback, previousValue);
            return previousValue;
        } else {
            return previousValue;
        }
    }

    private transplant(node: IBinaryTreeNode<T>, newNode: IBinaryTreeNode<T>) {
        if (node.isRoot()) {
            this.root = newNode;
        } else if (node.isLeftChild()) {
            node.parent.left = newNode;
        } else {
            node.parent.right = newNode;
        }
        if (newNode !== null) {
            newNode.parent = node.parent;
        }
    }
}