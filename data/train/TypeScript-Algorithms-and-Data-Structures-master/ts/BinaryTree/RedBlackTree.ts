import IBinaryTree from "../Interfaces/IBinaryTree";
import RedBlackTreeNode from "./RedBlackTreeNode";
class RedBlackTree<T> implements IBinaryTree<T, RedBlackTreeNode<T>> {

    public comparator: (a: T, b: T) => boolean;
    public root: RedBlackTreeNode<T> = RedBlackTreeNode.sentinel;

    private sentinel: RedBlackTreeNode<T> = RedBlackTreeNode.sentinel;

    constructor(comparator: (a: T, b: T) => boolean = (a, b) => a > b) {
        this.comparator = comparator;
    }

    public add(value: T): void {
        let parent: RedBlackTreeNode<T> = this.sentinel;
        let currentLoopNode: RedBlackTreeNode<T> = this.root;
        const newNode: RedBlackTreeNode<T> = new RedBlackTreeNode(value, RedBlackTreeNode.sentinel, false);
        while (currentLoopNode.isNotSentinel()) {
            parent = currentLoopNode;
            if (this.comparator(currentLoopNode.value, newNode.value)) {
                currentLoopNode = currentLoopNode.left;
            } else {
                currentLoopNode = currentLoopNode.right;
            }
        }
        newNode.parent = parent;
        if (parent.isSentinel()) {
            this.root = newNode;
        } else if (this.comparator(parent.value, newNode.value)) {
            parent.left = newNode;
        } else {
            parent.right = newNode;
        }
        this.addFixup(newNode);
    }
    public remove(value: T): boolean {
        return false;
    }
    public inOrderTreeWalk(callback: (pv: any, cv: T) => any, initialValue: any): any {
        return this.inorderNodeWalk(this.root, callback, initialValue);
    }

    public isEmpty() {
        return this.root === RedBlackTreeNode.sentinel;
    }

    public max(): RedBlackTreeNode<T> {
        const currentNode: RedBlackTreeNode<T> = this.root;
        if (this.isEmpty()) {
            return null;
        }
        return currentNode.max();
    }

    public min(): RedBlackTreeNode<T> {
        const currentNode: RedBlackTreeNode<T> = this.root;
        if (this.isEmpty()) {
            return null;
        }
        return currentNode.min();
    }
    public reverseTreeWalk(callback: (pv: any, cv: T) => any, initialValue: any): any {
        return this.reverseNodeWalk(this.root, callback, initialValue);
    }

    public search(value: T): RedBlackTreeNode<T> {
        let currentNode: RedBlackTreeNode<T> = this.root;
        while (currentNode !== this.sentinel && currentNode.value !== value) {
            if (this.comparator(currentNode.value, value)) {
                currentNode = currentNode.left;
            } else {
                currentNode = currentNode.right;
            }
        }

        return currentNode === this.sentinel ? null : currentNode;
    }

    public successor (value: T) {
        let node = this.search(value);
        let ancestor: RedBlackTreeNode<T>;
        if (node === null || value === null) {
            return null;
        }
        if (node.right !== this.sentinel) {
            return node.right.min();
        }
        ancestor = node.parent;
        while (ancestor !== this.sentinel && ancestor.right === node) {
            node = ancestor;
            ancestor = node.parent;
        }
        return ancestor === this.sentinel ? null : ancestor;
    }

    private addFixup(node: RedBlackTreeNode<T>) {
        while (node.parent.isRed()) {
            if (node.parent.isLeftChild()) {
                let parentSibling = node.parent.parent.right;
                if (parentSibling.isRed()) {
                    node.parent.setBlack();
                    parentSibling.setBlack();
                    node.parent.parent.setRed();
                    node = node.parent.parent;
                } else {
                    if (node.isRightChild()) {
                        node = node.parent;
                        this.leftRotate(node);
                    }
                    node.parent.setBlack();
                    node.parent.parent.setRed();
                    this.rightRotate(node.parent.parent)
                }
            } else {
                let parentSibling = node.parent.parent.left;
                if (parentSibling.isRed()) {
                    node.parent.setBlack();
                    parentSibling.setBlack();
                    node.parent.parent.setRed();
                    node = node.parent.parent;
                } else {
                    if (node.isLeftChild()) {
                        node = node.parent;
                        this.rightRotate(node);
                    }
                    node.parent.setBlack();
                    node.parent.parent.setRed();
                    this.leftRotate(node.parent.parent)
                }
            }
        }
        this.root.setBlack();
    }

    private inorderNodeWalk(node: RedBlackTreeNode<T>, callback: (pv: any, cv: T) => any, previousValue: any) {
        if (node !== this.sentinel) {
            previousValue = this.inorderNodeWalk(node.left, callback, previousValue);
            previousValue = callback(previousValue, node.value);
            previousValue = this.inorderNodeWalk(node.right, callback, previousValue);
            return previousValue;
        } else {
            return previousValue;
        }
    }

    private leftRotate(node: RedBlackTreeNode<T>) {
        const promotedNode: RedBlackTreeNode<T> = node.right;
        node.right = promotedNode.left;
        if (promotedNode.left !== this.sentinel) {
            promotedNode.left.parent = node;
        }
        promotedNode.parent = node.parent;
        if (node.parent === this.sentinel) {
            this.root = promotedNode;
        } else if (node.isLeftChild()) {
            node.parent.left = promotedNode;
        } else {
            node.parent.right = promotedNode;
        }

        promotedNode.left = node;
        node.parent = promotedNode;
    }

    private reverseNodeWalk(node: RedBlackTreeNode<T>, callback: (pv: any, cv: T) => any, previousValue: any) {
        if (node !== this.sentinel) {
            previousValue = this.reverseNodeWalk(node.right, callback, previousValue);
            previousValue = callback(previousValue, node.value);
            previousValue = this.reverseNodeWalk(node.left, callback, previousValue);
            return previousValue;
        } else {
            return previousValue;
        }
    }

    private rightRotate(node: RedBlackTreeNode<T>) {
        const promotedNode: RedBlackTreeNode<T> = node.left;
        node.left = promotedNode.right;
        if (promotedNode.right !== this.sentinel) {
            promotedNode.right.parent = node;
        }
        promotedNode.parent = node.parent;
        if (node.parent === this.sentinel) {
            this.root = promotedNode;
        } else if (node.isRightChild()) {
            node.parent.right = promotedNode;
        } else {
            node.parent.left = promotedNode;
        }

        promotedNode.right = node;
        node.parent = promotedNode;
    }
}

export default RedBlackTree;