export class RadixTreeNode<T> {
    public word: string;
    public value: T;
    public children: RadixTreeNode<T>[] = [];

    constructor (word: string, value: T) {
        this.word = word;
        this.value = value;
    }

    public addChild(child: RadixTreeNode<T>) {
        return this.children.push(child);
    }

    public isLeaf() {
        return this.value !== null;
    }

    public getNodeWithSharedPrefix(prefix: string): RadixTreeNode<T> {
        const partialMatch: RadixTreeNode<T>[] = this.children.filter(node => node.word[0] === prefix[0]);
        if (partialMatch.length === 0) {
            return null;
        }
        return partialMatch[0]
    }

    public getCommonPrefix(prefix: string): string {
        let i = 0;
        let commonPrefix = "";
        while (prefix[i] === this.word[i]) {
            commonPrefix += prefix[i];
            i++;
        }
        return commonPrefix;
    }

    public splitNode(sufix: string): void {
        const commonPrefix: string = this.getCommonPrefix(sufix);
        const currentChildren = this.children.slice(0);
        const newNode = new RadixTreeNode(this.word.substr(commonPrefix.length), this.value);
        currentChildren.forEach(newNode.addChild, newNode);
        this.word = commonPrefix;
        this.value = null;
        this.children = [newNode];
    }

    public selectNextNodeFromPrefix(prefix: string): RadixTreeNode<T> {
        const exactMatch: RadixTreeNode<T>[] = this.children.filter(node => node.word === prefix);
        if (exactMatch.length === 1) {
            return exactMatch[0];
        }
        const partialMatch: RadixTreeNode<T>[] = this.children.filter(node => node.word[0] === prefix[0]);
        if (partialMatch.length === 0) {
            return null;
        }
        const partialMatchWord: string = partialMatch[0].word;

        if (prefix.indexOf(partialMatchWord) === -1) {
            return null;
        }
        return partialMatch[0];
    }
}