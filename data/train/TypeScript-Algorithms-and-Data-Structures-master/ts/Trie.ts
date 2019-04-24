// TODO: getWordsFromPrefix

export class Trie<T> {
    public static KEY: string = "id";
    public root: any = {};

    public insert(word: string): boolean {
        const wordwSize = word.length;
        let i: number = 0;
        let level = this.root;
        while (i < wordwSize) {
            if (!level[word[i]]) {
                level[word[i]] = {};
            }
            level = level[word[i]];
            i++;
            if (i === wordwSize) {
                level[Trie.KEY] =  true;
            }
        }
        return true;
    }

    public contains(word: string) {
        const wordwSize = word.length;
        let i: number = 0;
        let level = this.root;
        while (i < wordwSize) {
            if (!level[word[i]]) {
                return false;
            }
            level = level[word[i]];
            i++;
            if (i === wordwSize && level[Trie.KEY]) {
                return true;
            }
        }
        return false;
    }
}


