// TODO: add delete and getWordsFromPrefix

export class TrieWithValue<T> {
    public static KEY: string = "id";
    public root: any = {};

    public insert(word: string, value: T = null): boolean {
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
                level[TrieWithValue.KEY] =  value;
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
            if (i === wordwSize && level.hasOwnProperty(TrieWithValue.KEY)) {
                return true;
            }
        }
        return false;
    }

    public getValue = function(word: string) {
        const wordwSize = word.length;
        let i: number = 0;
        let level = this.root;
        while (i < wordwSize) {
            if (!level[word[i]]) {
                return void 0;
            }
            level = level[word[i]];
            i++;
            if (i === wordwSize && level.hasOwnProperty(TrieWithValue.KEY)) {
                return level[TrieWithValue.KEY];
            }
        }
        return void 0;
    }
}


