import {TrieWithValue} from "../ts/TrieWithValue";

describe("Trie", function () {
    it("should be able to check if word exist", function () {
        const trie = new TrieWithValue();
        expect(trie.contains("test")).toEqual(false);
        trie.insert("test");
        expect(trie.contains("test")).toEqual(true);
        expect(trie.contains("test for non existing word")).toEqual(false);
        trie.insert("test with value", 20);
        expect(trie.contains("test with value")).toEqual(true);
    });

    it("should be able to add word", function () {
        const trie = new TrieWithValue();
        trie.insert("test");
        expect(trie.contains("test")).toEqual(true);
    });

    it("should be able to add word with value", function () {
        const trie = new TrieWithValue();
        const value: number = 23;
        trie.insert("test", value);
        trie.insert("tester", value + 3);
        expect(trie.contains("test")).toEqual(true);
        expect(trie.getValue("test")).toEqual(value);
        expect(trie.getValue("tester")).toEqual(value + 3);
    });
});
