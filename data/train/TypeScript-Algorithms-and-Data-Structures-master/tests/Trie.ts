import {Trie} from "../ts/Trie";

describe("Trie", function () {
    it("should be able to check if word exist", function () {
        const trie = new Trie();
        expect(trie.contains("test")).toEqual(false);
        trie.insert("test");
        expect(trie.contains("test")).toEqual(true);
        expect(trie.contains("test for non existing word")).toEqual(false);
        trie.insert("test with value");
        expect(trie.contains("test with value")).toEqual(true);
    });

    it("should be able to add word", function () {
        const trie = new Trie();
        expect(trie.contains("test")).toEqual(false);
        trie.insert("test");
        expect(trie.contains("test")).toEqual(true);
    });
});
