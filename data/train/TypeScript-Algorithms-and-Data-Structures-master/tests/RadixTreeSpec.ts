import {RadixTree} from "../ts/RadixTree";

describe("Radix Tree", function () {
    it("should be able to add word that doesn't exist", function () {
        const tree: RadixTree<number> = new RadixTree<number>();
        tree.insert("test", 1);
        tree.insert("slow", 2);
        tree.insert("water", 3);
        expect(tree.contains("test")).toEqual(true);
        expect(tree.contains("slow")).toEqual(true);
        expect(tree.contains("water")).toEqual(true);
    });

    it("should be able to add word that extends existing word", function () {
        const tree: RadixTree<number> = new RadixTree<number>();
        tree.insert("test", 1);
        tree.insert("slow", 2);
        tree.insert("water", 3);
        tree.insert("slower", 4);
        tree.insert("tester", 5);
        tree.insert("team", 6);
        tree.insert("toast", 7);
        expect(tree.contains("test")).toEqual(true);
        expect(tree.contains("slow")).toEqual(true);
        expect(tree.contains("water")).toEqual(true);
        expect(tree.contains("slower")).toEqual(true);
        expect(tree.contains("tester")).toEqual(true);
        expect(tree.contains("toast")).toEqual(true);
        expect(tree.contains("team")).toEqual(true);
        expect(tree.contains("te")).toEqual(false);
    });

    it("should be able to add word that extends existing word", function () {
        const tree: RadixTree<number> = new RadixTree<number>();
        tree.insert("test", 1);
        tree.insert("slow", 2);
        tree.insert("water", 3);
        tree.insert("slower", 4);
        tree.insert("tester", 5);
        tree.insert("team", 6);
        tree.insert("toast", 7);
        expect(tree.get("test")).toEqual(1);
        expect(tree.get("slow")).toEqual(2);
        expect(tree.get("water")).toEqual(3);
        expect(tree.get("slower")).toEqual(4);
        expect(tree.get("tester")).toEqual(5);
        expect(tree.get("team")).toEqual(6);
        expect(tree.get("toast")).toEqual(7);
        expect(tree.get("te")).toEqual(null);
    });
});