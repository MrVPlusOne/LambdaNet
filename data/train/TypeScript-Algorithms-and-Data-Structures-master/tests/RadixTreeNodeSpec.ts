import {RadixTreeNode} from "../ts/RadixTreeNode";

describe("RadixTreeNode", function () {
    it("should be able to search empty node", function () {
        const node: RadixTreeNode<number> = new RadixTreeNode("", null);
        expect(node.selectNextNodeFromPrefix("test")).toBeNull();
        expect(node.selectNextNodeFromPrefix("")).toBeNull();
    });

    it("should be able to find empty node by prefix", function () {
        const node: RadixTreeNode<number> = new RadixTreeNode("", null);
        node.addChild(new RadixTreeNode("", 1));
        expect(node.selectNextNodeFromPrefix("").value).toEqual(1);
    });

    it("should be able to find existing exact node by prefix", function () {
        const node: RadixTreeNode<number> = new RadixTreeNode("", null);
        node.addChild(new RadixTreeNode("test", 1));
        expect(node.selectNextNodeFromPrefix("test").value).toEqual(1);
    });

    it("should be able to find existing partial node by prefix", function () {
        const node: RadixTreeNode<number> = new RadixTreeNode("", null);
        node.addChild(new RadixTreeNode("tes", 1));
        expect(node.selectNextNodeFromPrefix("test").value).toEqual(1);
    });

    it("should be able to find existing exact node by prefix when there is an empty node before it", function () {
        const node: RadixTreeNode<number> = new RadixTreeNode("", null);
        node.addChild(new RadixTreeNode("", 2));
        node.addChild(new RadixTreeNode("test", 1));
        expect(node.selectNextNodeFromPrefix("test").value).toEqual(1);
    });

    it("should be able to find existing exact node by prefix when there is an empty node after it", function () {
        const node: RadixTreeNode<number> = new RadixTreeNode("", null);
        node.addChild(new RadixTreeNode("test", 1));
        node.addChild(new RadixTreeNode("", 2));
        expect(node.selectNextNodeFromPrefix("test").value).toEqual(1);
    });

    it("should be able to find not exact match by prefix", function () {
        const node: RadixTreeNode<number> = new RadixTreeNode("", null);
        node.addChild(new RadixTreeNode("test", 1));
        node.addChild(new RadixTreeNode("", 2));
        expect(node.selectNextNodeFromPrefix("tester").value).toEqual(1);
    });

    it("should return null if no node is found by prefix", function () {
        const node: RadixTreeNode<number> = new RadixTreeNode("", null);
        node.addChild(new RadixTreeNode("test", 1));
        node.addChild(new RadixTreeNode("", 2));
        expect(node.selectNextNodeFromPrefix("tes")).toBeNull();
    });

    describe("getCommonPrefix", function () {
        it("should return empty string if no common prefix", function () {
            const node: RadixTreeNode<number> = new RadixTreeNode("test", null);
            expect(node.getCommonPrefix("ars")).toEqual("");
        });
        it("should return common prefix", function () {
            const node: RadixTreeNode<number> = new RadixTreeNode("test", null);
            expect(node.getCommonPrefix("tes")).toEqual("tes");
            expect(node.getCommonPrefix("ter")).toEqual("te");
        });
        it("should return empty string if looking for empty string", function () {
            const node: RadixTreeNode<number> = new RadixTreeNode("test", null);
            expect(node.getCommonPrefix("")).toEqual("");
        });
    });
});