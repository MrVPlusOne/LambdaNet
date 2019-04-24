import {DirectedAdjacencyMatrixGraph} from "../../ts/Graph/DirectedAdjacencyMatrixGraph";

describe("DirectedAdjacencyMatrixGraph", function () {
    it("should be able to add nodes", function () {
        const graph = new DirectedAdjacencyMatrixGraph();
        graph.addNode("test");
        graph.addNode("test 2");
        graph.addNode("test 3");
        expect(graph.nodes()).toEqual(["test", "test 2", "test 3"])
    });

    it("should be able to get adjacent nodes", function () {
        const graph = new DirectedAdjacencyMatrixGraph();
        graph.addNode("a");
        graph.addNode("b");
        graph.addNode("c");
        graph.addNode("d");
        graph.addEdge("b", "a");
        graph.addEdge("b", "c");
        graph.addEdge("d", "b");
        expect(graph.adjacent("b")).toEqual(["a", "c"]);
        expect(graph.outdegree("b")).toEqual(["a", "c"]);
    });

    it("should be able to get indegree nodes", function () {
        const graph = new DirectedAdjacencyMatrixGraph();
        graph.addNode("a");
        graph.addNode("b");
        graph.addNode("c");
        graph.addNode("d");
        graph.addEdge("b", "a");
        graph.addEdge("b", "c");
        graph.addEdge("d", "b");
        expect(graph.indegree("a")).toEqual(["b"]);
        expect(graph.indegree("b")).toEqual(["d"]);
        expect(graph.indegree("c")).toEqual(["b"]);
        expect(graph.indegree("d")).toEqual([]);
    });
    it("should be able to remove edge", function () {
        const graph = new DirectedAdjacencyMatrixGraph();
        graph.addNode("a");
        graph.addNode("b");
        graph.addEdge("b", "a");
        expect(graph.indegree("a")).toEqual(["b"]);
        expect(graph.outdegree("b")).toEqual(["a"]);
        graph.removeEdge("b", "a");
        expect(graph.indegree("a")).toEqual([]);
        expect(graph.outdegree("b")).toEqual([]);
    });
    describe("topologicalSort", function () {
        it("topologicalSort", function () {
            const graph = new DirectedAdjacencyMatrixGraph();
            graph.addNode("5");
            graph.addNode("7");
            graph.addNode("3");
            graph.addNode("11");
            graph.addNode("8");
            graph.addNode("2");
            graph.addNode("9");
            graph.addNode("10");
            graph.addEdge("5", "11");
            graph.addEdge("7", "11");
            graph.addEdge("7", "8");
            graph.addEdge("3", "8");
            graph.addEdge("3", "10");
            graph.addEdge("11", "2");
            graph.addEdge("11", "9");
            graph.addEdge("11", "10");
            graph.addEdge("8", "9");
            expect(graph.topologicalSort()).toEqual(["5", "7", "3", "11", "8", "2", "10", "9"]);
        });
    });
});