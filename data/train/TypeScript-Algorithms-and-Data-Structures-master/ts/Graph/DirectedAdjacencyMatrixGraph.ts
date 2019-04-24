import {IBitMatrix} from "../Interfaces/IBitMatrix";
import {BitMatrix} from "../BitMatrix";
import {Queue} from "../Queue";

export class DirectedAdjacencyMatrixGraph <T> {
    private hashFunction: (node: T) => string = (a: T) => a.toString();
    private vertices: {[name: string]: number} = {};
    private edgeToVertexMap: T[] = [];
    private edges: IBitMatrix = new BitMatrix(0, 0);

    constructor (hashFunction?: (node: T) => string) {
        this.hashFunction = hashFunction || this.hashFunction;
    }

    public outdegree(vertex: T): T[] {
        const vertexIndex: number = this.vertices[this.hashFunction(vertex)];
        return this.edges.getRowIndexes(vertexIndex).map(vertexIndex => this.edgeToVertexMap[vertexIndex]);
    }

    public adjacent (vertex: T): T[] {
        return this.outdegree(vertex);
    }

    public addNode (node: T): DirectedAdjacencyMatrixGraph<T> {
        this.edgeToVertexMap.push(node);
        const newNodeIndex: number = this.edges.size()[0];
        this.vertices[this.hashFunction(node)] = newNodeIndex;
        this.edges.resize(newNodeIndex + 1, newNodeIndex + 1);
        return this;
    }

    public addEdge (from: T, to: T): DirectedAdjacencyMatrixGraph<T> {
        const fromNodeIndex: number = this.vertices[this.hashFunction(from)];
        const toNodeIndex: number = this.vertices[this.hashFunction(to)];
        this.edges.set(fromNodeIndex, toNodeIndex, true);
        return this;
    }

    public indegree (vertex: T): T[] {
        const vertexIndex: number = this.vertices[this.hashFunction(vertex)];
        const indexes: number[] = this.edges.getColIndexes(vertexIndex);
        return indexes.map(index => this.edgeToVertexMap[index]);
    }

    public nodes (): T[] {
        return this.edgeToVertexMap.slice(0);
    }

    public removeEdge (from: T, to: T): DirectedAdjacencyMatrixGraph<T> {
        const fromNodeIndex: number = this.vertices[this.hashFunction(from)];
        const toNodeIndex: number = this.vertices[this.hashFunction(to)];
        this.edges.set(fromNodeIndex, toNodeIndex, false);
        return this;
    }

    public topologicalSort() {
        const sortedElements: T[] = [];
        const verticesCount: number = this.edges.size()[0];
        const edgesCopy: IBitMatrix = this.edges.clone();
        const verticesWithNoIncomingEdges: Queue<number> = new Queue<number>(verticesCount);
        edgesCopy.getIndexes(true)
            .map((column, index) => column.length === 0 ? index : -1)
            .filter(index => index !== -1)
            .forEach(index => verticesWithNoIncomingEdges.enqueue(index));

        while (verticesWithNoIncomingEdges.size() > 0) {
            const currentNodeIndex: number = verticesWithNoIncomingEdges.dequeue();
            sortedElements.push(this.edgeToVertexMap[currentNodeIndex]);
            edgesCopy.getRowIndexes(currentNodeIndex).forEach(index => {
                edgesCopy.set(currentNodeIndex, index, false);
                if (edgesCopy.getColIndexes(index).length === 0) {
                    verticesWithNoIncomingEdges.enqueue(index);
                }
            });
        }

        if (edgesCopy.count() > 0) {
            throw new Error(`The graph contains cycles.`);
        }
        return sortedElements;
    }
}