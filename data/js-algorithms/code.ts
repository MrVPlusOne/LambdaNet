class BloomFilter {
    size: number;
    storage;
    constructor(size = 100) {
        // Bloom filter size directly affects the likelihood of false positives.
        // The bigger the size the lower the likelihood of false positives.
        this.size = size;
        this.storage = this.createStore(size);
    }

    insert(item: string) {
        const hashValues = this.getHashValues(item);

        // Set each hashValue index to true.
        hashValues.forEach(val => this.storage.setValue(val));
    }


    mayContain(item: string): boolean {
        const hashValues = this.getHashValues(item);

        for (let hashIndex = 0; hashIndex < hashValues.length; hashIndex += 1) {
            if (!this.storage.getValue(hashValues[hashIndex])) {
                // We know that the item was definitely not inserted.
                return false;
            }
        }

        // The item may or may not have been inserted.
        return true;
    }

    createStore(size: number): object {
        const storage = [];

        // Initialize all indexes to false
        for (let storageCellIndex = 0; storageCellIndex < size; storageCellIndex += 1) {
            storage.push(false);
        }

        const storageInterface = {
            getValue(index) {
                return storage[index];
            },
            setValue(index) {
                storage[index] = true;
            },
        };

        return storageInterface;
    }

    hash1(item: string): number {
        let hash = 0;

        for (let charIndex = 0; charIndex < item.length; charIndex += 1) {
            const char = item.charCodeAt(charIndex);
            hash = (hash << 5) + hash + char;
            hash &= hash; // Convert to 32bit integer
            hash = Math.abs(hash);
        }

        return hash % this.size;
    }

    hash2(item: string): number {
        let hash = 5381;

        for (let charIndex = 0; charIndex < item.length; charIndex += 1) {
            const char = item.charCodeAt(charIndex);
            hash = (hash << 5) + hash + char; /* hash * 33 + c */
        }

        return Math.abs(hash % this.size);
    }

    hash3(item: string): number {
        let hash = 0;

        for (let charIndex = 0; charIndex < item.length; charIndex += 1) {
            const char = item.charCodeAt(charIndex);
            hash = (hash << 5) - hash;
            hash += char;
            hash &= hash; // Convert to 32bit integer
        }

        return Math.abs(hash % this.size);
    }

    getHashValues(item: string): number[] {
        return [
            this.hash1(item),
            this.hash2(item),
            this.hash3(item),
        ];
    }
}

class Comparator {
    compare: Function;

    constructor(compareFunction) {
        this.compare = compareFunction || Comparator.defaultCompareFunction;
    }


    static defaultCompareFunction(a, b): number {
        if (a === b) {
            return 0;
        }

        return a < b ? -1 : 1;
    }


    equal(a, b): boolean {
        return this.compare(a, b) === 0;
    }

    lessThan(a, b): boolean {
        return this.compare(a, b) < 0;
    }


    greaterThan(a, b): boolean {
        return this.compare(a, b) > 0;
    }

    lessThanOrEqual(a, b): boolean {
        return this.lessThan(a, b) || this.equal(a, b);
    }

    greaterThanOrEqual(a, b) {
        return this.greaterThan(a, b) || this.equal(a, b);
    }

    /**
     * Reverses the comparison order.
     */
    reverse() {
        const compareOriginal = this.compare;
        this.compare = (a, b) => compareOriginal(b, a);
    }
}



class LinkedListNode {
    value;
    next;
    constructor(value, next = null) {
        this.value = value;
        this.next = next;
    }

    toString(callback) {
        return callback ? callback(this.value) : "";
    }
}

class LinkedList {
    head: LinkedListNode;
    tail: LinkedListNode;
    compare: Comparator;
    constructor(comparatorFunction: Function) {
        this.head = null;

        this.tail = null;

        this.compare = new Comparator(comparatorFunction);
    }


    prepend(value): LinkedList {
        // Make new node to be a head.
        const newNode = new LinkedListNode(value, this.head);
        this.head = newNode;

        // If there is no tail yet let's make new node a tail.
        if (!this.tail) {
            this.tail = newNode;
        }

        return this;
    }

    append(value): LinkedList {
        const newNode = new LinkedListNode(value);

        // If there is no head yet let's make new node a head.
        if (!this.head) {
            this.head = newNode;
            this.tail = newNode;

            return this;
        }

        // Attach new node to the end of linked list.
        this.tail.next = newNode;
        this.tail = newNode;

        return this;
    }

    delete(value): LinkedListNode {
        if (!this.head) {
            return null;
        }

        let deletedNode = null;

        // If the head must be deleted then make next node that is differ
        // from the head to be a new head.
        while (this.head && this.compare.equal(this.head.value, value)) {
            deletedNode = this.head;
            this.head = this.head.next;
        }

        let currentNode = this.head;

        if (currentNode !== null) {
            // If next node must be deleted then make next node to be a next next one.
            while (currentNode.next) {
                if (this.compare.equal(currentNode.next.value, value)) {
                    deletedNode = currentNode.next;
                    currentNode.next = currentNode.next.next;
                } else {
                    currentNode = currentNode.next;
                }
            }
        }

        // Check if tail must be deleted.
        if (this.compare.equal(this.tail.value, value)) {
            this.tail = currentNode;
        }

        return deletedNode;
    }


    find({ value = undefined, callback = undefined }): LinkedListNode {
        if (!this.head) {
            return null;
        }

        let currentNode = this.head;

        while (currentNode) {
            // If callback is specified then try to find node by callback.
            if (callback && callback(currentNode.value)) {
                return currentNode;
            }

            // If value is specified then try to compare by value..
            if (value !== undefined && this.compare.equal(currentNode.value, value)) {
                return currentNode;
            }

            currentNode = currentNode.next;
        }

        return null;
    }

    deleteTail(): LinkedListNode {
        const deletedTail = this.tail;

        if (this.head === this.tail) {
            // There is only one node in linked list.
            this.head = null;
            this.tail = null;

            return deletedTail;
        }

        // If there are many nodes in linked list...

        // Rewind to the last node and delete "next" link for the node before the last one.
        let currentNode = this.head;
        while (currentNode.next) {
            if (!currentNode.next.next) {
                currentNode.next = null;
            } else {
                currentNode = currentNode.next;
            }
        }

        this.tail = currentNode;

        return deletedTail;
    }

    deleteHead(): LinkedListNode {
        if (!this.head) {
            return null;
        }

        const deletedHead = this.head;

        if (this.head.next) {
            this.head = this.head.next;
        } else {
            this.head = null;
            this.tail = null;
        }

        return deletedHead;
    }


    fromArray(values: any[]): LinkedList {
        values.forEach(value => this.append(value));

        return this;
    }

    toArray(): LinkedListNode[] {
        const nodes = [];

        let currentNode = this.head;
        while (currentNode) {
            nodes.push(currentNode);
            currentNode = currentNode.next;
        }

        return nodes;
    }

    toString(callback: Function): string {
        return this.toArray().map(node => node.toString(callback)).toString();
    }

    reverse(): LinkedList {
        let currNode = this.head;
        let prevNode = null;
        let nextNode = null;

        while (currNode) {
            // Store next node.
            nextNode = currNode.next;

            // Change next node of the current node so it would link to previous node.
            currNode.next = prevNode;

            // Move prevNode and currNode nodes one step forward.
            prevNode = currNode;
            currNode = nextNode;
        }

        // Reset head and tail.
        this.tail = this.head;
        this.head = prevNode;

        return this;
    }
}

class Queue {
    linkedList: LinkedList;
    constructor() {
        // We're going to implement Queue based on LinkedList since the two
        // structures are quite similar. Namely, they both operate mostly on
        // the elements at the beginning and the end. Compare enqueue/dequeue
        // operations of Queue with append/deleteHead operations of LinkedList.
        this.linkedList = new LinkedList();
    }


    isEmpty(): boolean {
        return !this.linkedList.head;
    }

    peek() {
        if (!this.linkedList.head) {
            return null;
        }

        return this.linkedList.head.value;
    }

    enqueue(value) {
        this.linkedList.append(value);
    }

    dequeue() {
        const removedHead = this.linkedList.deleteHead();
        return removedHead ? removedHead.value : null;
    }

    toString(callback: Function): string {
        // Return string representation of the queue's linked list.
        return this.linkedList.toString(callback);
    }
}


class GraphEdge {

    startVertex: GraphVertex;
    endVertex: GraphVertex;
    weight: number;

    constructor(startVertex, endVertex, weight = 0) {
        this.startVertex = startVertex;
        this.endVertex = endVertex;
        this.weight = weight;
    }

    getKey(): string {
        const startVertexKey = this.startVertex.getKey();
        const endVertexKey = this.endVertex.getKey();

        return `${startVertexKey}_${endVertexKey}`;
    }

    reverse(): GraphEdge {
        const tmp = this.startVertex;
        this.startVertex = this.endVertex;
        this.endVertex = tmp;

        return this;
    }

    toString(): string {
        return this.getKey();
    }
}

class GraphVertex {
    value;
    edges;

    constructor(value) {
        if (value === undefined) {
            throw new Error('Graph vertex must have a value');
        }

        const edgeComparator = (edgeA: GraphEdge, edgeB: GraphEdge) => {
            if (edgeA.getKey() === edgeB.getKey()) {
                return 0;
            }

            return edgeA.getKey() < edgeB.getKey() ? -1 : 1;
        };

        // Normally you would store string value like vertex name.
        // But generally it may be any object as well
        this.value = value;
        this.edges = new LinkedList(edgeComparator);
    }

    addEdge(edge: GraphEdge): GraphVertex {
        this.edges.append(edge);

        return this;
    }

    deleteEdge(edge: GraphEdge) {
        this.edges.delete(edge);
    }

    getNeighbors(): GraphVertex[] {
        const edges = this.edges.toArray();

        const neighborsConverter = (node: LinkedListNode) => {
            return node.value.startVertex === this ? node.value.endVertex : node.value.startVertex;
        };

        // Return either start or end vertex.
        // For undirected graphs it is possible that current vertex will be the end one.
        return edges.map(neighborsConverter);
    }


    getEdges(): GraphEdge[] {
        return this.edges.toArray().map(linkedListNode => linkedListNode.value);
    }

    getDegree(): number {
        return this.edges.toArray().length;
    }

    hasEdge(requiredEdge: GraphEdge): boolean {
        const edgeNode = this.edges.find({
            callback: edge => edge === requiredEdge,
        });

        return !!edgeNode;
    }

    hasNeighbor(vertex: GraphVertex):boolean {
        const vertexNode = this.edges.find({
            callback: edge => edge.startVertex === vertex || edge.endVertex === vertex,
        });

        return !!vertexNode;
    }

    findEdge(vertex: GraphVertex): GraphEdge|null {
        const edgeFinder = (edge) => {
            return edge.startVertex === vertex || edge.endVertex === vertex;
        };

        const edge = this.edges.find({ callback: edgeFinder });

        return edge ? edge.value : null;
    }


    getKey(): string {
        return this.value;
    }

    deleteAllEdges(): GraphVertex {
        this.getEdges().forEach(edge => this.deleteEdge(edge));

        return this;
    }

    toString(callback: Function): string {
        return callback ? callback(this.value) : `${this.value}`;
    }
}

class Graph {
    vertices;
    edges;
    isDirected: boolean;

    constructor(isDirected = false) {
        this.vertices = {};
        this.edges = {};
        this.isDirected = isDirected;
    }

    addVertex(newVertex: GraphVertex): Graph {
        this.vertices[newVertex.getKey()] = newVertex;

        return this;
    }


    getVertexByKey(vertexKey: string): GraphVertex {
        return this.vertices[vertexKey];
    }

    getNeighbors(vertex: GraphVertex): GraphVertex[] {
        return vertex.getNeighbors();
    }

    getAllVertices(): GraphVertex[] {
        return Object.values(this.vertices);
    }


    getAllEdges(): GraphEdge[] {
        return Object.values(this.edges);
    }


    addEdge(edge: GraphEdge): Graph {
        // Try to find and end start vertices.
        let startVertex = this.getVertexByKey(edge.startVertex.getKey());
        let endVertex = this.getVertexByKey(edge.endVertex.getKey());

        // Insert start vertex if it wasn't inserted.
        if (!startVertex) {
            this.addVertex(edge.startVertex);
            startVertex = this.getVertexByKey(edge.startVertex.getKey());
        }

        // Insert end vertex if it wasn't inserted.
        if (!endVertex) {
            this.addVertex(edge.endVertex);
            endVertex = this.getVertexByKey(edge.endVertex.getKey());
        }

        // Check if edge has been already added.
        if (this.edges[edge.getKey()]) {
            throw new Error('Edge has already been added before');
        } else {
            this.edges[edge.getKey()] = edge;
        }

        // Add edge to the vertices.
        if (this.isDirected) {
            // If graph IS directed then add the edge only to start vertex.
            startVertex.addEdge(edge);
        } else {
            // If graph ISN'T directed then add the edge to both vertices.
            startVertex.addEdge(edge);
            endVertex.addEdge(edge);
        }

        return this;
    }

    deleteEdge(edge: GraphEdge) {
        // Delete edge from the list of edges.
        if (this.edges[edge.getKey()]) {
            delete this.edges[edge.getKey()];
        } else {
            throw new Error('Edge not found in graph');
        }

        // Try to find and end start vertices and delete edge from them.
        const startVertex = this.getVertexByKey(edge.startVertex.getKey());
        const endVertex = this.getVertexByKey(edge.endVertex.getKey());

        startVertex.deleteEdge(edge);
        endVertex.deleteEdge(edge);
    }


    findEdge(startVertex: GraphVertex, endVertex: GraphVertex): GraphEdge | null {
        const vertex = this.getVertexByKey(startVertex.getKey());

        if (!vertex) {
            return null;
        }

        return vertex.findEdge(endVertex);
    }


    getWeight(): number {
        return this.getAllEdges().reduce((weight, graphEdge) => {
            return weight + graphEdge.weight;
        }, 0);
    }

    reverse(): Graph {
        /** @param {GraphEdge} edge */
        this.getAllEdges().forEach((edge) => {
            // Delete straight edge from graph and from vertices.
            this.deleteEdge(edge);

            // Reverse the edge.
            edge.reverse();

            // Add reversed edge back to the graph and its vertices.
            this.addEdge(edge);
        });

        return this;
    }

    getVerticesIndices(): object {
        const verticesIndices = {};
        this.getAllVertices().forEach((vertex, index) => {
            verticesIndices[vertex.getKey()] = index;
        });

        return verticesIndices;
    }


    getAdjacencyMatrix() {
        const vertices = this.getAllVertices();
        const verticesIndices = this.getVerticesIndices();

        // Init matrix with infinities meaning that there is no ways of
        // getting from one vertex to another yet.
        const adjacencyMatrix = Array(vertices.length).fill(null).map(() => {
            return Array(vertices.length).fill(Infinity);
        });

        // Fill the columns.
        vertices.forEach((vertex, vertexIndex) => {
            vertex.getNeighbors().forEach((neighbor) => {
                const neighborIndex = verticesIndices[neighbor.getKey()];
                adjacencyMatrix[vertexIndex][neighborIndex] = this.findEdge(vertex, neighbor).weight;
            });
        });

        return adjacencyMatrix;
    }

    toString(): string {
        return Object.keys(this.vertices).toString();
    }
}



function breadthFirstSearch(graph: Graph, startVertex: GraphVertex, originalCallbacks) {
    const callbacks = initCallbacks(originalCallbacks);
    const vertexQueue = new Queue();

    // Do initial queue setup.
    vertexQueue.enqueue(startVertex);

    let previousVertex = null;

    // Traverse all vertices from the queue.
    while (!vertexQueue.isEmpty()) {
        const currentVertex = vertexQueue.dequeue();
        callbacks.enterVertex({ currentVertex, previousVertex });

        // Add all neighbors to the queue for future traversals.
        graph.getNeighbors(currentVertex).forEach((nextVertex) => {
            if (callbacks.allowTraversal({ previousVertex, currentVertex, nextVertex })) {
                vertexQueue.enqueue(nextVertex);
            }
        });

        callbacks.leaveVertex({ currentVertex, previousVertex });

        // Memorize current vertex before next loop.
        previousVertex = currentVertex;
    }
}

