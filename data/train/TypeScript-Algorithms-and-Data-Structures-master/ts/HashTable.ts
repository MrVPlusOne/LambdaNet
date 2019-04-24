import {IHashTable} from "./Interfaces/IHashTable";
import {IKeyValuePair} from "./Interfaces/IKeyValuePair";
import {Utils} from "./Utils";
export class HashTable<K, V> implements IHashTable<K, V> {
    private capacity: number = 0;
    private buckets: IKeyValuePair<K, V>[][] = [];
    private hash: (value: K) => number;
    private equalKey: (value1: K, value2: K) => boolean = (value1: K, value2: K) => value1 === value2;
    private equalValue: (value1: V, value2: V) => boolean = (value1: V, value2: V) => value1 === value2;

    constructor(capacity: number, hash: (value: K) => number) {
        this.capacity = capacity;
        this.hash = hash;
        for (let i = 0; i < this.capacity; i++) {
            this.buckets.push([]);
        }
    }

    public clear(): void {
        for (let i = 0; i < this.capacity; i++) {
            this.buckets.push([]);
        }
    }

    public clone(): IHashTable<K, V> {
        const copy: IHashTable<K, V> = new HashTable<K, V>(this.capacity, this.hash);
        copy.putAll(this.elements());
        return copy;
    }

    public contains(value: V): boolean {
        return this.buckets.some(bucket => bucket.some(entry => this.equalValue(entry.value, value)));
    }

    public containsKey(key: K): boolean {
        const bucket: IKeyValuePair<K, V>[] = this.getBucket(key);
        if (bucket.length === 0) {
            return false;
        }
        return bucket.some(entry => this.equalKey(entry.key, key));
    }

    public containsValue(value: V): boolean {
        return this.buckets.some(bucket => bucket.some(entry => this.equalValue(entry.value, value)));
    }

    public elements(): IKeyValuePair<K, V>[] {
        return this.buckets.reduce((pv, cv) => pv.concat(cv), [ ]);
    }

    public get(key: K): V {
        const bucket: IKeyValuePair<K, V>[] = this.getBucket(key);
        let value: V = null;
        bucket.some(entry => {
            if (this.equalKey(entry.key, key)) {
                value = entry.value;
                return true;
            }
            return false;
        });
        return value;
    }

    public put(key: K, value: V) {
        const bucket: IKeyValuePair<K, V>[] = this.getBucket(key);
        bucket.push({key: key, value: value});
    }

    public putAll(elements: IKeyValuePair<K, V>[]) {
        elements.forEach(el => this.put(el.key, el.value));
    }

    public remove(key: K): V {
        const bucket: IKeyValuePair<K, V>[] = this.getBucket(key);
        const keyIndex: number = Utils.findIndexBy(bucket, (el) => this.equalKey(el.key, key));
        if (keyIndex === -1) {
            return null;
        } else {
            return bucket.splice(keyIndex, 1)[0].value;
        }
    }

    public size(): number {
        return this.buckets.reduce((total, bucket) => total + bucket.length, 0);
    }

    public values(): V[] {
        return this.elements().map(e => e.value);
    }

    private getBucket(key: K): IKeyValuePair<K, V>[] {
        let index = this.hash(key);
        if (index < 0) {
            index = Math.abs(index);
        }
        return this.buckets[index < this.buckets.length ? index : index % this.buckets.length];
    }
}

export  default HashTable;