import {IKeyValuePair} from "./IKeyValuePair";

export interface IHashTable<K, V> {
    clear(): void;
    clone(): IHashTable<K, V>;
    contains(value: V): boolean;
    containsKey(value: K): boolean;
    containsValue(value: V): boolean;
    elements(): IKeyValuePair<K, V>[];
    get(key: K): V;
    put(key: K, value: V): void;
    putAll(elements: IKeyValuePair<K, V>[]): void;
    remove(key: K): V;
    size(): number;
    values(): V[];
}