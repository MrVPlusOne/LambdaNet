export interface ILinkedList<T> {
    forEach:(callback: (item: T, index:number, list:ILinkedList<T>) => void, thisArg:any) => void;
    isEmpty: () => boolean;
    push: (value:T) => void;
    pop: () => T;
    remove: (value:T) => void;
    shift: () => T;
    unshift: (value:T) => void;
}