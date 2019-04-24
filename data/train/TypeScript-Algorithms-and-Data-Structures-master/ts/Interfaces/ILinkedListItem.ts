export  interface ILinkedListItem<T> {
    next:ILinkedListItem<T>;
    prev:ILinkedListItem<T>;
    value:T;
}