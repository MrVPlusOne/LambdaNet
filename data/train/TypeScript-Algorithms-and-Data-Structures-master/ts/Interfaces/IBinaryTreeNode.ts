export class IBinaryTreeNode<T>{
    left:IBinaryTreeNode<T>;
    right:IBinaryTreeNode<T>;
    value:T;
    parent:IBinaryTreeNode<T>;
    hasLeftChild: () =>  boolean;
    hasRightChild: ()=> boolean;
    isLeftChild: ()=> boolean;
    isRightChild: ()=> boolean;
    isRoot: ()=> boolean;
    min: ()=> IBinaryTreeNode<T>;
    max: ()=> IBinaryTreeNode<T>;
}