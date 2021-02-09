export interface VNode {
  /**
   * The CSS selector containing tagname, css classnames and id. An empty string is used to denote a text node.
   */
  readonly vnodeSelector: string;
  /**
   * Object containing attributes, properties, event handlers and more, see [[h]].
   */
  // readonly properties: VNodeProperties | undefined;
  /**
   * Array of [[VNode]]s to be used as children. This array is already flattened.
   */
  readonly children: VNode[] | undefined;
  /**
   * Used in a special case when a [[VNode]] only has one child node which is a text node. Only used in combination with children === undefined.
   */
  readonly text: string | undefined;
  /**
   * Used by maquette to store the domNode that was produced from this [[VNode]].
   */
  domNode: Node | null;
}

let text: string | undefined;
let tex2: undefined | string;
let flattenedChildren: VNode[] | undefined
let flattenedChildren2: undefined | VNode[];
let result = (text === '') ? undefined : text
