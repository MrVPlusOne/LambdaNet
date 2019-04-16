import { Visitor, ViewData } from 'mojiito-core';
import { DomVisitor } from './dom_visitor';

export interface Traverser {
  traverse(node: any, visitor: Visitor, context: ViewData): void;
}

export class DomTraverser implements Traverser {

  private _nodeCount = 0;
  private _elementCount = 0;
  private _attributeCount = 0;
  private _textCount = 0;
  private _commentCount = 0;

  constructor() { }

  traverse(node: Node, visitor: Visitor, context: ViewData = null) {
    let lclCntxt: ViewData = context;
    this._nodeCount++;

    if (node instanceof Element) {
      lclCntxt = visitor.visitElement(node, lclCntxt) || lclCntxt;
      this._elementCount++;
    } else if (node instanceof Text) {
      lclCntxt = visitor.visitText(node, lclCntxt) || lclCntxt;
      this._textCount++;
    } else if (node instanceof Comment) {
      lclCntxt = visitor.visitComment(node, lclCntxt) || lclCntxt;
      this._commentCount++;
    }

    // Check if context has changed and look up the corresponding
    // NodeVisitor if available
    if (!!lclCntxt && lclCntxt !== context) {
      let rendererType = lclCntxt.def.componentRendererType;
      if (rendererType) {
        visitor = rendererType.visitor;
      }
    } else {
      // Traverse through all the attributes of the node
      // if it is type of Element
      if (node instanceof Element && node.attributes.length) {
        for (let i = 0, max = node.attributes.length; i < max; i++) {
          lclCntxt = visitor.visitAttribute(node, node.attributes[i], lclCntxt) || lclCntxt;
          this._attributeCount++;
        }
      }
    }

    // Start traversing the child nodes
    let childNode = node.firstChild;
    if (childNode) {
      this.traverse(childNode, visitor, lclCntxt);
      while (childNode = childNode.nextSibling) {
        this.traverse(childNode, visitor, lclCntxt);
      }
    }
  }
}
