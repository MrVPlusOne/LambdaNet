import { Renderer, RendererFactory, Injectable, RendererType, Visitor } from 'mojiito-core';
import { isPresent, stringify } from './facade/lang';
import { DOCUMENT } from './tokens';
import { DomTraverser } from './dom_traverser';
import { DomVisitor } from './dom_visitor';

export const NAMESPACE_URIS: { [ns: string]: string } = {
  'xlink': 'http://www.w3.org/1999/xlink',
  'svg': 'http://www.w3.org/2000/svg',
  'xhtml': 'http://www.w3.org/1999/xhtml',
  'xml': 'http://www.w3.org/XML/1998/namespace'
};

function getGlobalEventTarget(target: string): any {
  if (target === 'window') {
    return window;
  }
  if (target === 'document') {
    return this.document;
  }
  if (target === 'body') {
    return this.document.body;
  }
  return undefined;
}

@Injectable()
export class DomRendererFactory implements RendererFactory {
  private rendererByCompId = new Map<string, Renderer>();
  private defaultRenderer: Renderer;

  constructor() {
    this.defaultRenderer = new DefaultDomRenderer();
  };

  createRenderer(element: any, type: RendererType): Renderer {
    if (!element || !type) {
      return this.defaultRenderer;
    }
    return new ParseableDomRenderer(type.visitor, element);
  }
}

export class DefaultDomRenderer implements Renderer {
  parse(context: any) {
    throw new Error(`Parse is not allowed on the DefaultDomRenderer!`);
  }
  destroy(): void { }
  createElement(name: string, namespace?: string): any {
    if (namespace) {
      return document.createElementNS(NAMESPACE_URIS[namespace], name);
    }
    return document.createElement(name);
  }
  createComment(value: string): any { return document.createComment(value); }
  createText(value: string): any { return document.createTextNode(value); }
  destroyNode(node: any): void { }
  appendChild(parent: any, newChild: any): void { parent.appendChild(newChild); }
  insertBefore(parent: any, newChild: any, refChild: any): void {
    if (parent) {
      parent.insertBefore(newChild, refChild);
    }
  }
  removeChild(parent: any, oldChild: any): void {
    if (parent) {
      parent.removeChild(oldChild);
    }
  }
  selectRootElement(selectorOrNode: string | any): any {
    let el: any = selectorOrNode;
    if (typeof selectorOrNode === 'string') {
      el = document.querySelector(selectorOrNode);
    }
    if (!el) {
      throw new Error(`The selector "${selectorOrNode}" did not match any elements`);
    }
    return el;
  }
  parentNode(node: any): any { return node.parentNode; }
  nextSibling(node: any): any { return node.nextSibling; }
  setAttribute(el: any, name: string, value: string, namespace?: string): void {
    if (namespace) {
      el.setAttributeNS(NAMESPACE_URIS[namespace], namespace + ':' + name, value);
    } else {
      el.setAttribute(name, value);
    }
  }
  removeAttribute(el: any, name: string, namespace?: string): void {
    if (namespace) {
      el.removeAttributeNS(NAMESPACE_URIS[namespace], name);
    } else {
      el.removeAttribute(name);
    }
  }
  addClass(el: any, name: string): void { el.classList.add(name); }
  removeClass(el: any, name: string): void { el.classList.remove(name); }
  setStyle(el: any, style: string, value: any, hasVendorPrefix: boolean, hasImportant: boolean):
    void {
    if (hasVendorPrefix || hasImportant) {
      el.style.setProperty(style, value, hasImportant ? 'important' : '');
    } else {
      el.style[style] = value;
    }
  }

  removeStyle(el: any, style: string, hasVendorPrefix: boolean): void {
    if (hasVendorPrefix) {
      el.style.removeProperty(style);
    } else {
      // IE requires '' instead of null
      el.style[style] = '';
    }
  }
  setProperty(el: any, name: string, value: any): void { el[name] = value; }
  setValue(node: any, value: string): void { node.nodeValue = value; }
  listen(target: 'window' | 'document' | 'body' | any, event: string,
    callback: (event: any) => boolean | void): () => void {
    if (typeof target === 'string') {
      target = getGlobalEventTarget(target);
      if (!target) {
        throw new Error(`Unsupported event target ${target} for event ${event}`);
      }
    }
    target.addEventListener(event, callback as any, false);
    return () => target.removeEventListener(event, callback as any, false);
  }
}

export class ParseableDomRenderer extends DefaultDomRenderer {
  constructor(private _visitor: Visitor,
    public hostElement: Node) {
    super();
  }

  parse(context: any) {
    const traverser = new DomTraverser();
    traverser.traverse(this.hostElement, this._visitor, context);
  }

  destroyNode(node: any) {
    if (node instanceof Node) {
      this.removeChild(node.parentNode, node);
    }
  }
}
