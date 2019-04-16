export interface Visitor {
  visitElement(element: Element, context: any): any;
  visitAttribute(element: Element, attr: Attr, context: any): any;
  visitText(text: Text, context: any): any;
  visitComment(comment: Comment, context: any): any;
}

export interface RendererType {
  visitor: Visitor;
  data: {[kind: string]: any};
}

/**
 * @experimental
 */
export abstract class RendererFactory {
  abstract createRenderer(hostElement: any, type: RendererType): Renderer;
}


export abstract class Renderer {
  abstract parse(context: any): void;
  abstract destroy(): void;
  abstract createElement(name: string, namespace?: string): any;
  abstract createComment(value: string): any;
  abstract createText(value: string): any;
  abstract destroyNode(node: any): void;
  abstract appendChild(parent: any, newChild: any): void;
  abstract insertBefore(parent: any, newChild: any, refChild: any): void;
  abstract removeChild(parent: any, oldChild: any): void;
  abstract selectRootElement(selectorOrNode: string|any): any;
  abstract parentNode(node: any): any;
  abstract nextSibling(node: any): any;
  abstract setAttribute(el: any, name: string, value: string, namespace?: string): void;
  abstract removeAttribute(el: any, name: string, namespace?: string): void;
  abstract addClass(el: any, name: string): void;
  abstract removeClass(el: any, name: string): void;
  abstract setStyle(
      el: any, style: string, value: any, hasVendorPrefix: boolean, hasImportant: boolean): void;
  abstract removeStyle(el: any, style: string, hasVendorPrefix: boolean): void;
  abstract setProperty(el: any, name: string, value: any): void;
  abstract setValue(node: any, value: string): void;
  abstract listen(
      target: 'window'|'document'|'body'|any, eventName: string,
      callback: (event: any) => boolean | void): () => void;
}
