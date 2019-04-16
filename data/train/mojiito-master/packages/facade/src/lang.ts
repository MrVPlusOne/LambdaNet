/**
 * Returns the class name of a type.
 *
 * @export
 * @template T
 * @param {Function} klass
 * @returns
 */
export function getClassName<T>(klass: Function) {
  return (klass as any).name ? (klass as any).name :
    /^function\s+([\w\$]+)\s*\(/.exec(this.toString())[1];
}

/**
 * Tries to stringify a token. A token can be any type.
 *
 * @export
 * @param {*} token
 * @returns {string}
 */
export function stringify(token: any): string {
  if (typeof token === 'string') {
    return token;
  }

  if (token === undefined || token === null) {
    return '' + token;
  }
  if (token.name) {
    return token.name;
  }
  if (token.overriddenName) {
    return token.overriddenName;
  }
  if (typeof token === 'function') {
      return getClassName(token);
  }
  if (token instanceof HTMLElement) {
    let parts = token.toString().match(/\w+/g);
    if (parts && parts.length) {
      return parts[parts.length - 1];
    }
  }

  var res = token.toString();
  var newLineIndex = res.indexOf('\n');
  return (newLineIndex === -1) ? res : res.substring(0, newLineIndex);
}

export interface BrowserNodeGlobal {
  Object: typeof Object;
  Array: typeof Array;
  Map: typeof Map;
  Set: typeof Set;
  Date: DateConstructor;
  RegExp: RegExpConstructor;
  JSON: typeof JSON;
  Math: any;  // typeof Math;
  assert(condition: any): void;
  Reflect: any;
  setTimeout: Function;
  clearTimeout: Function;
  setInterval: Function;
  clearInterval: Function;
  encodeURI: Function;
}

declare var WorkerGlobalScope: any;
declare var global: any;
let globalScope: BrowserNodeGlobal;
if (typeof window === 'undefined') {
  if (typeof WorkerGlobalScope !== 'undefined' && self instanceof WorkerGlobalScope) {
    // TODO: Replace any with WorkerGlobalScope from lib.webworker.d.ts #3492
    globalScope = <any>self;
  } else {
    globalScope = <any>global;
  }
} else {
  globalScope = <any>window;
}
export {globalScope as global};

export function isPresent(obj: any): boolean {
  return obj != null;
}

export function isBlank(obj: any): boolean {
  return obj == null;
}

export class NumberWrapper {
  static parseIntAutoRadix(text: string): number {
    const result: number = parseInt(text);
    if (isNaN(result)) {
      throw new Error('Invalid integer literal when parsing ' + text);
    }
    return result;
  }

  static isNumeric(value: any): boolean { return !isNaN(value - parseFloat(value)); }
}
