export interface ClassType<T> extends Function {
  new (...args: Array<any>): T;
  constructor: Function | any[];
  [propertyName: string]: any;
  name: string;
}

export function isClassInstance(instance: any): boolean {
  return typeof instance === 'object' && !!instance['constructor'];
}

// tslint:disable:variable-name
export const Type = Function;

export function isType(v: any): v is Type<any> {
  return typeof v === 'function';
}

export interface Type<T> extends Function { new (...args: any[]): T; }
