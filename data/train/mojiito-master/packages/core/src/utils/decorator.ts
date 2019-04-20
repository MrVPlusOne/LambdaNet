/**
 * @license
 * Copyright Google Inc. All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */

import { global, stringify } from '../../../facade/src/lang';
import { ClassType } from '../type';

let _nextClassId = 0;
const Reflect = global.Reflect;


export interface TypeDecorator {
  <T extends ClassType<any>>(type: T): T;
  (target: Object, propertyKey?: string | symbol, parameterIndex?: number): void;
  annotations: any[];
  Class(obj: ClassType<any>): ClassType<any>;
}

function extractAnnotation(annotation: any): any {
  if (typeof annotation === 'function' && annotation.hasOwnProperty('annotation')) {
    // it is a decorator, extract annotation
    annotation = annotation.annotation;
  }
  return annotation;
}

function applyParams(fnOrArray: (Function | any[]), key: string): Function {
  if (fnOrArray === Object || fnOrArray === String || fnOrArray === Function ||
    fnOrArray === Number || fnOrArray === Array) {
    throw new Error(`Can not use native ${stringify(fnOrArray)} as constructor`);
  }

  if (typeof fnOrArray === 'function') {
    return fnOrArray;
  }

  if (Array.isArray(fnOrArray)) {
    const annotations: any[] = fnOrArray;
    const annoLength = annotations.length - 1;
    const fn: Function = fnOrArray[annoLength];
    if (typeof fn !== 'function') {
      throw new Error(`Last position of Class method array must be ` +
        `Function in key ${key} was '${stringify(fn)}'`);
    }
    if (annoLength != fn.length) {
      throw new Error(`Number of annotations (${annoLength}) does not match ` +
        `number of arguments (${fn.length}) in the function: ${stringify(fn)}`);
    }
    const paramsAnnotations: any[][] = [];
    for (let i = 0, ii = annotations.length - 1; i < ii; i++) {
      const paramAnnotations: any[] = [];
      paramsAnnotations.push(paramAnnotations);
      const annotation = annotations[i];
      if (Array.isArray(annotation)) {
        for (let j = 0; j < annotation.length; j++) {
          paramAnnotations.push(extractAnnotation(annotation[j]));
        }
      } else if (typeof annotation === 'function') {
        paramAnnotations.push(extractAnnotation(annotation));
      } else {
        paramAnnotations.push(annotation);
      }
    }
    Reflect.defineMetadata('parameters', paramsAnnotations, fn);
    return fn;
  }

  throw new Error(`Only Function or Array is supported in Class ` +
    `definition for key '${key}' is '${stringify(fnOrArray)}'`);
}

export function makeDecorator(
  name: string, props: { [name: string]: any }, parentClass?: any,
  chainFn: (fn: Function) => void = null): (...args: any[]) => (cls: any) => any {
  const metaCtor = makeMetadataCtor([props]);

  function DecoratorFactory(objOrType: any): (cls: any) => any {
    if (!(Reflect && Reflect.getOwnMetadata)) {
      throw 'reflect-metadata shim is required when using class decorators';
    }

    if (this instanceof DecoratorFactory) {
      metaCtor.call(this, objOrType);
      return this;
    }

    const annotationInstance = new (<any>DecoratorFactory)(objOrType);
    const chainAnnotation =
      typeof this === 'function' && Array.isArray(this.annotations) ? this.annotations : [];
    chainAnnotation.push(annotationInstance);
    // tslint:disable-next-line:variable-name
    const TypeDecorator: TypeDecorator =
      <TypeDecorator>function TypeDecorator(cls: ClassType<any>) {
        const annotations = Reflect.getOwnMetadata('annotations', cls) || [];
        annotations.push(annotationInstance);
        Reflect.defineMetadata('annotations', annotations, cls);
        return cls;
      };
    TypeDecorator.annotations = chainAnnotation;
    // TypeDecorator.Class = Class;
    if (chainFn) chainFn(TypeDecorator);
    return TypeDecorator;
  }

  if (parentClass) {
    DecoratorFactory.prototype = Object.create(parentClass.prototype);
  }

  DecoratorFactory.prototype.toString = () => `@${name}`;
  (<any>DecoratorFactory).annotationCls = DecoratorFactory;
  return DecoratorFactory;
}

function makeMetadataCtor(props: ([string, any] | { [key: string]: any })[]): any {
  return function ctor(...args: any[]) {
    props.forEach((prop, i) => {
      const argVal = args[i];
      if (Array.isArray(prop)) {
        // plain parameter
        this[prop[0]] = argVal === undefined ? prop[1] : argVal;
      } else {
        for (const propName in prop) {
          this[propName] =
            argVal && argVal.hasOwnProperty(propName) ? argVal[propName] : prop[propName];
        }
      }
    });
  };
}

export function makeParamDecorator(
  name: string, props: ([string, any] | { [name: string]: any })[], parentClass?: any): any {
  const metaCtor = makeMetadataCtor(props);
  function ParamDecoratorFactory(...args: any[]): any {
    if (this instanceof ParamDecoratorFactory) {
      metaCtor.apply(this, args);
      return this;
    }
    const annotationInstance = new (<any>ParamDecoratorFactory)(...args);

    (<any>ParamDecorator).annotation = annotationInstance;
    return ParamDecorator;

    function ParamDecorator(cls: any, unusedKey: any, index: number): any {
      const parameters: any[][] = Reflect.getOwnMetadata('parameters', cls) || [];

      // there might be gaps if some in between parameters do not have annotations.
      // we pad with nulls.
      while (parameters.length <= index) {
        parameters.push(null);
      }

      parameters[index] = parameters[index] || [];
      parameters[index].push(annotationInstance);

      Reflect.defineMetadata('parameters', parameters, cls);
      return cls;
    }
  }
  if (parentClass) {
    ParamDecoratorFactory.prototype = Object.create(parentClass.prototype);
  }
  ParamDecoratorFactory.prototype.toString = () => `@${name}`;
  (<any>ParamDecoratorFactory).annotationCls = ParamDecoratorFactory;
  return ParamDecoratorFactory;
}

export function makePropDecorator(
  name: string, props: ([string, any] | { [key: string]: any })[], parentClass?: any): any {
  const metaCtor = makeMetadataCtor(props);

  function PropDecoratorFactory(...args: any[]): any {
    if (this instanceof PropDecoratorFactory) {
      metaCtor.apply(this, args);
      return this;
    }

    const decoratorInstance = new (<any>PropDecoratorFactory)(...args);

    // tslint:disable-next-line:no-shadowed-variable
    return function PropDecorator(target: any, name: string) {
      const meta = Reflect.getOwnMetadata('propMetadata', target.constructor) || {};
      meta[name] = meta.hasOwnProperty(name) && meta[name] || [];
      meta[name].unshift(decoratorInstance);
      Reflect.defineMetadata('propMetadata', meta, target.constructor);
    };
  }

  if (parentClass) {
    PropDecoratorFactory.prototype = Object.create(parentClass.prototype);
  }

  PropDecoratorFactory.prototype.toString = () => `@${name}`;
  (<any>PropDecoratorFactory).annotationCls = PropDecoratorFactory;
  return PropDecoratorFactory;
}
