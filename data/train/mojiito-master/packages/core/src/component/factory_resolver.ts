import { ClassType } from '../type';
import { ComponentFactory } from './factory';
import { BaseError } from '../facade/error';
import { stringify } from '../facade/lang';

export class ComponentFactoryResolver {
  private _factories = new Map<any, ComponentFactory<any>>();

  constructor(factories: ComponentFactory<any>[], private _parent?: ComponentFactoryResolver) {
    for (let i = 0; i < factories.length; i++) {
      const factory = factories[i];
      this._factories.set(factory.componentType, factory);
    }
  }

  resolveComponentFactory<C>(componentType: ClassType<C>): ComponentFactory<C> {
    let result = this._factories.get(componentType);
    if (!result) {
      if (!this._parent) {
        throw new CouldNotResolveFactoryError(componentType);
      }
      result = this._parent.resolveComponentFactory(componentType);
    }
    return result;
  }
}

export class CouldNotResolveFactoryError extends BaseError {
  constructor(type: ClassType<any>) {
    super(`Could not resolve factory for "${stringify(type)}! ` +
      `Did you provide the component to the bootstrap function?`);
  }
}
