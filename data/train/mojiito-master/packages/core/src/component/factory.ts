import { ClassType } from '../type';
import { ComponentRef } from './reference';
import { Injector } from '../di/injector';

export abstract class ComponentFactory<C> {
  abstract get selector(): string;
  abstract get componentType(): ClassType<any>;
  /**
   * Creates a new component.
   */
  abstract create(injector: Injector, rootSelectorOrNode?: string|any): ComponentRef<C>;
}
