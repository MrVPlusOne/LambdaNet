import { Injector } from '../di/injector';
import { ViewDefinition, ViewDefinitionFactory, ViewData } from './types';
import { createInjector } from './refs';

const VIEW_DEFINITION_CACHE = new WeakMap<any, ViewDefinition>();
export function resolveViewDefinition(factory: ViewDefinitionFactory): ViewDefinition {
  let value: ViewDefinition = VIEW_DEFINITION_CACHE.get(factory);
  if (!value) {
    value = factory();
    VIEW_DEFINITION_CACHE.set(factory, value);
  }
  return value;
}
