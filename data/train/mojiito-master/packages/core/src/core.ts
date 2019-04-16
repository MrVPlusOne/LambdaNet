import { ComponentResolver } from './component/resolver';
import { reflector, Reflector } from './reflection/reflection';
import { ReflectorReader } from './reflection/reflector_reader';
import {
  Provider, ClassProvider, ExistingProvider,
  FactoryProvider, TypeProvider, ValueProvider,
} from './di/provider';

// Platform & Application
export { createPlatformFactory, getPlatform, PlatformRef } from './application/platform';
export { ApplicationRef } from './application/application';

// Component
export { Component, HostListener, ChildListener } from './component/metadata';
export { ComponentResolver };
export { ComponentFactory } from './component/factory';
export { ComponentFactoryResolver } from './component/factory_resolver';
export { ComponentRef } from './component/reference';

// View
export { createComponentFactory } from './view/refs';
export * from './view/types';
export { ViewRef } from './view/view_ref';
export { ViewContainerRef } from './view/view_container_ref';
export { ElementRef } from './view/element_ref';
export { createViewDefinitionFactory, createView } from './view/view';

// Dependency Injection
export { forwardRef } from './di/forward_ref';
export { InjectionToken } from './di/injection_token';
export { Injector } from './di/injector';
export { Host, Inject, Injectable, Optional, Self, SkipSelf } from './di/metadata';
export {
  Provider, ClassProvider, ExistingProvider,
  FactoryProvider, TypeProvider, ValueProvider
};
export { ReflectiveInjector, ReflectiveInjector_ } from './di/reflective_injector';
export { ReflectiveKey } from './di/reflective_key';
export {
  ReflectiveDependency, ResolvedReflectiveFactory, ResolvedReflectiveProvider,
  ResolvedReflectiveProvider_, resolveReflectiveProviders, mergeResolvedReflectiveProviders,
  constructDependencies
} from './di/reflective_provider';
export { reflector, ReflectorReader, Reflector }

// Others
export { ClassType, Type } from './type';
export { Renderer, RendererFactory, RendererType, Visitor } from './render';

// Providers
export const CORE_PROVIDERS: Provider[] = [
  { provide: ReflectorReader, useValue: reflector },
  ComponentResolver
];
