import { ListWrapper } from '../facade/collection';
import { ComponentFactoryResolver } from '../component/factory_resolver';
import { ComponentRef } from '../component/reference';
import { ComponentFactory } from '../component/factory';
import { ClassType } from '../type';
import {
  NotYetBootstrappedError,
  AlreadyBootstrappedError
} from './application_errors';
import { Component } from '../component/metadata';
import { ViewRef, InternalViewRef } from '../view/view_ref';
import { Injectable, Inject } from '../di/metadata';
import { Injector, THROW_IF_NOT_FOUND } from '../di/injector';
import {reflector} from '../reflection/reflection';
import { getPlatform } from './platform';

/**
 * This is a reference of a Mojiito Application.
 *
 * @export
 * @class ApplicationRef
 */
@Injectable()
export class ApplicationRef {

  private _rootComponents: ComponentRef<any>[] = [];
  private _rootComponentTypes: ClassType<any>[] = [];
  private _views: InternalViewRef[] = [];

  constructor(public injector: Injector,
    private _componentFactoryResolver: ComponentFactoryResolver) { }

  bootstrap<C>(componentOrFactory: ClassType<C> | ComponentFactory<C>): ComponentRef<C> {
    let componentFactory: ComponentFactory<C>;
    if (componentOrFactory instanceof ComponentFactory) {
      componentFactory = componentOrFactory;
    } else {
      componentFactory = this._componentFactoryResolver.resolveComponentFactory(componentOrFactory);
    }
    this._rootComponentTypes.push(componentFactory.componentType);
    const compRef = componentFactory.create(this.injector, componentFactory.selector);
    compRef.onDestroy(() => { this._unloadComponent(compRef); });
    this._loadComponent(compRef);
    return compRef;
  }

  attachView(viewRef: ViewRef): void {
    const view = (viewRef as InternalViewRef);
    this._views.push(view);
    view.attachToAppRef(this);
  }

  detachView(viewRef: ViewRef): void {
    const view = (viewRef as InternalViewRef);
    ListWrapper.remove(this._views, view);
    view.detachFromAppRef();
  }

  private _loadComponent(componentRef: ComponentRef<any>): void {
    this.attachView(componentRef.hostView);
    // this.tick();
    this._rootComponents.push(componentRef);
  }

  private _unloadComponent(componentRef: ComponentRef<any>): void {
    this.detachView(componentRef.hostView);
    ListWrapper.remove(this._rootComponents, componentRef);
  }

  onDestroy() {
    this._views.slice().forEach((view) => view.destroy());
  }
}
