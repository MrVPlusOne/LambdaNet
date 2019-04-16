// tslint:disable:class-name
import { ClassType } from '../type';
import { ApplicationRef } from '../application/application';
import { Renderer } from '../render';
import { Injector } from '../di/injector';
import { ComponentRef } from '../component/reference';
import { ComponentFactory } from '../component/factory';
import { createRootView, destroyView, initView } from './view';
import { ViewRef, InternalViewRef } from './view_ref';
import { ViewContainerRef } from './view_container_ref';
import { attachEmbeddedView, detachEmbeddedView } from './view_attach';
import { ElementRef } from './element_ref';
import {
  ViewData, ViewDefinitionFactory, ViewDefinition, ViewState,
  asProviderData, DepFlags, ViewContainerData
} from './types';
import { resolveViewDefinition } from './utils';
import { resolveDep, tokenKey } from './provider';

const EMPTY_CONTEXT = new Object();

export function createInjector(view: ViewData): Injector {
  return new Injector_(view);
}

/**
 * Internal ComponentFactory
 */
class ComponentFactory_ extends ComponentFactory<any> {
  constructor(public selector: string, public componentType: ClassType<any>,
    private _viewDefFactory: ViewDefinitionFactory) {
    super();
  }
  create(injector: Injector, rootSelectorOrNode?: string | any): ComponentRef<any> {
    const viewDef = resolveViewDefinition(this._viewDefFactory);
    const componentNodeIndex = viewDef.componentProvider.index;
    const view = createRootView(viewDef, injector, rootSelectorOrNode, EMPTY_CONTEXT);
    const component = asProviderData(view, componentNodeIndex).instance;
    return new ComponentRef_(view, new ViewRef_(view), component);
  }
}

export function createComponentFactory(selector: string, componentType: ClassType<any>,
  viewDefFactory: ViewDefinitionFactory): ComponentFactory<any> {
  return new ComponentFactory_(selector, componentType, viewDefFactory);
}

/**
 * Internal ComponentRef
 */
class ComponentRef_ extends ComponentRef<any> {
  constructor(private _view: ViewData, private _viewRef: ViewRef, private _component: any) {
    super();
  }

  get location(): ElementRef { return new ElementRef(null); }
  get injector(): Injector { return new Injector_(this._view); }
  get instance(): any { return this._component; };
  get hostView(): ViewRef { return this._viewRef; };
  // get changeDetectorRef(): ChangeDetectorRef { return this._viewRef; };
  get componentType(): ClassType<any> { return <any>this._component.constructor; }

  destroy(): void { this._viewRef.destroy(); }
  onDestroy(callback: Function): void { this._viewRef.onDestroy(callback); }
}

/**
 * Internal ViewContainerRef
 */
class ViewContainerRef_ implements ViewContainerData {

  /* @internal */
  _embeddedViews: ViewData[] = [];
  constructor(private _view: ViewData) { }

  get anchorElement(): ElementRef { return new ElementRef(this._view.renderElement); }
  get injector(): Injector { return new Injector_(this._view); }
  get parentInjector(): Injector {
    let view = this._view;
    let def = view.def;
    while (!def && view) {
      view = view.parent;
      def = view.def;
    }
    return view ? new Injector_(view) : this._view.root.injector;
  }

  clear(): void { }

  get(index: number): ViewRef {
    const view = this._embeddedViews[index];
    if (view) {
      const ref = new ViewRef_(view);
      ref.attachToViewContainerRef(this);
      return ref;
    }
    return null;
  }

  get length(): number { return 0; } // this._view.embeddedViews.length; }

  createEmbeddedView<C>(templateRef: any, context?: C, index?: number):
    any { } // EmbeddedViewRef<C> { }

  createComponent<C>(componentFactory: ComponentFactory<C>, index?: number, injector?: Injector,
    rootSelectorOrNode?: any): ComponentRef<C> {
    const contextInjector = injector || this.parentInjector;
    const componentRef = componentFactory.create(contextInjector, rootSelectorOrNode);
    this.insert(componentRef.hostView, index);
    return componentRef;
  }

  insert(viewRef: ViewRef, index?: number): ViewRef {
    // tslint:disable-next-line:variable-name
    const viewRef_ = <ViewRef_>viewRef;
    const viewData = viewRef_._view;
    attachEmbeddedView(this._view, index, viewData);
    viewRef_.attachToViewContainerRef(this);
    return viewRef;
  }

  // move(viewRef: ViewRef, currentIndex: number): ViewRef { }

  indexOf(viewRef: ViewRef): number {
    return this._embeddedViews.indexOf((<ViewRef_>viewRef)._view);
  }

  remove(index?: number): void {
    const view = detachEmbeddedView(this._view, index);
    if (view) {
      destroyView(view);
    }
  }

  detach(index?: number): ViewRef {
    const view = detachEmbeddedView(this._view, index);
    return view ? new ViewRef_(view) : null;
  }
}

export function createViewContainerData(view: ViewData): ViewContainerData {
  return new ViewContainerRef_(view);
}

/**
 * Internal View Reference
 */
class ViewRef_ implements InternalViewRef {
  _view: ViewData;
  private _viewContainerRef: ViewContainerRef;
  private _appRef: ApplicationRef;

  constructor(_view: ViewData) {
    this._view = _view;
    this._viewContainerRef = null;
    this._appRef = null;
  }

  // get rootNodes(): any[] { return rootRenderNodes(this._view); }

  get context() { return this._view.context; }
  // tslint:disable-next-line:no-bitwise
  get destroyed(): boolean { return (this._view.state & ViewState.Destroyed) !== 0; }

  // markForCheck(): void { markParentViewsForCheck(this._view); }
  detach(): void { this._view.state &= ~ViewState.ChecksEnabled; }
  // detectChanges(): void { Services.checkAndUpdateView(this._view); }
  // checkNoChanges(): void { Services.checkNoChangesView(this._view); }

  reattach(): void { this._view.state |= ViewState.ChecksEnabled; }
  onDestroy(callback: Function) {
    if (!this._view.disposables) {
      this._view.disposables = [];
    }
    this._view.disposables.push(<any>callback);
  }

  destroy() {
    if (this._appRef) {
      this._appRef.detachView(this);
    } else if (this._viewContainerRef) {
      this._viewContainerRef.detach(this._viewContainerRef.indexOf(this));
    }
    destroyView(this._view);
  }

  detachFromAppRef() {
    this._appRef = null;
  }

  attachToAppRef(appRef: ApplicationRef) {
    if (this._viewContainerRef) {
      throw new Error('This view is already attached to a ViewContainer!');
    }
    this._appRef = appRef;
  }

  attachToViewContainerRef(vcRef: ViewContainerRef) {
    if (this._appRef) {
      throw new Error('This view is already attached directly to the ApplicationRef!');
    }
    this._viewContainerRef = vcRef;
  }
}

/**
 * Internal View Injector
 */
class Injector_ implements Injector {
  constructor(private _view: ViewData) { }
  get(token: any, notFoundValue: any = Injector.THROW_IF_NOT_FOUND): any {
    return resolveDep(this._view,
      {flags: DepFlags.None, token, tokenKey: tokenKey(token)}, notFoundValue);
  }
}
