import {
  PlatformRef, Injectable, Injector, ClassType, ApplicationRef,
  ComponentResolver, ReflectiveInjector, ComponentFactoryResolver
} from 'mojiito-core';
import { Compiler } from './compiler/compiler';

@Injectable()
export class BrowserPlatformRef extends PlatformRef {
  private _destroyed = false;
  private _destroyListeners: Function[] = [];

  constructor(private _injector: Injector, private _resolver: ComponentResolver,
    private _compiler: Compiler) {
    super();
  }

  get injector(): Injector { return this._injector; }
  get destroyed(): boolean { return this._destroyed; }

  bootstrapComponent<C>(component: ClassType<C>): void {
    this._compiler.compileComponents([component]);
    const appInjector = ReflectiveInjector.resolveAndCreate([
      {
        provide: ComponentFactoryResolver,
        useFactory: () => this._compiler.createComponentFactoryResolver()
      },
      ApplicationRef
    ], this._injector);

    const app = appInjector.get(ApplicationRef) as ApplicationRef;
    app.bootstrap(component);
  }

  onDestroy(callback: () => void): void {
    this._destroyListeners.push(callback);
  }

  destroy(): void {
    if (this._destroyed) {
      throw new Error('The platform has already been destroyed!');
    }
    // TODO: destroy all se stuff
    this._destroyListeners.forEach(listener => listener());
    this._destroyed = true;
  }
}
