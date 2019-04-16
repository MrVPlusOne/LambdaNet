import { ViewRef } from './view_ref';
import { ElementRef } from './element_ref';
import { Injector } from '../di/injector';
import { ComponentFactory } from '../component/factory';
import { ComponentRef } from '../component/reference';

export abstract class ViewContainerRef {

  abstract get anchorElement(): ElementRef;
  abstract get injector(): Injector;
  abstract get parentInjector(): Injector;

  /** Destroys all Views in this container. */
  abstract clear(): void;

  /** Returns the ViewRef for the View located in this container at the specified index. */
  abstract get(index: number): ViewRef;

  /** Returns the number of Views currently attached to this container. */
  abstract get length(): number;

  /**
   * Instantiates an Embedded View based on the TemplateRef `templateRef`} and inserts it
   * into this container at the specified `index`.
   *
   * If `index` is not specified, the new View will be inserted as the last View in the container.
   *
   * Returns the ViewRef for the newly created View.
   */
  abstract createEmbeddedView<C>(templateRef: any, context?: C, index?: number):
    any; // EmbeddedViewRef<C>;

  /**
   * Instantiates a single Component and inserts its Host View into this container at the
   * specified `index`.
   *
   * The component is instantiated using its ComponentFactory which can be
   * obtained via ComponentFactoryResolver#resolveComponentFactory}.
   *
   * If `index` is not specified, the new View will be inserted as the last View in the container.
   *
   * You can optionally specify the Injector that will be used as parent for the Component.
   *
   * Returns the ComponentRef of the Host View created for the newly instantiated Component.
   */
  abstract createComponent<C>(
      componentFactory: ComponentFactory<C>, index?: number, injector?: Injector,
      projectableNodes?: any[][]): ComponentRef<C>;

  /**
   * Inserts a View identified by a ViewRef into the container at the specified `index`.
   *
   * If `index` is not specified, the new View will be inserted as the last View in the container.
   *
   * Returns the inserted ViewRef.
   */
  // abstract insert(viewRef: ViewRef, index?: number): ViewRef;

  /**
   * Moves a View identified by a ViewRef into the container at the specified `index`.
   *
   * Returns the inserted {@link ViewRef}.
   */
  // abstract move(viewRef: ViewRef, currentIndex: number): ViewRef;

  /**
   * Returns the index of the View, specified via ViewRef, within the current container or
   * `-1` if this container doesn't contain the View.
   */
  abstract indexOf(viewRef: ViewRef): number;

  /**
   * Destroys a View attached to this container at the specified `index`.
   *
   * If `index` is not specified, the last View in the container will be removed.
   */
  abstract remove(index?: number): void;

  /**
   * Use along with #nsert} to move a View within the current container.
   *
   * If the `index` param is omitted, the last ViewRef is detached.
   */
  abstract detach(index?: number): ViewRef;
}
