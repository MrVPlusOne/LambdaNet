import { ClassType } from '../type';
import { Injector } from '../di/injector';
import { ElementRef } from '../view/element_ref';
import { ViewRef } from '../view/view_ref';

/**
 * Represents an instance of a Component created via a ComponentFactory.
 * `ComponentRef` provides access to the Component Instance as well other objects related to this
 * Component Instance and allows you to destroy the Component Instance via the destroy method.
 *
 * @export
 * @class ComponentRef
 * @template C
 */
export abstract class ComponentRef<C> {

  /** Location of the component instance */
  abstract get location(): ElementRef;

  /** The injector on which the component instance exists. */
  abstract get injector(): Injector;

  /** The instance of the Component. */
  abstract get instance(): C;

  abstract get hostView(): ViewRef;

  // get changeDetectorRef(): ChangeDetectorRef;

  /** The component type. */
  abstract get componentType(): ClassType<C>;

  /** Destroys the component instance and all of the data structures associated with it. */
  abstract destroy(): void;

  /** Allows to register a callback that will be called when the component is destroyed. */
  abstract onDestroy(callback: Function): void;
}
