// tslint:disable:variable-name

import { makeDecorator, TypeDecorator, makePropDecorator } from '../utils/decorator';
import { stringify } from '../facade/lang';
import { ClassType } from '../type';
import { Provider } from '../di/provider';

/** Type of the Component decorator / constructor function. */
export interface ComponentDecorator {
  (obj: Component): TypeDecorator;
  new (obj: Component): Component;
}

/** Type of the Component metadata. */
export interface Component {
  /**
   * The CSS selector that triggers the instantiation of a directive.
   *
   * Mojiito only allows components to trigger on CSS selectors that do not cross element
   * boundaries.
   *
   * `selector` may be declared as one of the following:
   *
   * - `element-name`: select by element name.
   * - `.class`: select by class name.
   * - `[attribute]`: select by attribute name.
   * - `[attribute=value]`: select by attribute name and value.
   * - `:not(sub_selector)`: select only if the element does not match the `sub_selector`.
   * - `selector1, selector2`: select if either `selector1` or `selector2` matches.
   */
  selector?: string;

  /**
   * Defines the set of injectable objects that are visible to a Components.
   */
  providers?: Provider[];

  /**
   * Defines a list of components which belong to this component and
   * can be instantiated.
   *
   * When creating this component, mojiito will look for them in the
   * DOM and create them if found.
   */
  components?: any[] | any[][];

  /**
   * Specify the events, actions, properties and attributes related to the host element.
   */
  host?: {[key: string]: string};

  /**
   * Specify the events, actions, properties and attributes related to child elements.
   */
  childs?: {[key: string]: string};
}

/**
 * Component decorator and metadata.
 *
 * @Annotation
 */
export const Component: ComponentDecorator = <ComponentDecorator>makeDecorator('Component', {
  selector: undefined,
  providers: undefined,
  components: undefined,
  host: undefined,
  childs: undefined,
});


/** Type of the HostListener decorator / constructor function. */
export interface HostListenerDecorator {
  /** Declares a host listener. */
  (eventName: string, args?: string[]): any;
  new (eventName: string, args?: string[]): any;
}

/** Type of the HostListener metadata. */
export interface HostListener {
  eventName?: string;
  args?: string[];
}

/**
 * HostListener decorator and metadata.
 *
 * @Annotation
 */
export const HostListener: HostListenerDecorator =
    makePropDecorator('HostListener', [['eventName', undefined], ['args', []]]);

/** Type of the ChildListener decorator / constructor function. */
export interface ChildListenerDecorator {
  /** Declares a Child listener. */
  (selector: string, eventName: string, args?: string[]): any;
  new (selector: string, eventName: string, args?: string[]): any;
}

/** Type of the ChildListener metadata. */
export interface ChildListener {
  selector?: string;
  eventName?: string;
  args?: string[];
}

/**
 * ChildListener decorator and metadata.
 *
 * @Annotation
 */
export const ChildListener: ChildListenerDecorator =
    makePropDecorator('ChildListener', [
      ['selector', undefined],
      ['eventName', undefined],
      ['args', []]
    ]);
