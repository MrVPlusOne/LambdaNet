import { ListWrapper, StringMapWrapper } from '../facade/collection';
import { stringify } from '../facade/lang';
import { ClassType } from '../type';
import { Component, HostListener, ChildListener } from './metadata';
import { reflector } from '../reflection/reflection';
import { ReflectorReader } from '../reflection/reflector_reader';
import { Injectable } from '../di/metadata';

@Injectable()
export class ComponentResolver {

  private _resolved = new Map<ClassType<any>, Component>();

  constructor(private _reflector: ReflectorReader = reflector) { }

  /**
   * Resolve the metadata of a Component.
   *
   * @param {ClassType<any>} type component type
   * @param {boolean} [throwIfNotFound=true]
   * @returns component metadata
   * @memberOf ComponentResolver
   */
  resolve(type: ClassType<any>, throwIfNotFound = true): Component {
    let resolved = this._resolved.get(type);
    if (resolved) {
      return resolved;
    }
    const metadata: Component =
      ListWrapper.findLast(this._reflector.annotations(type), obj => obj instanceof Component);
    if (metadata) {
      const propertyMetadata = this._reflector.propMetadata(type);
      return this._mergeWithPropertyMetadata(metadata, propertyMetadata, type);
    } else {
      if (throwIfNotFound) {
        throw new Error(`No Component metadata found for '${stringify(type)}'.`);
      }
      return null;
    }
  }

  private _mergeWithPropertyMetadata(meta: Component, propertyMetadata: {[key: string]: any[]},
      type: ClassType<any>): Component {
    const host: {[key: string]: string} = {};
    const childs: {[key: string]: string} = {};

    Object.keys(propertyMetadata).forEach((propName: string) => {
      const hostListeners = propertyMetadata[propName]
        .filter(a => a && a instanceof HostListener);
      hostListeners.forEach(hostListener => {
        const args = hostListener.args || [];
        host[`(${hostListener.eventName})`] = `${propName}(${args.join(',')})`;
      });
      const childListeners = propertyMetadata[propName]
        .filter(a => a && a instanceof ChildListener);
      childListeners.forEach(childListener => {
        const args = childListener.args || [];
        childs[`${childListener.selector};(${childListener.eventName})`] =
          `${propName}(${args.join(',')})`;
      });
    });

    const resolved = /*new Component*/({
      selector: meta.selector,
      host: meta.host ? StringMapWrapper.merge(meta.host, host) : host,
      childs: meta.childs ? StringMapWrapper.merge(meta.childs, childs) : childs,
      providers: meta.providers,
      components: meta.components
    });
    this._resolved.set(type, resolved);
    return resolved;
  }
}
