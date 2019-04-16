import { stringify } from '../facade/lang';
import { Renderer } from '../render';
import { Injector } from '../di/injector';
import { resolveReflectiveProviders, ReflectiveDependency } from '../di/reflective_provider';
import { ReflectiveKey } from '../di/reflective_key';
import { Provider } from '../di/provider';
import { ElementRef } from './element_ref';
import { ViewContainerRef } from './view_container_ref';
import { createViewContainerData, createInjector } from './refs';
import {
  ViewData, ProviderData, NodeDef, NodeFlags, DepDef, DepFlags,
  asProviderData
} from './types';

const NOT_CREATED = new Object();

// tslint:disable:variable-name
const _tokenKeyCache = new Map<any, string>();
const RendererTokenKey = tokenKey(Renderer);
const ElementRefTokenKey = tokenKey(ElementRef);
const ViewContainerRefTokenKey = tokenKey(ViewContainerRef);
// const ChangeDetectorRefTokenKey = tokenKey(ChangeDetectorRef);
const InjectorRefTokenKey = tokenKey(Injector);
// tslint:enable:variable-name

export function tokenKey(token: any): string {
  let key = _tokenKeyCache.get(token);
  if (!key) {
    key = stringifyToken(token); // + '_' + _tokenKeyCache.size;
    _tokenKeyCache.set(token, key);
  }
  return key;
}

function stringifyToken(token: any): string {
  if (token instanceof ReflectiveDependency) {
    return token.key.displayName;
  }
  if (token instanceof ReflectiveKey) {
    return token.displayName;
  }
  return stringify(token);
}

export function resolveDep(view: ViewData, depDef: DepDef, allowPrivateServices: boolean,
  notFoundValue = Injector.THROW_IF_NOT_FOUND): any {

  if (depDef.flags & DepFlags.Value) {
    return depDef.token;
  }
  const startView = view;
  if (depDef.flags & DepFlags.Optional) {
    notFoundValue = null;
  }

  const tokenKey = depDef.tokenKey;

  if (depDef.flags & DepFlags.SkipSelf) {
    allowPrivateServices = false;
    view = view.parent;
  }

  while (view) {
    let def = view.def;
    if (def) {
      switch (tokenKey) {
        case RendererTokenKey:
          return view.renderer;
        case ElementRefTokenKey:
          return new ElementRef(view.renderElement);
        case ViewContainerRefTokenKey:
          return view.viewContainer || view.viewContainerParent;
        // case ChangeDetectorRefTokenKey: {
        //   let cdView = findCompView(view, elDef, allowPrivateServices);
        //   return createChangeDetectorRef(cdView);
        // }
        case InjectorRefTokenKey:
          return createInjector(view);
        default:
          const providerDef =
            (allowPrivateServices ? def.allProviders : def.publicProviders)[tokenKey];
          if (providerDef) {
            const providerData = asProviderData(view, providerDef.index);
            if (providerData.instance === NOT_CREATED) {
              providerData.instance = _createProviderInstance(view, providerDef);
            }
            return providerData.instance;
          }
      }
    }
    view = view.parent;
  }
  return startView.root.injector.get(depDef.token, notFoundValue);
}


function _createProviderInstance(view: ViewData, def: NodeDef): any {
  // private services can see other private services

  const allowPrivateServices = (def.flags & NodeFlags.PrivateProvider) > 0;
  const providerDef = def.provider;
  let deps: any[] = [];
  if (providerDef.deps) {
    deps = providerDef.deps.map(d => resolveDep(view, d, allowPrivateServices));
  }
  return providerDef.factory(...deps);
}


export function createProviderInstance(view: ViewData, def: NodeDef): any {
  return def.flags & NodeFlags.LazyProvider ? NOT_CREATED : _createProviderInstance(view, def);
}


function callProviderLifecycles(view: ViewData, index: number, lifecycles: NodeFlags) {
  const provider = asProviderData(view, index).instance;
  if (provider === NOT_CREATED) {
    return;
  }
  if (lifecycles & NodeFlags.OnDestroy) {
    provider.ngOnDestroy();
  }
}
