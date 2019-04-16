import { Renderer, RendererFactory, RendererType } from '../render';
import { ComponentRef } from '../component/reference';
import { Injector } from '../di/injector';
import { Provider } from '../di/provider';
import { resolveReflectiveProviders } from '../di/reflective_provider';
import { ClassType } from '../type';
import { ViewContainerRef } from './view_container_ref';
import {
  ViewData, ViewState, RootData, ViewDefinition, NodeFlags, ProviderData,
  NodeData
} from './types';
import { tokenKey, createProviderInstance } from './provider';
import { createViewContainerData } from './refs';

export function createRootView(def: ViewDefinition, injector: Injector,
  rootSelectorOrNode: string | any, context?: any): ViewData {
  const rendererFactory: RendererFactory = injector.get(RendererFactory);
  const root = createRootData(injector, rendererFactory, rootSelectorOrNode);
  const view = createView(root, null, root.element, def);
  view.renderer.parse(view);
  return view;
}

export function createView(root: RootData,
  parent: ViewData, renderElement: any, def: ViewDefinition): ViewData {
  const nodes: NodeData[] = new Array(def.nodes.length);
  const view: ViewData = {
    def,
    renderElement,
    root,
    renderer: createRenderer(renderElement, def, parent, root),
    nodes,
    parent,
    viewContainerParent: undefined,
    viewContainer: undefined,
    context: undefined,
    component: undefined,
    state: ViewState.FirstCheck | ViewState.ChecksEnabled,
    disposables: undefined,
  };
  if (def.nodeFlags & NodeFlags.TypeComponent) {
    view.viewContainer = createViewContainerData(view);
  }
  createViewNodes(view);
  return view;
}

export function initView(view: ViewData, component: any, context: any) {
  view.component = component;
  view.context = context;
}

export function destroyView(view: ViewData) {
  if (view.state & ViewState.Destroyed) {
    return;
  }
  // execEmbeddedViewsAction(view, ViewAction.Destroy);
  // execComponentViewsAction(view, ViewAction.Destroy);
  // callLifecycleHooksChildrenFirst(view, NodeFlags.OnDestroy);
  if (view.disposables) {
    for (let i = 0; i < view.disposables.length; i++) {
      view.disposables[i]();
    }
  }
  destroyViewNodes(view);
  view.renderer.destroy();
  view.state |= ViewState.Destroyed;
}

function createViewNodes(view: ViewData) {
  const def = view.def;
  const nodes = view.nodes;
  let nodeData: any;
  for (let i = 0; i < def.nodes.length; i++) {
    const nodeDef = def.nodes[i];
    switch (nodeDef.flags & NodeFlags.Types) {
      case NodeFlags.TypeProvider: {
        const instance = createProviderInstance(view, nodeDef);
        nodeData = <ProviderData>{ instance };
        break;
      }
      case NodeFlags.TypeComponent: {
        const instance = createProviderInstance(view, nodeDef);
        nodeData = <ProviderData>{ instance };

        initView(view, instance, instance);
        break;
      }
    }
    nodes[i] = nodeData;
  }
}

function destroyViewNodes(view: ViewData) {
  view.renderer.destroyNode(view.renderElement);
}

function createRootData(
  injector: Injector, rendererFactory: RendererFactory, rootSelectorOrNode: any): RootData {
  const renderer = rendererFactory.createRenderer(null, null);
  let element = rootSelectorOrNode;
  if (typeof rootSelectorOrNode === 'string') {
    element = renderer.selectRootElement(rootSelectorOrNode);
  }
  return {
    injector,
    selectorOrNode: rootSelectorOrNode,
    element,
    rendererFactory,
    renderer
  };
}

function viewDef(publicProviders: Provider[], componentProvider: any): ViewDefinition {
  var viewDef: any = {};
  // resolve public providers
  const publicProv: any = Object.create(null);
  if (publicProviders) {
    resolveReflectiveProviders(publicProviders).forEach(p => {
      const resolvedFactory = p.resolvedFactories[0];
      publicProv[tokenKey(p.key)] = {
        factory: resolvedFactory.factory,
        dependencies: resolvedFactory.dependencies,
        multi: p.multiProvider
      };
    });
  }
  viewDef.publicProviders = publicProv;

  // combine to all providers
  const allProviders = Object.create(publicProv);
  viewDef.allProviders = allProviders;

  // resolve component provider
  if (componentProvider) {
    const resolvedComp = resolveReflectiveProviders([componentProvider])[0];
    const resolvedCompFactory = resolvedComp.resolvedFactories[0];
    viewDef.componentProvider = {
      factory: resolvedCompFactory.factory,
      dependencies: resolvedCompFactory.dependencies,
      multi: false,
    };
    allProviders[tokenKey(resolvedComp.key)] = viewDef.componentProvider;
  }

  return viewDef;
}

export function createViewDefinitionFactory(publicProviders: Provider[], componentProvider: any) {
  return () => {
    return viewDef(publicProviders, componentProvider);
  };
}

function createRenderer(hostElement: any, viewDef: ViewDefinition,
      parentView: ViewData, root: RootData) {
    let rendererType: RendererType = viewDef.componentRendererType;
    let view = parentView;
    while (view && !rendererType) {
      rendererType = view.def.componentRendererType;
      view = view.parent;
    }

    if (!rendererType) {
      return root.renderer;
    } else {
      return root.rendererFactory.createRenderer(hostElement, rendererType);
    }
}
