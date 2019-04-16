import {
  createPlatformFactory, PlatformRef, InjectionToken, RendererFactory, CORE_PROVIDERS, Provider
} from 'mojiito-core';
import { unimplemented } from './facade/error';
import { ListWrapper } from './facade/collection';
import { BrowserPlatformRef } from './platform_ref';
import { DOCUMENT } from './tokens';
import { Compiler } from './compiler/compiler';
import { DomTraverser } from './dom_traverser';
import { DomRendererFactory } from './dom_renderer';
import { ExpressionParser } from './expression/expression';
import { BindingParser } from './binding_parser';

export { DOCUMENT, DomRendererFactory, BrowserPlatformRef, ExpressionParser };

export const PLATFORM_PROVIDERS: Provider[] = [
  { provide: PlatformRef, useClass: BrowserPlatformRef },
  { provide: DOCUMENT, useValue: document },
  { provide: RendererFactory, useClass: DomRendererFactory},
  Compiler,
  ExpressionParser,
  BindingParser
];

export const platformBrowser = createPlatformFactory([PLATFORM_PROVIDERS, CORE_PROVIDERS]);
