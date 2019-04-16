import { PlatformAlreadyExistsError } from './platform_errors';
import { Injector } from '../di/injector';
import { InjectionToken } from '../di/injection_token';
import { Provider } from '../di/provider';
import { ReflectiveInjector } from '../di/reflective_injector';
import { ClassType } from '../type';
import { ComponentFactory } from '../component/factory';

let _platform: PlatformRef;

export abstract class PlatformRef {
  abstract bootstrapComponent<C>(component: ClassType<C>): void
  abstract get injector(): Injector;
  abstract onDestroy(callback: () => void): void;
  abstract destroy(): void;
  abstract get destroyed(): boolean;
}

export function getPlatform(): PlatformRef {
  return _platform && !_platform.destroyed ? _platform : null;
}

export function createPlatformFactory(providers: Provider[] = []):
  (extraProviders?: Provider[]) => PlatformRef {
  return (extraProviders: Provider[] = []) => {
    if (getPlatform()) {
      throw new PlatformAlreadyExistsError();
    }

    const injector = ReflectiveInjector.resolveAndCreate(providers.concat(extraProviders));
    _platform = injector.get(PlatformRef);
    return _platform;
  };
}
