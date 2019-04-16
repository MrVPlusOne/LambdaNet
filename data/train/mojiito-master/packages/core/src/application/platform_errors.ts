import { BaseError } from '../facade/error';
import { stringify } from '../facade/lang';
import { ClassType } from '../type';

export class PlatformAlreadyExistsError extends BaseError {
  constructor() {
    super(`A platform already exists. Destroy it first before creating this one.`);
  }
}
