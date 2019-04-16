/**
 * @license
 * Copyright Google Inc. All Rights Reserved.
 *
 * Use of this source code is governed by an MIT-style license that can be
 * found in the LICENSE file at https://angular.io/license
 */

export class StringMapWrapper {
  static merge<V>(m1: {[key: string]: V}, m2: {[key: string]: V}): {[key: string]: V} {
    const m: {[key: string]: V} = {};

    for (const k of Object.keys(m1)) {
      m[k] = m1[k];
    }

    for (const k of Object.keys(m2)) {
      m[k] = m2[k];
    }

    return m;
  }

  static equals<V>(m1: {[key: string]: V}, m2: {[key: string]: V}): boolean {
    const k1 = Object.keys(m1);
    const k2 = Object.keys(m2);

    if (k1.length != k2.length) {
      return false;
    }

    for (let i = 0; i < k1.length; i++) {
      const key = k1[i];
      if (m1[key] !== m2[key]) {
        return false;
      }
    }

    return true;
  }
}

export class ListWrapper {
  static findLast<T>(arr: T[], condition: (value: T) => boolean): T {
    for (let i = arr.length - 1; i >= 0; i--) {
      if (condition(arr[i])) {
        return arr[i];
      }
    }
    return null;
  }

  static removeAll<T>(list: T[], items: T[]) {
    for (let i = 0; i < items.length; ++i) {
      const index = list.indexOf(items[i]);
      if (index > -1) {
        list.splice(index, 1);
      }
    }
  }

  static remove<T>(list: T[], el: T): boolean {
    const index = list.indexOf(el);
    if (index > -1) {
      list.splice(index, 1);
      return true;
    }
    return false;
  }

  static equals(a: any[], b: any[]): boolean {
    if (a.length != b.length) return false;
    for (let i = 0; i < a.length; ++i) {
      if (a[i] !== b[i]) return false;
    }
    return true;
  }

  static flatten<T>(list: Array<T | T[]>): T[] {
    return list.reduce((flat: any[], item: T | T[]): T[] => {
      const flatItem = Array.isArray(item) ? ListWrapper.flatten(item) : item;
      return (<T[]>flat).concat(flatItem);
    }, []);
  }

  static forEach<T>(list: ArrayLike<T>, callback: (item: T, index: number) => void): void {
    for (let i = 0, max = list.length; i < max; i++) {
      callback.call(callback, list[i], i);
    }
  }
}
