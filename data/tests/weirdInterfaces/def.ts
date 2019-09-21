// Goal:
// 1) Get rid of empty interfaces from our prediction space.
// 2) Handle empty class single inheritance as equality

export interface ISnapshottable<S> {}

export interface IType<S, T> {
  name: string
  is(thing: any): thing is S | T
  create(snapshot?: S, environment?: any): T
  isType: boolean
  describe(): string
  Type: T
  SnapshotType: S

}

export interface INode {
  readonly type: IType<any, any>
  readonly storedValue: any
  readonly path: string
  readonly isRoot: boolean
  die(): void
}

export interface ISimpleType<T> extends IType<T, T> {}