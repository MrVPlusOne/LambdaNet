export class List<A> {
  constructor(
    public bits: number
  ) {}

  [5](): A {

  }
}

new List(1)[5]();