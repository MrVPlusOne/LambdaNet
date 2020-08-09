class FieldTest{
  f1: number;
  f2: any;
  f3: boolean;
  f4: FieldTest;

  constructor(){
    this.f1 = 1;
    this.f2 = undefined;
    this.f3 = true;
    this.f4 = this;
  }
}

interface A {
  foo: number;
  bar: number;
}

type B = {foo: number; bar: number};

function foo(x: {x1: string, x2: number}){
  const {x1: s, x2} = x;
  return s
}


let x: A = {foo: 5, bar: 4};

class FieldTest2{
  f1: boolean = false;
  f2: number;
  f4: FieldTest2;

  constructor(){
    this.f2 = 1;
    this.f4 = this;
  }
}

class FunctionTest{
  static B: boolean = true;

  f1(x: number, y: number): boolean {}

  g1(x: Node): any {}

  id<T>(x: T): T {}

  constructor(n2: number){
    let n = 1;
    let e = undefined;
    if(this.f1(1, n)){
      e = this.g1(new Node(e));
    }else{
      this.id(1+n2);
      FunctionTest.B = this.id(true);
      this.id(new Array()).length;
    }
  }
}

const lambdaTest = (x: number) => x + 1

export class Node{
  element: any;
  next: Node;
  prev: Node;

  constructor(element: any){
    this.element = element;
  }
}

class LinkedList{
  _first: Node;
  _last: Node;
  _size: number;

  constructor(){
    this._size = 0;
  }

  size(): number{
    return this._size;
  }

  isEmpty():boolean{
    return ! this._first;
  }

  push(element: any): void {
    return this._insert(element, true)
  }

  _insert(element: any, atTheEnd: boolean): void {
    let newNode = new Node(element);
    if(!this._first){
      this._first = newNode;
      this._last = newNode;
    }else if(atTheEnd){
      let oldLast = this._last;
      this._last = newNode;
      newNode.prev = oldLast;
      oldLast.next = newNode;
    } else{
      let oldFirst = this._first;
      this._first = newNode;
      newNode.next = oldFirst;
      oldFirst.prev = newNode;
    }
    this._size = this._size + 1;
  }
}
