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

class FieldTest2{
  f1: boolean;
  f2: number;
  f4: FieldTest2;

  constructor(){
    this.f1 = true;
    this.f2 = 1;
    this.f4 = this;
  }
}

class FunctionTest{
  constructor(n2: number){
    let n = 1;
    let e = undefined;
    if(this.f1(1, n)){
      e = this.g1(new Node(e));
    }else{
      this.id(1+n2);
      this.id(true) && true;
      this.id(new Array()).length;
    }
  }

  f1(x: number, y: number): boolean {}

  g1(x: Node): any {}

  id<T>(x: T): T {}
}

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

function f<T,U>(x: {plus: (_:T)=>U} & T): U {
  return x.plus(x)
}

class Number{
  constructor(public value: number){}

  public plus(n: Number): Number { throw new Error() }
}

f(new Number(5));

function g<T>(x: {plus: (_:T)=>T} & T): T {
  return x.plus(x)
}