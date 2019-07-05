class A{
  a1: number = 1;
  a2: string = "a";
  a3: A = new A();
  a4: number;
}

class B{
  b1: string;
  b2: B = new B();
  b3: string[];
  b4: boolean;
  constructor(){
    this.b1 = "b";
    this.b3 = ["a"];
  }
}

class C{
  c1: any;
  c2: number;
}

new A().a4 = 5;
if(new B().b4){
  new C().c1 = undefined;
  new C().c2 = 5;
}

let x1: number;
let x2: string;
let o1 = {a: 5, b: 'a'};
x1 = o1.a;
x2 = o1.b;

let y1: boolean;
let y2: Window;
let o2 = {a: true, b: window};
y1 = o2.a;
y2 = o2.b;
