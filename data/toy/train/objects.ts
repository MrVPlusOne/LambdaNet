class A{
  x1: number = 1;
  x2: string = "a";
  x3: A = new A();
  x4: number;
}

class B{
  x1: string;
  x2: B = new B();
  x3: string[];
  x4: boolean;
  constructor(){
    this.x1 = "b";
    this.x3 = ["a"];
  }
}

class C{
  c1: any;
  c2: number;
}

new A().x4 = 5;
if(new B().x4){
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
