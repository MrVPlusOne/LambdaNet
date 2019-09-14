class A1{
  x1: number = 1;
  x2: string = "a";
  x3: A1 = new A1();
  x4: number;
}

class B1{
  x1: string;
  x2: B1 = new B1();
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

new A1().x4 = 5;
if(new B1().x4){
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

class Loss {
  name;  // require name based reasoning
  param;  // require reason about usages
  data;

  constructor(name, param){
    this.name = name; this.param = param;
  }

  // multiple type assignments
  gradient(y){
    return this.param * y;
  }
}

// requires chain of uncertain reasoning
function training(loss, epoch=0){
  // some other code...
  loss.data = epoch;
}