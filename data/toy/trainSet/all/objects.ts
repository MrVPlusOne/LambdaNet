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
  x1: any;
  x2: number;
}

new A1().x4 = 5;
if(new B1().x4){
  new C().x1 = undefined;
  new C().x2 = 5;
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
