class A1{
  x1: number = 1;
  x2: boolean;
  x3: A1 = new A1();
}

class D{
  x1: string[];
  x3: boolean;
  x4: number;
  constructor(){
    this.x1 = ["b"];
    this.x3 = false;
  }
}

class C{
  c2: any;
  c1: number;
  x4: string;
}

new A1().x2 = true;
if(new D().x4){
  new C().c1 = 5;
  new C().c2 = undefined;
  new C().x4 = "";
}

let v1: A1 = new A1();
let v2: D = new D();
let v3: C = new C();

let y1: number;
let y2: Window;
let o2 = {u: 3, w: window};
y1 = o2.u;
y2 = o2.w;
