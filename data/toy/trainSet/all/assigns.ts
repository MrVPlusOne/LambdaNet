let num1: number = 1;
let bool1: boolean = true;
let str1: string = "s";
let window1: Window = window;

class C2{
  num2: number;
  bool2: boolean;
  str2: string;

  constructor(){
    this.num1 = 2;
    this.bool2 = true;
    this.str1 = "x";
  }
}

class C3{
  e1: Event = Event.prototype;
  bool2: boolean = true;
}

class C1 {
  num3: number;
  array1: number[];

  constructor(){
    this.num3 = 1;
    this.array1 = [1];
  }
}

class C4{
  window5: Window;
  str5: string;
}

class C5{
  event1: Event;
  array2: boolean[];
}


let c1: C1 = new C1();
let c2: C2 = new C2();
let c3: C3 = new C3();
let c4: C4 = new C4();
let c5: C5 = new C5();


let c21 = new C2();
let n: number = c21.num2;
let bool6: boolean = c21.bool2;

new C4().window5 = window;
new C4().str5 = "1";

new C5().event1 = Event.prototype;
new C5().array2 = [true];

let x14: any = 1;
x14 = "a";
let xB: any = true;
xB = Event.prototype;

let x15: string = 'a';
x15 = "a";