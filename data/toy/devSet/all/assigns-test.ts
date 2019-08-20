let a: number = 1;
let t: boolean = true;
let s: string = "s";
let e: Event = Event.prototype;

class C2{
  str1: string;
  num1: number;

  constructor(){
    this.num1 = 2;
    this.str1 = "x";
  }
}

class C3{
  str2: string = "a";
  num4: number;

  constructor(){
    this.num4 = 4;
  }
}

class C1 {
  str5: string = "";
  num5: number = 5;
  event5: Event = Event.prototype;
  window5: Window = window;
  bool5: boolean = true;
}

class C4{
  window5: Window;
  number5: number;
}


let c1: C1 = new C1();
let c2: C2 = new C2();
let c3: C3 = new C3();
let c4: C4 = new C4();


let c21 = new C2();
let n: number = c21.num1;
let s6: string = c21.str1;

new C4().window5 = window;
new C4().number5 = 5;

let x14: any = window;
x14 = 1;
let x15: number = 5;
x15 = 5;