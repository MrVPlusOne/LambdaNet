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
let c3: C3 = new C3();
let c4: C4 = new C4();
let r1: RegExp = /ab+c/;