const x1: number = 1;
const x2: boolean = true;
const x3: string = "s";

class Foo{
  x4: number;
  x5: boolean;
  x6: string;

  constructor(){
    this.x4 = 2;
    this.x5 = true;
    this.x6 = "x";
  }
}

const x7: Foo = new Foo();
let x8: number = 1+1;
x8 = 3;
const x9: string = x3;
const x10: boolean = x7.x5;

class Bar{
  z1;
  x11: number = 3;
}

const x12 = new Bar();
const x13: number = x12.x11;
let x14: any = 1;
x14 = "a";