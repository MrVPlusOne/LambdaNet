
const num1: number = 1;
const bool1: boolean = true;
const str1: string = "s";

class Foo{
  num2: number;
  bool2: boolean;
  str2: string;

  constructor(){
    this.num2 = 2;
    this.bool2 = true;
    this.str2 = "x";
  }
}

const foo1: Foo = new Foo();
const num3: number = 1+1;
const str3: string = str1;
const bool3: boolean = foo1.bool2;

class Bar{
  z1;
  num4: number = 3;
}

const bar1 = new Bar();
const num5 = bar1.num4;
let x14: any = 1;
x14 = "a";
let x15: string = 'a';
x15 = "a";