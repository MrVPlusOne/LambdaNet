function f1(x: number, y: boolean, c: Window): number[] {
  x = 5;
  y = true;
  c = window;
  return [1];
}
function f2(y: boolean, x: number, c: Window): string {
  x = 5;
  y = true;
  c = window;
  return 'a';
}
function f3(c: Event, x: number, y: boolean): void {
  x = 5;
  y = true;
  c = Event.prototype;
}

function f4(c: boolean, x: number): Event {
  c = true;
  x = 5;
  return Event.prototype;
}

let a1: number;
let a2: boolean;
let a3: Window;
let a4: number[] = f1(a1, a2, a3);

let b1: boolean;
let b2: number;
let b3: Window;
let b4: string = f2(b1, b2, b3);

let z1: Event;
let z2: number;
let z3: boolean;
let z4: void = f3(z1, z2, z3);


function g1(x: number, y: boolean, c: Window): string {}
g1(1,true, window).charAt(5);

function g2(y: boolean, c: Window, x: number): number {}
g2(true, window, 1) + 1;

function g3(y: Array<number>, c: number, x: string): void {}
g3([1],2, 'a');