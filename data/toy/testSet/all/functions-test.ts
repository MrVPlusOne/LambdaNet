function f1(x: boolean, z: Window, y: Event): number {
  x = true;
  z = window;
  y = Event.prototype;
  return 1;
}
function f2(x: number, c: Window, y: boolean, k: number[]): string {
  x = 5;
  y = true;
  c = window;
  k = [5];
  return 'a';
}

// tests any prediction
function f3(x: any) {
  return x
}

let x1: boolean;
let y1: Window;
let c1: Event;
let r1: number = f1(x1, y1, c1);

let x2: Window;
let y2: number;
let c2: boolean;
let w2: number;
let r2: string = f2(y2, x2, c2, w2);


function g1(x: number, c: Event, y: boolean): number {}
g1(1,Event.prototype, true) + 1;

function g3(y: Array<number>, x: string): void {}
g3([1], 'a');
