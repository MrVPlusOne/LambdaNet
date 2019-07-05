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
function f3(c: Window, x: number, y: boolean): void {
  x = 5;
  y = true;
  c = window;
}

let x1: number;
let y1: boolean;
let c1: Window;
let r1: number[] = f1(x1, y1, c1);

let x2: number;
let y2: boolean;
let c2: Window;
let r2: string = f2(y2, x2, c2);

let x3: number;
let y3: boolean;
let c3: Window;
let r3: void = f3(c3, x3, y3);