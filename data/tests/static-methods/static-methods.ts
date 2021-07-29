class Test {
  constructor(public x: number) {
  }
  static mag(v: Test) { return Math.sqrt(v.x * v.x); }

  static norm(v: Test) {
    var mag = Test.mag(v);
    var m1 = Test.mag(v);
    var div = (mag === 0) ? Infinity : 1.0 / m1;
    return div
  }
}
