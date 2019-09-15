class Matrix {
  constructor(public data: Array<number>, size: number){}

  times(matrix: Matrix): Matrix {
    return
  }
}

function printResult(a: Matrix, b: Matrix){
  return a.concat(b).times(3);
}