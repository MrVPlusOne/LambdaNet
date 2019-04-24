function binarySum(A:number[], B:number[]) {
    var i:number = 0;
    var n:number = A.length;
    var C:number[] = [];
    A = A.slice(0).reverse();
    B = B.slice(0).reverse();
    while (i < n) {
        let carryOn = C[i];
        C[i] = A[i] ^ B[i] ^ carryOn;
        C[i + 1] = (A[i] & B[i]) | (A[i] & carryOn) | (B[i] & carryOn);
        i++;
    }
    return C.reverse();
}

export default binarySum;