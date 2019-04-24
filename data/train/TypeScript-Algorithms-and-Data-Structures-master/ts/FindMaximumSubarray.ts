function FindMaximumSubarray (array: number[], left: number = 0, right: number = array.length): number[] {
    if (left === right) {
        return [left, right, left > array.length ? -Infinity : array[left]];
    } else {
        const middle = Math.floor((left + right) / 2);
        const [leftLow, leftHigh, leftSum] = FindMaximumSubarray(array, left, middle);
        const [rightLow, rightHigh, rightSum] = FindMaximumSubarray(array, middle + 1, right);
        const [crossLow, crossHigh, crossSum] = FindMaximumCrossingSubarray(array, left, middle, right);
        if (leftSum >= rightSum && leftSum >= crossSum) {
            return [leftLow, leftHigh, leftSum];
        }

        if (rightSum >= leftSum && rightSum >= crossSum) {
            return [rightLow, rightHigh, rightSum];
        }
        return [crossLow, crossHigh, crossSum];
    }
}

function FindMaximumCrossingSubarray (array: number[], left: number, middle: number, right: number) {
    let leftSum: number = -Infinity;
    let rightSum: number = -Infinity;
    let sum: number = 0;
    let maxLeft: number = middle;
    let maxRight: number = Math.min(middle + 1, array.length - 1);
    for (let i = middle; i >= left; i--) {
        sum = sum + array[i];
        if (sum > leftSum) {
            leftSum = sum;
            maxLeft = i;
        }
    }

    sum = 0;
    for (let i = middle + 1; i < right; i++) {
        sum = sum + array[i];
        if (sum > rightSum) {
            rightSum = sum;
            maxRight = i;
        }
    }

    return [maxLeft, maxRight, Math.max(rightSum, 0) + Math.max(leftSum, 0)];
}

export default FindMaximumSubarray;