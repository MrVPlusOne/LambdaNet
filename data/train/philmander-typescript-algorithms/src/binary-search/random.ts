export function withinRange(min: number, max: number) {
    return Math.floor(Math.random() * (max - min + 1) + min);
}

export function generateArray(len: number, randomLo:number, randomHi:number) {
    let arr = [];
    for(let i = 0; i < len; i++) {
        arr.push(withinRange(randomLo, randomHi));
    }
    arr.sort();
    return arr;
}
