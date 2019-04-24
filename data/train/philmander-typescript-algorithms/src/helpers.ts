let chart = undefined;

export function generateArray(len: number, sort = true) {
    let arr = [];
    for(let i = 0; i < len; i++) {
        arr.push(i);
    }
    if(!sort) {
        arr = shuffle(arr);
    }
    return arr;
}


export function shuffle(array:Array<number>) {
    let counter = array.length;

    while (counter > 0) {
        let index = Math.floor(Math.random() * counter);
        counter--;
        let tmp = array[counter];
        array[counter] = array[index];
        array[index] = tmp;
    }

    return array;
}

export function swap(arr:Array<number>, i1:number, i2:number): void {
    const tmp = arr[i2];
    arr[i2] = arr[i1];
    arr[i1] = tmp;
}

export function runBenchmark(suite) {
    let results = [];
    suite.on('cycle', (ev) => {
        let t = ev.target;
        results.push( Math.round(t.stats.mean * 1000000000));
    });
    suite.on('complete', (ev) => {
        console.log(ev.currentTarget.name);
        console.log(results);
        console.log(chart(results, {
            width: 40,
            height: 20,
            pointChar: '█',
            negativePointChar: '░'
        }));
    });
    suite.run({ async: true});
}
