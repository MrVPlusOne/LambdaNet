// All: 6 JSNice: 2 LamdbaNet: 5
function spliceOne (arr: Array<any>, index: number): boolean {
    // manually splice availableRooms array
    // http://jsperf.com/manual-splice
    if (index === -1 || index >= arr.length) {
        return false;
    }

    for (var i = index, len = arr.length - 1; i < len; i++) {
        arr[i] = arr[i + 1];
    }

    arr.length = len;

    return true;
}

function merge (a: any, ...objs: any[]) {
    for (let i = 0, len = objs.length; i < len; i++) {
        let b = objs[i];
        for (let key in b) {
            if (b.hasOwnProperty(key)) {
                a[key] = b[key]
            }
        }
    }
    return a;
}

function logError (err: Error): void {
    if (err) {
        console.log(err)
    }
}
