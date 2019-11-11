// All: 17 JSNice: 7 LambdaNet: 15

const dateRE = /\d{8}(?!T)/g;
const dateSeperatorRE = /\-/g;
const yyyymmddRE = /(\d{4})(\d{2})(\d{2})/;

const dateTimeRE = /(\d{8})T(\d{6})Z/g;
const dateTimeSeperatorRE = /\-|\:/g;
const hhmmssRE = /(\d{2})(\d{2})(\d{2})/;

const dropMsZLength = 'YYYY-MM-DDTHH:mm:ss'.length;

const toDateTime = (rruleSet: string[]): string[] => {
    return rruleSet.map((s) => {
        return s.replace(dateRE, (dateStr) => {
            const yyyy_mm_dd = dateStr.replace(yyyymmddRE, '$1-$2-$3');
            const dateTimeStr = dateToTime(yyyy_mm_dd);
            const dropMsZ = dateTimeStr.slice(0, dropMsZLength);
            return dropMsZ.replace(dateTimeSeperatorRE, '') + 'Z'
        })
    })
};

const toDate = (rruleSet: string[]): string[] => {
    return rruleSet.map((s) => {
        return s.replace(dateTimeRE, (_, dateStr, timeStr) => {
            const yyyy_mm_dd = dateStr.replace(yyyymmddRE, '$1-$2-$3');
            const hh_mm_ss = timeStr.replace(hhmmssRE, '$1:$2:$3');
            const date = timeToDate(`${yyyy_mm_dd}T${hh_mm_ss}Z`);
            return date.replace(dateSeperatorRE, '')
        })
    })
};

function dateToTime(date: string, returnValue?: boolean) {
    const src = new Date(date);

    const ret = new Date(
        src.getUTCFullYear(),
        src.getUTCMonth(),
        src.getUTCDate()
    );
    return returnValue ? ret.valueOf() : ret.toISOString()
}

function timeToDate(date: string, returnValue?: boolean) {
    const src = new Date(date);

    const ret = new Date(Date.UTC(
        src.getFullYear(),
        src.getMonth(),
        src.getDate()
    ));
    return returnValue ? ret.valueOf() : ret.toISOString().slice(0, 10)
}

// ====
function headers2Object(headers: Headers): Object {
    const retHeaders = {};
    headers.forEach((val: any, key: any) => retHeaders[key] = val);
    return retHeaders
}

function pagination(count: number, page: number) {
    return {
        limit: count,
        skip: (count * (page - 1)),
    }
}


function forEach (target, eachFunc: (val: any, key: any) => any, inverse?: boolean) {
    let length: number;
    if (target instanceof Array) {
        length = target.length;
        if (!inverse) {
            let i = -1;
            while (++i < length) {
                if (eachFunc(target[i], i) === false) {
                    break
                }
            }
        } else {
            let i = length;
            while (i --) {
                if (eachFunc(target[i], i) === false) {
                    break
                }
            }
        }

    } else if (typeof target === 'object') {
        const keys = Object.keys(target);
        let key: string;
        length = keys.length;
        let i = -1;
        while (++i < length) {
            key = keys[i];
            if (eachFunc(target[key], key) === false) {
                break
            }
        }
    }
    return target
}


function dropEle<T>(ele: T, arr: T[]): T[] {
    forEach(arr, (_ele, pos) => {
        const isEqual = ele === _ele;
        if (isEqual) {
            arr.splice(pos, 1)
        }
        return !isEqual
    });
    return arr
}

function capitalizeFirstLetter(str?: string) {
    if (!str) {
        return null
    }
    const upper = str[0].toUpperCase();
    if (str[0] === upper) {
        return str
    }
    return upper + str.slice(1)
}
