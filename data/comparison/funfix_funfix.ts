// All: 10 JSNice: 3 LambdaNet: 8

function arrayBSearchInsertPos<A>(array: Array<A>, f: (a: A) => number):
    ((search: number) => number) {

    return search => {
        let minIndex = 0
        let maxIndex = array.length - 1

        while (minIndex <= maxIndex) {
            const index = (minIndex + maxIndex) / 2 | 0
            const current = f(array[index])
            const next = index + 1 <= maxIndex ? f(array[index + 1]) : undefined

            if (current <= search && (next === undefined || search < next)) {
                return index + 1
            } else if (current <= search) {
                minIndex = index + 1
            } else { /* if (current > search) */
                maxIndex = index - 1
            }
        }

        return 0
    }
}

/**
 * Internal utility that builds an iterator out of an `Iterable` or an `Array`.
 *
 * @hidden
 */
function iterableToArray<A>(values: Iterable<A>): A[] {
    if (!values) return []
    if (Object.prototype.toString.call(values) === "[object Array]")
        return values

    const cursor = values[Symbol.iterator]()
    const arr: A[] = []

    while (true) {
        const item = cursor.next()
        if (item.value) arr.push(item.value)
        if (item.done) return arr
    }
}

/**
 * Natural log of 2.
 * @hidden
 */
const lnOf2 = Math.log(2)

/**
 * Calculates the base 2 logarithm of the given argument.
 *
 * @hidden
 * @return a number such that 2^nr^ is equal to our argument.
 */
function log2(x: number): number {
    return Math.log(x) / lnOf2
}

/**
 * The maximum number that can be returned by {@link nextPowerOf2}.
 * @hidden
 */
const maxPowerOf2: number = 1 << 30

/**
 * Given a positive integer, returns the next power of 2 that is bigger
 * than our argument, or the maximum that this function can
 * return which is 2^30^ (or 1,073,741,824).
 *
 * @return an integer that is a power of 2, that is bigger or
 *        equal with our argument and that is "closest" to it.
 *
 * @hidden
 */
function nextPowerOf2(nr: number): number {
    if (nr < 0) throw new IllegalArgumentError("nr must be positive")
    const bit = Math.ceil(log2(nr))
    return 1 << (bit > 30 ? 30 : (bit & bit))
}
