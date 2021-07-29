class Add {
    k: number
    constructor(k: number) {
        this.k = k
    }
    add(x: number, y: number): number {
        return x + y;
    }
}

class AddWithAnonymousFunction extends Add {
    constructor(k: number) {
        super(k)
        this.add = function(x: number, y: number): number {
            return x + y;
        }
    }
}

let adder = new Add(3)
let anon = new AddWithAnonymousFunction(2)
let a = 1
let b = 3
let c = adder.add(a, b)
let d = anon.add(a, b)
let e = adder.k
let f = anon.k