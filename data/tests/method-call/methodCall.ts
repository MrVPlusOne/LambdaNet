class Add {
    add(x: number, y: number): number {
        return x + y;
    }
}

class AddWithAnonymousFunction extends Add {
    constructor() {
        super()
        this.add = function(x: number, y: number): number {
            return x + y;
        }
    }
}

let adder = new Add()
let anon = new AddWithAnonymousFunction()
let a = 1
let b = 3
let c = adder.add(a, b)
let d = anon.add(a, b)