class A {
    x: number
    y: boolean
}

class B {
    x: number
    y: boolean
}

let a = new A()
let b = new B()
let c: A = new B()

a.x = 1
b.y = true