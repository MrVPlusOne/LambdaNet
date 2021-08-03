type NonNullable<T> = T extends undefined | null ? never : T
type X = NonNullable<number>// number
type Y = NonNullable<number | undefined>// number
type Z = NonNullable<null>// never

let x: X = 1
let y: Y = 2
let z: Z = null