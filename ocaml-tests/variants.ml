type nonrec t = Some of int | None

type nonrec t2 = Pair of int * int | Triple of int * float * int
type nonrec t3 = t

let x: t3 = Some 33
let y = x

let v = match x with
  | Some z -> z
  | None -> 34

let (a,b,c) = (3,4,5)