type 'a t = Some of 'a | None

type t2 = Pair of int * int | Triple of int * float * int
type 'a t3 = 'a t

let x: int t3 = Some 33
let y = x

let v = match x with
  | Some z -> z
  | None -> 34

let default = function
 | Some v -> v 
 | None -> false

let (a,b,c) = (3,4,5)
