type nonrec t = Some of int | None

type nonrec t2 = Pair of int * int | Triple of int * float * int

let x = Some 33
let y = x
(* should fail *)
let my_pair = Pair (3, 4, 5.6)
