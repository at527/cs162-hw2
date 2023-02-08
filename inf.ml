type 'a inf = Cons of (unit -> ('a * 'a inf))

let hd xs = 
  match xs with
  | Cons f -> fst (f ())

let tl xs = 
  match xs with
  | Cons f -> snd (f ())

let cons x xs =
  Cons (fun () -> (x, xs))

let rec from n =
  Cons (fun () -> (n, from (n+1)))

let rec map f xs =
  Cons (fun () -> f (hd xs), map f (tl xs))

let rec repeat x = 
  Cons (fun () -> (x, repeat (x)));;

let fib =
  let rec fib_n (n: int) : int =
    match n with 
    | 0 -> 0
    | 1 -> 1
    | n -> fib_n(n - 1) + fib_n(n - 2)
  in
  map (fib_n) (from 0)

let rec firstn n xs =
  if n = 0 then []
  else
    hd xs::firstn (n-1) (tl xs)
      
let rec interleave xs ys =
  Cons (fun () -> (hd xs), ( cons (hd ys) (interleave (tl xs) (tl ys))) )

let z = interleave ( map (fun x -> -1 * x) (from 0) ) (from 1) 

let product xs ys =
  map (fun x -> map (fun y -> (x, y)) (ys)) (xs) 

let corner n xss =
  let rows = firstn (n) (xss) in
  List.map (fun row -> firstn (n) row) (rows)

let rec diag xxs =
  let shrink xys = 
    map (fun xs -> tl xs) (tl xys) in
  Cons (fun () -> (hd (hd xxs)), (diag (shrink xxs)))