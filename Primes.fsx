
// squqreroot of a integer
let intsqrt n = (int << sqrt << float) n

let rec IsPrime p = function                        // function taking a integer p and a list of primes less then p return true or false
    | []                           -> true          // case for empty list
    | x :: _  when x > (intsqrt p) -> true          // case for first item of the list bigger then the squarerrot of p
    | x :: xs when p % x <> 0      -> IsPrime p xs  // case for when p modulo x is not zero
    | _                            -> false         // any other cases

let NthPrimes n =                                   // function which takes an intger n and returning all primes less then or equal 2n+1
    List.rev                                        // see line 21
        (List.fold                                  // see line 22
            (fun acc p ->                           // accumulator function taking [] (line 18) and the list 2::[for i in 1 .. n -> ..])
                if IsPrime p (List.rev acc) then    // if statement checking if p is a prime. are used on every item on the list above
                    p::acc                          // if p is a prime it is added to the accumulator list acc.
                else acc)                           // if p is not a prime, simply skips it.
        [] (2::[for i in 1 .. n -> 2 * i + 1]))     // input to line 13.
        // in F# the last calculation is the result, so since the List.fold returns a list of primes send it to List.rev.
        // The List.rev's result are the output of NthPrimes.
        
// A predefined function which revers a list (here the result so that the primes are sortet in increasing order).
// A predefined function which takes a accumualor function.

let T(b, B : int[], P : int[], n) =
    for i in 0 .. n-2 do
        if b > B.[i] then
            B.[i+1] <- B.[i+1] - (b - B.[i]) - 2*abs(P.[i+1] - P.[i])
            B.[i]   <- b
        elif 2*abs(P.[i+1] - P.[i]) < B.[i] - b then
            B.[i+1] <- B.[i+1]+(B.[i] - b) - 2*abs(P.[i+1] - P.[i])
            B.[i]   <- b
    B.[n-1]>=b

let B = [|21;40;80;10;20|]
let P = [|0;5;13;33;36|]

let M(B: int[], P) =
    let mutable B1 = [| for i in 1 .. (B.Length) -> 0|]
    B.CopyTo (B1,0)
    let mutable l = 0
    let mutable h = (Array.sum B) / B.Length
    while l+1 < h do
        let b = (h + l)/2
        if T(b, B1, P, B.Length) then
            l <- b             
        else
            h <- b
        B.CopyTo (B1,0)
    l


type 'B Three =
    | Leaf
    | Node of 'B Three * 'B * 'B Three
    with
        member t.Left =
            match t with
            | Leaf -> Leaf
            | Node(l,_, _) -> l
        
        member t.Right =
            match t with
            | Leaf        -> Leaf
            | Node(_,_,r) -> r

        member t.Key =
            match t with
            | Leaf -> None
            | Node(_, k, _) -> Some k 



type 'M Matrix = M of 'M Three * int * int
    with 
        static member build (lst : 'b list list) =
                let rec row = function
                    | ([], _)                       -> Leaf
                    | (x::xs, submatrix : 'b Three) -> Node(submatrix, x, row (xs, submatrix.Right))
    
                let rec col = function
                    | ([], _)                        -> Leaf
                    | (x::xs, submatrix : 'b Three ) -> row (x, col (xs, submatrix.Right))
    
                M (col (lst, Leaf), lst.Length, lst.Head.Length)

        static member ( + ) (M (m1, h1, w1), M (m2, h2, w2)) =
                let rec add m1 m2 =
                        match (m1, m2) with
                        | (Leaf, Leaf)   -> Leaf
                        | _              -> Node(add m1.Left m2.Left, m1.Key.Value + m2.Key.Value, add m1.Right m2.Right)
                        
                if h1 = h2 && w1 = w2 then
                    M (add m1 m2, h1, w1)
                else
                    failwith "Dimensions of"


let matrix1 = Matrix<int>.build [[1]];

let matrix2 = Matrix<int>.build [[1;2];[3;4]];

let matrix3 = Matrix<int>.build [[1;2;3];[4;5;6];[7;8;9]];

let matrix4 = matrix2 + matrix2
