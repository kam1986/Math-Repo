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
        
        // not optimal has upto two of each nodes can be minimized.
        static member ( + ) (M (m1, h1, w1), M (m2, h2, w2)) =
                // needs to build from bottom up e.i dynamic programming. 
                let rec row m1 m2 (t : 'b Three) =
                    match m1, m2 with
                    | (Leaf, _)
                    | (_, Leaf) -> Leaf
                    | _         -> Node(t, m1.Key.Value + m1.Key.Value, row m1.Right m2.Right t.Right)

                let rec col m1 m2 (t : 'b Three) =
                    match m1, m2 with
                    | (Leaf, _) 
                    | (_, Leaf) -> Leaf
                    | _         -> row m1 m2 (col m1.Left m2.Left t.Left)

                if h1 = h2 && w1 = w2 then
                    M (col m1 m2 Leaf, h1, w1)
                else
                    failwith "Dimensions of"
        
        
        static member ( - ) (M (m1, h1, w1), M (m2, h2, w2)) =
                // needs to build from bottom up e.i dynamic programming. 
                let rec row m1 m2 (t : 'b Three) =
                    match m1, m2 with
                    | (Leaf, _)
                    | (_, Leaf) -> Leaf
                    | _         -> Node(t, m1.Key.Value - m1.Key.Value, row m1.Right m2.Right t.Right)

                let rec col m1 m2 (t : 'b Three) =
                    match m1, m2 with
                    | (Leaf, _) 
                    | (_, Leaf) -> Leaf
                    | _         -> row m1 m2 (col m1.Left m2.Left t.Left)

                if h1 = h2 && w1 = w2 then
                    M (col m1 m2 Leaf, h1, w1)
                else
                    failwith "Dimensions of"
       

        // same as above
        static member ( * ) (M (m1, h1, w1), M (m2, h2, w2)) =
                let rec mul m1 m2 acc =
                    match (m1, m2) with
                    | (Leaf, _) -> acc
                    | (_, Leaf) -> acc
                    | _         -> mul m1.Right m2.Left (m1.Key.Value * m2.Key.Value + acc) 

               
                // needs to build from bottom up e.i dynamic programming. 
                let rec row m1 m2 (t : 'b Three) =
                    match m1, m2 with
                    | (Leaf, _)
                    | (_, Leaf) -> Leaf
                    | _         -> Node(t, mul m1 m2 LanguagePrimitives.GenericZero, row m1 m2.Right t.Right)

                let rec col m1 m2 (t : 'b Three) =
                    match m1, m2 with
                    | (Leaf, _) 
                    | (_, Leaf) -> Leaf
                    | _         -> row m1 m2 (col m1.Left m2 t.Left)
                    
                if w1 = h2 then
                    M(col m1 m2 Leaf, h1, w2)
                else
                    failwith "Dimensions of"
         
        static member GetTree (M(m,h,w)) = m

        static member ToString (M(m, h, w)) =
            let rec row m =
                match m with
                | Leaf -> ""
                | _    -> sprintf "%s %s" (m.Key.Value.ToString()) (row m.Right)

            let rec col m =
                match m with
                | Leaf  -> ""
                | _     -> sprintf "%s\n%s" (row m) (col m.Left)
            sprintf "\n%s" (col m)

let matrix1 = Matrix<int>.build [[1]];

let matrix2 = Matrix<int>.build [[1;2];[3;4]];



let matrix3 = Matrix<int>.build [[1;2;3];[4;5;6];[7;8;9]];

let matrix4 = matrix2 + matrix2

let matrix5 = matrix4 - matrix4

let matrix6 = matrix2 * matrix2;