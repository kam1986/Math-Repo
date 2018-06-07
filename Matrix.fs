type 'B Tree =
    | Leaf
    | Node of 'B Tree * 'B * 'B Tree
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



type 'M Matrix = 
    | M of 'M Tree * int * int // normal matrix
    | T of 'M Tree * int * int // carrier for a transposed matrix
    // Because we carry we can transpose a matrix in constant time.
    // all operations are still in the same time as for normal matrices.
    with 
        static member build (lst : 'b list list) =
                let rec row = function
                    | ([], _)                       -> Leaf
                    | (x::xs, submatrix : 'b Tree) -> Node(submatrix, x, row (xs, submatrix.Right))
    
                let rec col = function
                    | ([], _)                        -> Leaf
                    | (x::xs, submatrix : 'b Tree ) -> row (x, col (xs, submatrix.Right))
    
                M (col (lst, Leaf), lst.Length, lst.Head.Length)     
        
        static member Transpose = function
            | M (m, h, w) -> T(m, w ,h) // transposion of a normal matrix
            | T (m, h, w) -> M(m, w, h) // transposion of a transposed matrix
       
        static member op_Equality (M1, M2) = 
            match M1, M2 with
            // = for normal matirces
            | M(m1, h1, w1), M(m2, h2, w2) ->
                let rec row m1 m2 =
                    match m1, m2 with
                    | Leaf, _
                    | _, Leaf -> true
                    | _       -> 
                        m1.Key.Value = m2.Key.Value && row m1.Right m2.Right
                let rec col m1 m2 =
                    match m1, m2 with
                    | Leaf, _
                    | _, Leaf -> true
                    | _       -> 
                        row m1 m2 && col m1.Left m2.Left
                  
                if h1 <> h2 || w1 <> w2 then
                    false
                else
                    col m1 m2
            
            // = for transposed matrices
            | T(m1, h1, w1), T(m2, h2, w2) ->
                let rec row m1 m2 =
                    match m1, m2 with
                    | Leaf, _
                    | _, Leaf -> true
                    | _       -> 
                        m1.Key.Value = m2.Key.Value && row m1.Right m2.Right
                let rec col m1 m2 =
                    match m1, m2 with
                    | Leaf, _
                    | _, Leaf -> true
                    | _       -> 
                        row m1 m2 && col m1.Left m2.Left
                  
                if h1 <> h2 || w1 <> w2 then
                    false
                else
                    col m1 m2
            
            // = first case of one matrix being normal and the other being transposed
            | T(m1, h1, w1), M(m2, h2, w2) ->
                let rec row m1 m2 =
                    match m1, m2 with
                    | Leaf, _
                    | _, Leaf -> true
                    | _       -> 
                        m1.Key.Value = m2.Key.Value && row m1.Left m2.Left
                let rec col m1 m2 =
                    match m1, m2 with
                    | Leaf, _
                    | _, Leaf -> true
                    | _       -> 
                        row m1 m2 && col m1.Left m2.Left
                  
                if h1 <> w2 || w1 <> h2 then
                    false
                else
                    col m1 m2

            // = second case
            | M(m1, h1, w1), T(m2, h2, w2) ->
                let rec row m1 m2 =
                    match m1, m2 with
                    | Leaf, _
                    | _, Leaf -> true
                    | _       -> 
                        m1.Key.Value = m2.Key.Value && row m1.Left m2.Left
                let rec col m1 m2 =
                    match m1, m2 with
                    | Leaf, _
                    | _, Leaf -> true
                    | _       -> 
                        row m1 m2 && col m1.Left m2.Left
                  
                if h1 <> w2 || w1 <> h2 then
                    false
                else
                    col m1 m2
                  
                    
        
        // not optimal has upto two of each nodes can be minimized.
        static member ( + ) (M1, M2) =
            match (M1, M2) with
            | M(m1,h1,w1),M(m2,h2,w2) ->
                // needs to build from bottom up e.i dynamic programming. 
                let rec row m1 m2 (t : 'b Tree) =
                    match m1, m2 with
                    | (Leaf, _)
                    | (_, Leaf) -> Leaf
                    | _         -> Node(t, m1.Key.Value + m2.Key.Value, row m1.Right m2.Right t.Right)

                let rec col m1 m2 (t : 'b Tree) =
                    match m1, m2 with
                    | (Leaf, _) 
                    | (_, Leaf) -> Leaf
                    | _         -> row m1 m2 (col m1.Left m2.Left t.Left)

                if h1 = h2 && w1 = w2 then
                    M (col m1 m2 Leaf, h1, w1)
                else
                    failwith "Dimensions of"
        
            | T (m1, h1, w1), T (m2, h2, w2) ->
                // needs to build from bottom up e.i dynamic programming. 
                let rec row m1 m2 (t : 'b Tree) =
                    match m1, m2 with
                    | (Leaf, _)
                    | (_, Leaf) -> Leaf
                    | _         -> Node(t, m1.Key.Value + m2.Key.Value, row m1.Right m2.Right t.Right)

                let rec col m1 m2 (t : 'b Tree) =
                    match m1, m2 with
                    | (Leaf, _) 
                    | (_, Leaf) -> Leaf
                    | _         -> row m1 m2 (col m1.Left m2.Left t.Left)

                if h1 = h2 && w1 = w2 then
                    T (col m1 m2 Leaf, h1, w1)
                else
                    failwith "Dimensions of"
        
            | T (m1, h1, w1), M (m2, h2, w2) ->
                let rec row m1 m2 (t : 'b Tree) =
                    match m1, m2 with
                    | (Leaf, _)
                    | (_, Leaf) -> Leaf
                    | _         -> Node(t, m1.Key.Value + m2.Key.Value, row m1.Left m2.Right t.Right)

                let rec col m1 m2 (t : 'b Tree) =
                    match m1, m2 with
                    | Leaf, _
                    | _, Leaf -> Leaf
                    | _       -> row m1 m2 (col m1.Right m2.Left t.Left)
                
                if h1 = w2 && w1 = h2 then
                    M (col m1 m2 Leaf, h2, w2)
                else
                    failwith "Dimensions of"
        
            | M (m1, h1, w1), T(m2, h2, w2) ->
                    let rec row m1 m2 (t : 'b Tree) =
                        match m1, m2 with
                        | (Leaf, _)
                        | (_, Leaf) -> Leaf
                        | _         -> Node(t, m1.Key.Value + m2.Key.Value, row m1.Right m2.Left t.Right)

                    let rec col m1 m2 (t : 'b Tree) =
                        match m1, m2 with
                        | Leaf, _
                        | _, Leaf -> Leaf
                        | _       -> row m1 m2 (col m1.Left m2.Right t.Left)
                
                    if h1 = w2 && w1 = h2 then
                        M (col m1 m2 Leaf, h2, w2)
                    else
                        failwith "Dimensions of"
        

        
        static member ( - ) (M1, M2) =
            match (M1, M2) with
            | M(m1,h1,w1),M(m2,h2,w2) ->
                // needs to build from bottom up e.i dynamic programming. 
                let rec row m1 m2 (t : 'b Tree) =
                    match m1, m2 with
                    | (Leaf, _)
                    | (_, Leaf) -> Leaf
                    | _         -> Node(t, m1.Key.Value - m2.Key.Value, row m1.Right m2.Right t.Right)

                let rec col m1 m2 (t : 'b Tree) =
                    match m1, m2 with
                    | (Leaf, _) 
                    | (_, Leaf) -> Leaf
                    | _         -> row m1 m2 (col m1.Left m2.Left t.Left)

                if h1 = h2 && w1 = w2 then
                    M (col m1 m2 Leaf, h1, w1)
                else
                    failwith "Dimensions of"
        
            | T (m1, h1, w1), T (m2, h2, w2) ->
                // needs to build from bottom up e.i dynamic programming. 
                let rec row m1 m2 (t : 'b Tree) =
                    match m1, m2 with
                    | (Leaf, _)
                    | (_, Leaf) -> Leaf
                    | _         -> Node(t, m1.Key.Value - m2.Key.Value, row m1.Right m2.Right t.Right)

                let rec col m1 m2 (t : 'b Tree) =
                    match m1, m2 with
                    | (Leaf, _) 
                    | (_, Leaf) -> Leaf
                    | _         -> row m1 m2 (col m1.Left m2.Left t.Left)

                if h1 = h2 && w1 = w2 then
                    T (col m1 m2 Leaf, h1, w1)
                else
                    failwith "Dimensions of"
        
            | T (m1, h1, w1), M (m2, h2, w2) ->
                let rec row m1 m2 (t : 'b Tree) =
                    match m1, m2 with
                    | (Leaf, _)
                    | (_, Leaf) -> Leaf
                    | _         -> Node(t, m1.Key.Value - m2.Key.Value, row m1.Left m2.Right t.Right)

                let rec col m1 m2 (t : 'b Tree) =
                    match m1, m2 with
                    | Leaf, _
                    | _, Leaf -> Leaf
                    | _       -> row m1 m2 (col m1.Right m2.Left t.Left)
                
                if h1 = w2 && w1 = h2 then
                    M (col m1 m2 Leaf, h2, w2)
                else
                    failwith "Dimensions of"
        
            | M (m1, h1, w1), T(m2, h2, w2) ->
                    let rec row m1 m2 (t : 'b Tree) =
                        match m1, m2 with
                        | (Leaf, _)
                        | (_, Leaf) -> Leaf
                        | _         -> Node(t, m1.Key.Value - m2.Key.Value, row m1.Right m2.Left t.Right)

                    let rec col m1 m2 (t : 'b Tree) =
                        match m1, m2 with
                        | Leaf, _
                        | _, Leaf -> Leaf
                        | _       -> row m1 m2 (col m1.Left m2.Right t.Left)
                
                    if h1 = w2 && w1 = h2 then
                        M (col m1 m2 Leaf, h2, w2)
                    else
                        failwith "Dimensions of"
       

        // same as above
        static member ( * ) (M1, M2) =
                match M1, M2 with
                | M(m1 : 'b Tree, h1, w1), M(m2, h2, w2) ->
                    let rec mul m1 m2 acc =
                        match (m1, m2) with
                        | (Leaf, _) -> acc
                        | (_, Leaf) -> acc
                        | _         -> mul m1.Right m2.Left (m1.Key.Value * m2.Key.Value + acc)
                                             
                    // needs to build from bottom up e.i dynamic programming. 
                    let rec row m1 m2 (t : 'b Tree) =
                        match m1, m2 with
                        | (Leaf, _)
                        | (_, Leaf) -> Leaf
                        | _         -> Node(t, mul m1 m2 LanguagePrimitives.GenericZero, row m1 m2.Right t.Right)

                    let rec col m1 m2 (t : 'b Tree) =
                        match m1, m2 with
                        | (Leaf, _) 
                        | (_, Leaf) -> Leaf
                        | _         -> row m1 m2 (col m1.Left m2 t.Left)
                    
                    if w1 = h2 then
                        M(col m1 m2 Leaf, h1, w2)
                    else
                        failwith "Dimensions of"

                | T(m1 : 'b Tree, h1, w1), T(m2, h2, w2) ->
                    let rec mul m1 m2 acc =
                        match (m1, m2) with
                        | (Leaf, _) -> acc
                        | (_, Leaf) -> acc
                        | _         -> mul m1.Right m2.Left (m1.Key.Value * m2.Key.Value + acc)
                                             
                    // needs to build from bottom up e.i dynamic programming. 
                    let rec row m1 m2 (t : 'b Tree) =
                        match m1, m2 with
                        | (Leaf, _)
                        | (_, Leaf) -> Leaf
                        | _         -> Node(t, mul m1 m2 LanguagePrimitives.GenericZero, row m1 m2.Right t.Right)

                    let rec col m1 m2 (t : 'b Tree) =
                        match m1, m2 with
                        | (Leaf, _) 
                        | (_, Leaf) -> Leaf
                        | _         -> row m1 m2 (col m1.Left m2 t.Left)
                    
                    if w1 = h2 then
                        T(col m1 m2 Leaf, h1, w2)
                    else
                        failwith "Dimensions of"
                
                | _ ->
                    // since we either has M1^T * M2 or M1 * M2^T we then only
                    // need to transpose one of them and change multiplication order to get one of the first two cases.
                    M2 * Matrix<'b>.Transpose M1 
                    
        
        member M.Height =
            match M with
            | M(_, h, _) -> h
            | T(_, h, _) -> h
        
        member M.Width =
            match M with
            | M(_, w, _) -> w
            | T(_, w, _) -> w

        member M.GetTree =
            match M with
            | M(m, _, _) -> m
            | T(m, _, _) -> m

        override M.ToString() =
            match M with
            | M(m, _, _) ->
                let rec row m =
                    match m with
                    | Leaf -> ""
                    | _    -> sprintf "%s %s" (m.Key.Value.ToString()) (row m.Right)

                let rec col m =
                    match m with
                    | Leaf  -> ""
                    | _     -> sprintf "%s\n%s" (row m) (col m.Left)
                sprintf "\n%s" (col m)
            
            | T(m, _, _) ->
                let rec row m =
                    match m with
                    | Leaf -> ""
                    | _    -> sprintf "%s %s" (m.Key.Value.ToString()) (row m.Left)

                let rec col m =
                    match m with
                    | Leaf  -> ""
                    | _     -> sprintf "%s\n%s" (row m) (col m.Right)
                sprintf "\n%s" (col m)




let m33 = Matrix<int>.build [[1;2;3];[4;5;6];[7;8;9]]