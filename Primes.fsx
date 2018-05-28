
// squqreroot of a integer
let intsqrt n = (int << sqrt << float) n


let rec IsPrime p = function
    | []                           -> true
    | x :: _  when x > (intsqrt p) -> true
    | x :: xs when p % x <> 0      -> IsPrime p xs
    | _                            -> false

let NthPrimes n = 
    List.rev 
        (List.fold 
            (fun acc p -> 
                if IsPrime p (List.rev acc) then 
                    p::acc 
                else acc)
        [] (2::[for i in 1 .. n -> 2 * i + 1]))
