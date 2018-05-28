
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

