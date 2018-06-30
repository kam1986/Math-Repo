
// squareroot as an integer
let intsqrt n = (int << sqrt << float) n

let rec IsPrime p = function                        // function taking a integer p and a list of all primes less then p return true or false
    | []                           -> true          // case for empty list
    | x :: _  when x > (intsqrt p) -> true          // case for first item of the list bigger then the squarerrot of p
    | x :: xs when p % x <> 0      -> IsPrime p xs  // case for when p modulo x is not zero
    | _                            -> false         // any other cases

let NthPrimes n =                                   // function which takes an intger n and returning all primes less then or equal 2n+1
        (List.foldBack                              // see line 21
            (fun p acc ->                           // accumulator function taking [] (line 17) and the list 2::[for i in 1 .. n -> ..])
                if IsPrime p (List.rev acc) then    // if statement checking if p is a prime. are used on every item on the list above
                    p::acc                          // if p is a prime it is added to the accumulator list acc.
                else acc)                           // if p is not a prime, simply skips it.
        (2::[for i in 1 .. n -> 2 * i + 1])) []     // input to line 13.
        // in F# the last calculation is the result, so since the List.fold returns a list of primes send it to List.rev.
        // The List.rev's result are the output of NthPrimes.
        
// A predefined function which takes a accumualor function.

// Imperative version that is alot faster
// function which given a integer and a array with all primes less then p returns the boolean value of if p is a prime
let isprime p (arr : int array) =            
    let mutable i = 0                       // index counter
    let mutable result = true               // result
    while arr.[i] <= intsqrt p && result do // loop while the entry is less then the floor of the sqrt of p or result is true
        if p % arr.[i] = 0 then             // test if p are divisible by arr.[i] if so p is no prime
            result <- false                 // set the result to false and terminate the loop
        i <- i+1                            // increment the index counter
    result                                  // returns result

let nthprimes n =                           // function which return an array of the first n primes.
    let result = [|for i in 1 .. n -> 2|]   // Initializing result array with 2 as element in each entry, size n
    let mutable p = 3                       // First odd number greater then 2
    let mutable i = 1                       // Index counter for the result array
    while i < result.Length do              // loops while index are less then the length
        if isprime p result then            // Test if p is a prime
            result.[i] <- p                 // If prime set the next prime ind the array at index i
            i <- i+1                        // Increment index counter by one
        p <- p+2                            // alle primes greater then 2 are odd, so we increment by two
    result                                  // exiting the loop and return the resulting array