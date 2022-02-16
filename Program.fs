//3.1
type aExp =
    | N of int // Integer value
    | Add of aExp * aExp // Addition
    | Sub of aExp * aExp // Subtraction
    | Mul of aExp * aExp // Multiplication

let (.+.) a b = Add (a, b)
let (.-.) a b = Sub (a, b)
let (.*.) a b = Mul (a, b)

let rec arithEvalSimple =
    function
    | N b -> b
    | Add (b,c) -> (arithEvalSimple b) + (arithEvalSimple c)
    | Sub (b,c) -> (arithEvalSimple b) - (arithEvalSimple c)
    | Mul (b,c) -> (arithEvalSimple b) * (arithEvalSimple c)

//3.2



//TESTING
let test expected actual =
    if expected = actual then
        "Passed"
    else
        sprintf "Failed: Expected: %A Actual: %A" expected actual

[<EntryPoint>]
let main args =
    printfn "3.1 arithEvalSimple a1: %s" (test 42 (arithEvalSimple (N 42)))
    printfn "3.1 arithEvalSimple a2: %s" (test 3 (arithEvalSimple (N 4 .+. (N 5 .-. N 6))))
    printfn "3.1 arithEvalSimple a3: %s" (test 42 (arithEvalSimple (N 4 .*. N 2 .+. N 34)))
    printfn "3.1 arithEvalSimple a4: %s" (test 204 (arithEvalSimple ((N 4 .+. N 2) .*. N 34)))
    printfn "3.1 arithEvalSimple a5: %s" (test 72 (arithEvalSimple (N 4 .+. (N 2 .*. N 34))))
    0