//3.1
type aExp =
    | N of int // Integer value
    | V of string // Variable
    | WL // Length of the word
    | PV of aExp // Point value of character at specific word index
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
let rec arithEvalState a (map: Map<string, int>) =
    match a with
    | N b -> b
    | V v -> 
        try
            (arithEvalState (N map[v]) map)
        with _ ->
            (arithEvalState (N 0) map)
    | Add (b,c) -> (arithEvalState b map) + (arithEvalState c map)
    | Sub (b,c) -> (arithEvalState b map) - (arithEvalState c map)
    | Mul (b,c) -> (arithEvalState b map) * (arithEvalState c map)

type word = (char * int) list

let hello : word = [('H',4);('E',1);('L',1);('L',1);('O',1)]

let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_")
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_")
let arithDoubleWordScore = N 2 .*. V "_acc_"
let arithTripleWordScore = N 3 .*. V "_acc_"

//3.3
let rec arithEval a (word: word) (map: Map<string, int>) =
    match a with
    | N n -> n
    | V v -> 
        try
            (arithEval (N map[v]) word map)
        with _ ->
            (arithEval (N 0) word map)
    | WL -> word.Length
    | PV pv -> 
        let _,i = word.[(arithEval pv word map)]
        i
    | Add (b,c) -> (arithEval b word map) + (arithEval c word map)
    | Sub (b,c) -> (arithEval b word map) - (arithEval c word map)
    | Mul (b,c) -> (arithEval b word map) * (arithEval c word map)

//3.4
type cExp =
| C of char (* Character value *)
| ToUpper of cExp (* Converts lower case to upper case character, non-letters are unchanged *)
| ToLower of cExp (* Converts upper case to lower case character, non-letters are unchanged *)
| CV of aExp (* Character lookup at word index *)

let rec charEval c (word : word) (map: Map<string, int>) =
    match c with
    | C c -> c
    | ToUpper c -> System.Char.ToUpper((charEval c word map))
    | ToLower c -> System.Char.ToLower((charEval c word map))
    | CV a -> 
        let c,_ = word.[(arithEval a word map)]
        c

//3.5
type bExp =
    | TT (* true *)
    | FF (* false *)

    | AEq of aExp * aExp (* numeric equality *)
    | ALt of aExp * aExp (* numeric less than *)

    | Not of bExp (* boolean not *)
    | Conj of bExp * bExp (* boolean conjunction *)

    | IsDigit of cExp (* check for digit *)
    | IsLetter of cExp (* check for letter *)
    | IsVowel of cExp (* check for vowel *)





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
    printfn ""
    printfn "3.2 arithEvalState a6: %s" (test 5 (arithEvalState (V "x") (Map.ofList [("x", 5)])))
    printfn "3.2 arithEvalState a6: %s" (test 0 (arithEvalState (V "x") (Map.ofList [("y", 5)])))
    printfn "3.2 arithEvalState a7: %s" (test 9 (arithEvalState (N 4 .+. (V "y" .-. V "z")) (Map.ofList [("x", 4); ("y", 5)])))
    printfn "3.2 arithEvalState a7: %s" (test 3 (arithEvalState (N 4 .+. (V "y" .-. V "z")) (Map.ofList [("y", 4); ("z", 5)])))
    printfn ""
    printfn "3.3 arithEval empty: %s" (test 0 ( arithEval WL [] Map.empty ))
    printfn "3.3 arithEval hello: %s" (test 5 ( arithEval WL hello Map.empty ))
    printfn "3.3 arithEval PV: %s" (test 4 ( arithEval (PV (N 0)) hello Map.empty ))
    printfn "3.3 arithEval sls: %s" (test 1 ( arithEval arithSingleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 0)]) ))
    printfn "3.3 arithEval sls: %s" (test 43 ( arithEval arithSingleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 42)]) ))
    printfn "3.3 arithEval dls: %s" (test 2 ( arithEval arithDoubleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 0)]) ))
    printfn "3.3 arithEval dls: %s" (test 44 ( arithEval arithDoubleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 42)]) ))
    printfn "3.3 arithEval tls: %s" (test 3 ( arithEval arithTripleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 0)]) ))
    printfn "3.3 arithEval tls: %s" (test 45 ( arithEval arithTripleLetterScore hello (Map.ofList [("_pos_", 4); ("_acc_", 42)]) ))
    printfn ""
    printfn "3.4 charEval C: %s" (test 'H' ( charEval (C 'H') [] Map.empty ))
    printfn "3.4 charEval ToLower: %s" (test 'h' ( charEval (ToLower (CV (N 0))) hello Map.empty ))
    printfn "3.4 charEval ToUpper: %s" (test 'H' ( charEval (ToUpper (C 'h')) [] Map.empty ))
    printfn "3.4 charEval ToLower: %s" (test '*' ( charEval (ToLower (C '*')) [] Map.empty ))
    printfn "3.4 charEval CV: %s" (test 'O' ( charEval (CV (V "x" .-. N 1)) hello (Map.ofList [("x", 5)]) ))
    printfn ""
    printfn "3.5 : %s" (test  (  ))
    0