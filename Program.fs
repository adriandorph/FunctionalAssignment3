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
            (arithEvalState (N map.[v]) map)
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
            (arithEval (N map.[v]) word map)
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

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.=.) a b = AEq (a, b)
let (.<.) a b = ALt (a, b)
let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b) (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b) (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

let isVowel (c:char) = 
    "aeiouyAEIOUY".Contains(c)

let rec boolEval b (word : word) (map : Map<string, int>) =
    match b with
    | TT -> true
    | FF -> false
    
    | AEq (a1,a2) -> (arithEval a1 word map) = (arithEval a2 word map)
    | ALt (a1, a2) -> (arithEval a1 word map) < (arithEval a2 word map)

    | Not b -> not (boolEval b word map)
    | Conj (b1, b2) -> (boolEval b1 word map) && (boolEval b2 word map)

    |IsDigit c -> System.Char.IsDigit(charEval c word map)
    |IsLetter c -> System.Char.IsLetter(charEval c word map)
    |IsVowel c -> isVowel (charEval c word map)

//3.6
let isConsonant c =
    Not (IsVowel c)

type stmnt =
| Skip (* does nothing *)
| Ass of string * aExp (* variable assignment *)
| Seq of stmnt * stmnt (* sequential composition *)
| ITE of bExp * stmnt * stmnt (* if-then-else statement *)
| While of bExp * stmnt (* while statement *)

//3.7
let rec evalStmnt (s:stmnt) (word:word) (map:Map<string, int>) =
    match s with
    | Skip -> map
    | Ass (s,i) -> map.Add(s,(arithEval i word map)) 
    | Seq (s1,s2) -> (evalStmnt s2 word (evalStmnt s1 word map))
    | ITE (guard, s1, s2) -> if (boolEval guard word map) then (evalStmnt s1 word map) else (evalStmnt s2 word map)
    | While (guard, s) -> if (boolEval guard word map) then (evalStmnt (While (guard, s)) word (evalStmnt s word map)) else map

//TESTING
let test expected actual =
    if expected = actual then
        "Passed"
    else
        sprintf "Failed: Expected: %A Actual: %A" expected actual

[<EntryPoint>]
let main args =
    printfn "GREEN"
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
    printfn "3.5 boolEval TT: %s" (test true ( boolEval TT [] Map.empty ))
    printfn "3.5 boolEval FF: %s" (test false ( boolEval FF [] Map.empty ))
    printfn "3.5 boolEval TT: %s" (test true ( boolEval ((V "x" .+. V "y") .=. (V "y" .+. V "x")) [] (Map.ofList [("x", 5); ("y", 7)]) ))
    printfn "3.5 boolEval FF: %s" (test false ( boolEval ((V "x" .+. V "y") .=. (V "y" .-. V "x")) [] (Map.ofList [("x", 5); ("y", 7)]) ))
    printfn "3.5 boolEval IsLetter: %s" (test true ( boolEval (IsLetter (CV (V "x"))) hello (Map.ofList [("x", 4)]) ))
    printfn "3.5 boolEval IsLetter: %s" (test false ( boolEval (IsLetter (CV (V "x"))) (('1', 0)::hello) (Map.ofList [("x", 0)]) ))
    printfn "3.5 boolEval IsDigit: %s" (test false ( boolEval (IsDigit (CV (V "x"))) hello (Map.ofList [("x", 4)]) ))
    printfn "3.5 boolEval IsDigit: %s" (test true ( boolEval (IsDigit (CV (V "x"))) (('1', 0)::hello) (Map.ofList [("x", 0)]) ))
    printfn ""
    printfn "YELLOW"
    printfn "3.6 isConsonant: %s" (test true ( boolEval (isConsonant (C 'H')) [] Map.empty ))
    printfn "3.6 isConsonant: %s" (test true ( boolEval (isConsonant (C 'h')) [] Map.empty ))
    printfn "3.6 isConsonant: %s" (test false ( boolEval (isConsonant (C 'A')) [] Map.empty ))
    printfn "3.6 isConsonant: %s" (test true ( boolEval (isConsonant (CV (V "x"))) hello (Map.ofList [("x", 0)]) ))
    printfn "3.6 isConsonant: %s" (test false ( boolEval (isConsonant (CV (V "x"))) hello (Map.ofList [("x", 1)]) ))
    printfn ""
    printfn "3.7 evalStmnt: %s" (test (Map<string, int>[]) ( evalStmnt Skip [] Map.empty ))
    printfn "3.7 evalStmnt: %s" (test (Map<string, int>[("x", 5)]) ( evalStmnt (Ass ("x", N 5)) [] Map.empty ))
    printfn "3.7 evalStmnt: %s" (test (Map<string, int>[("x", 5); ("y", 7)]) ( evalStmnt (Seq (Ass ("x", WL), Ass ("y", N 7))) hello Map.empty ))
    printfn "3.7 evalStmnt: %s" (test (Map<string, int>[("x", 1)]) ( evalStmnt (ITE (WL .>=. N 5, Ass ("x", N 1), Ass ("x", N 2))) hello Map.empty ))
    printfn "3.7 evalStmnt: %s" (test (Map<string, int>[("x", 2)]) ( evalStmnt (ITE (WL .<. N 5, Ass ("x", N 1), Ass ("x", N 2))) hello Map.empty ))
    printfn "3.7 evalStmnt: %s" (test (Map<string, int>[("x", 6); ("y", 15)]) ( evalStmnt (While (V "x" .<=. WL, Seq (Ass ("y", V "y" .+. V "x"), Ass ("x", V "x" .+. N 1)))) hello Map.empty ))
    printfn "3.7 evalStmnt: %s" (test (Map<string, int>[("x", 6); ("y", 112)]) ( evalStmnt (While (V "x" .<=. WL, Seq (Ass ("y", V "y" .+. V "x"), Ass ("x", V "x" .+. N 1)))) hello (Map.ofList [("x", 3); ("y", 100)]) ))
    0