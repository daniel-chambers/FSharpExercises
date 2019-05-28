open System

let notImplemented () = raise <| NotImplementedException ()


type MyList<'a> =
  | Cons of 'a * MyList<'a>
  | EndOfList


// The repeat function creates a list that contains the `x` parameter `count` times.
// eg. repeat 3 "a" = Cons ("a",Cons ("a",Cons ("a",EndOfList)))
// Implement this function using recursion.
let rec repeat : int -> 'a -> MyList<'a> =
  fun count x ->
    if count = 0 then
      EndOfList
    else
      Cons (x, repeat (count - 1) x)


// Is your function tail recursive? You can check by using a large count number
// like 30000 and seeing if it blows the stack.
// Make your implementation tail recursive below. If it is already tail recursive,
// copy your solution here and edit your previous one to be _not_ tail recusive
// for comparison.
let repeatTailRecursive : int -> 'a -> MyList<'a> =
  fun count x ->
    let rec step count' lst =
      if count' = 0 then
        lst
      else
        step (count' - 1) (Cons (x, lst))

    step count EndOfList


// Implement `countDownFrom` which counts down to zero from the number you pass in
// using recursion.
// eg. countDownFrom 3 = Cons (3,Cons (2,Cons (1,Cons (0,EndOfList))))
let rec countDownFrom : int -> MyList<int> =
  fun start ->
    if start = -1 then
      EndOfList
    else
      Cons (start, countDownFrom (start - 1))


// Is your `countDownFrom` tail recursive? If not, make it so below.
// If it is already tail recursive, copy your solution here and edit
// your previous one to be _not_ tail recusive for comparison.
let countDownFromTailRecursive : int -> MyList<int> =
  fun start ->
    let rec step index lst =
      if index > start then
        lst
      else
        step (index + 1) (Cons (index, lst))

    step 0 EndOfList


// Implement the sum function that adds up all the ints in the list
// and returns the total. Use recursion.
let rec sum : MyList<int> -> int =
  fun ints ->
    match ints with
    | Cons (x, rest) -> x + sum rest
    | EndOfList      -> 0


// Make your `sum` tail recursive if it is not already.
// If it is already tail recursive, copy your solution here and edit
// your previous one to be _not_ tail recusive for comparison.
let sumTailRecursive : MyList<int> -> int =
  fun ints ->
    let rec step ints' accumulator =
      match ints' with
      | Cons (x, rest) -> step rest (x + accumulator)
      | EndOfList      -> accumulator

    step ints 0


// Implement a function that takes a list and returns it in reverse
// order. Make sure your implementation is tail recursive.
let reverseList : MyList<'a> -> MyList<'a> =
  fun lst ->
    let rec step lst' accumulator =
      match lst' with
      | Cons (x, rest) -> step rest (Cons (x, accumulator))
      | EndOfList      -> accumulator

    step lst EndOfList


// Implement the partition function using tail recursion. Partition uses a
// predicate function to split a list into two sublists. The order of the
// list elements should be retained.
// For example:
// let isOdd x = x % 2 = 1
// partition isOdd (Cons (3,Cons (4,Cons (5,EndOfList)))) = (Cons (3, Cons (5, EndOfList)), Cons (4, EndOfList))
// HINT: You will need to use your reverseList function
let partition : ('a -> bool) -> MyList<'a> -> MyList<'a> * MyList<'a> =
  fun predicate lst ->
    let rec step lst' first second =
      match lst' with
      | Cons (x, rest) ->
        if predicate x then
          step rest (Cons (x, first)) second
        else
          step rest first (Cons (x, second))
      | EndOfList ->
        (first, second)

    step (reverseList lst) EndOfList EndOfList


// You might have noticed a common way all these different tail-recursive functions work.
// They all walk the list recursively and accumulate some state they pass forward
// through the recursion. At the end of the list, they return that final state.
// We can extract this commonality; This general pattern is called a fold.
// Implement the fold function for MyList, tail recursively.
let fold : ('state -> 'a -> 'state) -> 'state -> MyList<'a> -> 'state =
  fun folder initialState lst ->
    let rec step lst' state =
      match lst' with
      | Cons (x, rest) -> step rest (folder state x)
      | EndOfList      -> state

    step lst initialState


// Reimplement sum using fold
let sumUsingFold : MyList<int> -> int =
  fun lst ->
    fold (+) 0 lst


// Reimplement reverseList using fold
let reverseListUsingFold : MyList<'a> -> MyList<'a> =
  fun lst ->
    fold (fun s x -> Cons (x, s)) EndOfList lst


// Reimplement partition using fold
let partitionUsingFold : ('a -> bool) -> MyList<'a> -> MyList<'a> * MyList<'a> =
  fun predicate lst ->
    let folder (first, second) x =
      if predicate x then
        ((Cons (x, first)), second)
      else
        (first, (Cons (x, second)))

    fold folder (EndOfList, EndOfList) (reverseList lst)


// A different version of partition that would indicate its behaviour better
// in the types and also be more generic would use a Choice type instead of
// a bool. Let's try a version that can partition three ways.
// Implement this function using a fold.
let betterPartition : ('a -> Choice<'b,'c,'d>) -> MyList<'a> -> MyList<'b> * MyList<'c> * MyList<'d> =
  fun makeChoice lst ->
    let folder (first, second, third) x =
      match makeChoice x with
      | Choice1Of3 b ->
        ((Cons (b, first)), second, third)
      | Choice2Of3 c ->
        (first, (Cons (c, second)), third)
      | Choice3Of3 d ->
        (first, second, (Cons (d, third)))

    fold folder (EndOfList, EndOfList, EndOfList) (reverseList lst)


// Folds are not limited to linked lists. You can fold over many different types.
// Write a fold for an array using tail recursion.
let foldArray : ('state -> 'a -> 'state) -> 'state -> 'a[] -> 'state =
  fun folder initialState arr ->
    let rec step index state =
      if index < arr.Length then
        step (index + 1) (folder state arr.[index])
      else
        state

    step 0 initialState


// Write an array to MyList function using your foldArray function. The
// order of items in the array must be preserved.
let arrayToMyList : 'a[] -> MyList<'a> =
  fun arr ->
    foldArray (fun s x -> Cons (x, s)) EndOfList arr |> reverseList


// This is a weird (but simple) one. Write a fold for the option type.
// This sort of fold is not used in practice very much, however it demonstrates
// that the concept of a fold generalises beyond just lists.
let foldOption : ('state -> 'a -> 'state) -> 'state -> 'a option -> 'state =
  fun folder initialState opt ->
    match opt with
    | Some x -> folder initialState x
    | None   -> initialState


// Implement the 'defaultValue' function using your foldOption function
// eg. defaultValue 12 None     = 12
//     defaultValue 12 (Some 1) = 1
let defaultValue : 'a -> 'a option -> 'a =
  fun def opt ->
    foldOption (fun _ x -> x) def opt
