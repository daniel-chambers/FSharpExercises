open System

let notImplemented () = raise <| NotImplementedException ()

// Try to do each of these without using the compiler, then check your result
// after you've written it. This means don't use VSCode with Ionide either ;)


// Implement pure for option
let pureOption : 'a -> 'a option =
  fun x ->
    notImplemented ()


// Implement apply for option
let applyOption : ('a -> 'b) option -> 'a option -> 'b option =
  fun fn x ->
    notImplemented ()


// Without using the compiler, what is the type of pureStringLength?
let stringLength : string -> int = String.length
let pureStringLength = pureOption stringLength


// Without using the compiler, what is the type of pureLabelNumber?
let labelNumber : string -> int -> string =
  fun label num -> sprintf "%s: %i" label num
let pureLabelNumber = pureOption labelNumber


// Without using the compiler, what is the type of oneApply?
let myLabel : string option = Some "The Meaning of Life, the Universe, and Everything"
let oneApply = applyOption pureLabelNumber myLabel


// Without using the compiler, what is the type of twoApplys? What is its value?
let myNum : int option = Some 42
let twoApplys = applyOption oneApply myNum


// Use pureOption and applyOption to use mkFullName with maybeFirstName
// and maybeSurname to get maybeFullName
let maybeFirstName : string option = Some "Jim"
let maybeSurname : string option = Some "Pelletier"
let mkFullName firstName surname = sprintf "%s %s" firstName surname
let maybeFullName : string option =
  notImplemented ()


// That was probably messy. If we implemented applyOption as an operator
// can you clean that up?
let maybeFullName' : string option =
  let inline (<*>) fn x = notImplemented ()
  notImplemented ()


// Copy in your implementation of mapOption from FunctorExercises
let mapOption : ('a -> 'b) -> 'a option -> 'b option =
  fun fn opt ->
    notImplemented ()


// Without using the compiler, what is the type of oneMap?
let labelNumber' : string -> int -> string =
  fun label num -> sprintf "%s: %i" label num
let myLabel' : string option = Some "The Meaning of Life, the Universe, and Everything"
let oneMap = mapOption labelNumber' myLabel'


// Without using the compiler, what is the type of oneMapOneApply? What is its value?
let myNum' : int option = Some 42
let oneMapOneApply = applyOption oneMap myNum'


// If we define mapOption as an operator (<!>), can you clean up maybeFullName' even further?
// Hint: you can implement this wholly in terms of map and apply
let maybeFullName'' : string option =
  let inline (<!>) fn x = notImplemented ()
  let inline (<*>) fn x = notImplemented ()
  notImplemented ()


// To prove every Applicative is a Functor, implement Functor's map for option
// (mapOptionViaApplicative) using only pure and apply
let mapOptionViaApplicative : ('a -> 'b) -> 'a option -> 'b option =
  fun fn opt ->
    notImplemented ()


// Implement the not implemented functions below then refactor mkAddress to use
// functor and applicative functions for option into refactoredMkAddress.
// Hint: You'll want to use the <!> and <*> operators to make it readable
type Address =
  { StreetNumber : int
    Street : string
    Suburb : string
    Postcode : string }

let stringToInt : string -> int option =
  fun str ->
    notImplemented ()
let emptyStringToOption : string -> string option =
  fun str ->
    notImplemented ()

let mkAddress : string -> string -> string -> string -> Address option =
  fun streetNo street suburb postcode ->
    let streetNoOpt = stringToInt streetNo
    let streetOpt = emptyStringToOption street
    let suburbOpt = emptyStringToOption suburb
    let postcodeOpt = emptyStringToOption postcode
    match (streetNoOpt, streetOpt, suburbOpt, postcodeOpt) with
    | (Some strNo, Some str, Some sub, Some pc) ->
      Some { StreetNumber = strNo; Street = str; Suburb = sub; Postcode = pc }
    | _ ->
      None

let refactoredMkAddress : string -> string -> string -> string -> Address option =
  fun streetNo street suburb postcode ->
    notImplemented ()


// Copy in your implementation of Functor map for Result from Functor Exercises
let mapResult : ('a -> 'b) -> Result<'a, 'c> -> Result<'b, 'c> =
  fun fn res ->
    notImplemented ()


// Implement pure for Result
let pureResult : 'a -> Result<'a, 'e> =
  fun x ->
    notImplemented ()


// Implement apply for Result
let applyResult : Result<('a -> 'b), 'e> -> Result<'a, 'e> -> Result<'b, 'e> =
  fun fn x ->
    notImplemented ()


// Implement calculateCommissionAmount, where commission for a broker is
// calculated as a percentage of the loan amount, with a minimum payable
// commission of $1000 regardless of loan amount
let calculateCommissionAmount : decimal -> decimal -> decimal =
  fun commissionPercentage loanAmount ->
    notImplemented ()


// Use the (fake) database query functions below to get the data you need
// to perform the above commission calculation and return the amount.
// Use the functor and applicative functions for Result to achieve this
type BrokerId = int
type LoanId = int
type SqlError =
  | QueryTimeout
  | OtherError of exn
let getCommissionPercentageForBrokerFromDb : BrokerId -> Result<decimal, SqlError> =
  fun id -> Ok 2.5m
let getLoanAmountFromDb : LoanId -> Result<decimal, SqlError> =
  fun id -> Ok 500000m

let getCommissionAmount : BrokerId -> LoanId -> Result<decimal, SqlError> =
  fun brokerId loanId ->
    notImplemented ()



type Validation<'a, 'e when 'e : comparison>  =
  | Success of 'a
  | Failure of 'e Set

// Implement Functor map for the Validation type above
let mapValidation : ('a -> 'b) -> Validation<'a, 'e> -> Validation<'b, 'e> =
  fun fn x ->
    notImplemented ()


// Implement pure for the Validation type above
let pureValidation : 'a -> Validation<'a, 'e> =
  fun x ->
    notImplemented ()


// Implement apply for the Validation type above. The difference between
// Validation and Result is that Validation should accumulate errors in
// the Failure Set, whereas Result simply uses the first error and discards
// the rest
let applyValidation : Validation<('a -> 'b), 'e> -> Validation<'a, 'e> -> Validation<'b, 'e> =
  fun fn x ->
    notImplemented ()


// Implement the following validation functions
type ValidationError =
  | Required of name : string
  | MustBeAnInteger of name : string
  | InvalidPostcode
  | MustBeABoolean of name : string

let validateInt : string -> string -> Validation<int, ValidationError> =
  fun name str ->
    notImplemented ()

let validateStringRequired : string -> string -> Validation<string, ValidationError> =
  fun name str ->
    notImplemented ()

// Hint: Australian postcodes must be four digits
let validatePostcode : string -> Validation<string, ValidationError> =
  fun str ->
    notImplemented ()


// Implement validateAddress using functor and applicative functions for Validation
let validateAddress : string -> string -> string -> string -> Validation<Address, ValidationError> =
  fun streetNo street suburb postcode ->
    notImplemented ()


// Implement validateBool, then implement validateResidence using functor and applicative functions
// Hint: you should be able to compose with your previous validateAddress function
type Residence =
  { Address : Address
    YearsOccupied : int
    IsPrimary : bool }

let validateBool : string -> string -> Validation<bool, ValidationError> =
  fun name str ->
    notImplemented ()

let validateResidence : string -> string -> string -> string -> string -> string -> Validation<Residence, ValidationError> =
  fun streetNo street suburb postcode yearsOccupied isPrimary ->
    notImplemented ()


// Which built-in function implements applicative's pure? Implement pureAsync by using it
let pureAsync : 'a -> Async<'a> =
  fun x -> notImplemented()


// Implement apply for Async (use a F# async computation expression)
let applyAsync : Async<'a -> 'b> -> Async<'a> -> Async<'b> =
  fun fn x ->
    notImplemented()


// Copy in your implementation of mapAsync from Functor Exercises
let mapAsync : ('a -> 'b) -> Async<'a> -> Async<'b> =
  fun fn x ->
    notImplemented()


// In Functor Exercises you refactored 'refactorMe' to use mapAsync
// Using your refactored solution from last time, can you continue refactoring it
// to use applicative functions as well as functor functions?
// Write your refactoring into refactoredRefactorMe
let readFile : string -> Async<byte[]> = fun file -> notImplemented ()
let writeFile : string -> string -> Async<unit> = fun filename contents -> notImplemented ()

let refactorMe = async {
  let! bytes = readFile @"C:\Temp\Nice file.txt"
  let decodedFile = System.Text.Encoding.UTF8.GetString bytes
  let wordsFromFile = decodedFile.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries)

  let! bytes = readFile @"C:\Temp\Another nice file.txt"
  let decodedFile2 = System.Text.Encoding.UTF8.GetString bytes
  let wordsFromFile2 = decodedFile2.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries)

  let uniqueWords =
    Seq.append wordsFromFile wordsFromFile2
    |> Set.ofSeq
  do!
    String.Join (Environment.NewLine, uniqueWords)
    |> writeFile (@"C:\Temp\All unique words.txt")

  return Set.count uniqueWords
}

let refactoredRefactorMe : Async<int> =
  notImplemented ()


// Implement getHardcodedUris using functor & applicative functions for MyAsync
// Note that the order of HttpResponses returned should match the order
// of the uris
// You don't need to implement map, pure and apply for MyAsync, nor care about
// its implementation
type MyAsync<'a> = YouDontNeedToKnow
type HttpResponse = Unimportant
let mapMyAsync : ('a -> 'b) -> MyAsync<'a> -> MyAsync<'b> = fun fn x -> notImplemented ()
let pureMyAsync : 'a -> MyAsync<'a> = fun x -> notImplemented ()
let applyMyAsync : MyAsync<'a -> 'b> -> MyAsync<'a> -> MyAsync<'b> = fun fn x -> notImplemented ()
let httpGet : Uri -> MyAsync<HttpResponse> = fun uri -> notImplemented ()

let getHardcodedUris : MyAsync<HttpResponse list> =
  let uri1 = Uri "https://www.google.com"
  let uri2 = Uri "https://fsharp.org"
  let uri3 = Uri "https://portal.azure.com"
  notImplemented()


// Can you generalise your solution to getHardcodedUris to an arbitrary list of Uris?
// Demonstrate this by implementing getAllUris. You should only require functor and applicative
// functions for async to do this
let getUris : Uri list -> MyAsync<HttpResponse list> =
  fun uris ->
    notImplemented ()


// Can you generalise your solution further, such that it doesn't care about Uris and HttpResponses?
// FYI: This particular function is called 'traverse'
let traverseMyAsync : ('a -> MyAsync<'b>) -> 'a list -> MyAsync<'b list> =
  fun fn xs ->
    notImplemented ()


// Implement traverse for Option
let traverseOption : ('a -> 'b option) -> 'a list -> 'b list option =
  fun fn xs ->
    notImplemented ()


// Describe the similarities between traverseMyAsync and traverseOption. What is
// different between both implementations and what is the same?


// Sequence is a variation upon traverse. Implement this for async by reusing your traverseMyAsync function
let sequenceMyAsync : MyAsync<'a> list -> MyAsync<'a list> =
  fun listOfAsyncs ->
    notImplemented ()


// Copy in your implementation of functor's map for list from Functor Exercises
let mapList : ('a -> 'b) -> 'a list -> 'b list =
  fun fn lst ->
    notImplemented ()


// Implement pure for list
let pureList : 'a -> 'a list =
  fun x -> notImplemented()


// Implement apply for list
// HINT: The usual implementation of applicative for list creates a cross-product of the
// applied lists. For example (and note the ordering out the output!):
// (fun a b -> (a, b)) <!> [1;2] <*> [3;4] = [(1,3);(1,4);(2,3);(2,4)]
let applyList : ('a -> 'b) list -> 'a list -> 'b list =
  fun fns xs ->
    notImplemented ()


// Using functor and applicative for list, generate a list of all possible loan
// interest rate dimensions (implement 'loanInterestRateDimensions')
// A loan interest rate defined for combinations of RiskGrade, Product and
// LvrRange (LVR stands for Loan-to-Value Ratio).
type RiskGrade = AAA | AA | A | BPlus | B | BMinus
type Product = Sharp | Star | Free
type LvrRange =
  { From : int
    To : int }
let validLvrRanges : LvrRange list =
  [ { From = 0; To = 60 }
    { From = 60; To = 80 }
    { From = 80; To = 90 }
    { From = 90; To = 95 } ]

let loanInterestRateDimensions : (Product * RiskGrade * LvrRange) list =
  notImplemented ()


// Because F# is a strict-evaluation language, lists cannot be infinite. However,
// .NET and therefore F# has the IEnumerable<T> (or 'a seq in F#) interface to
// represent lazy and potentially infinite sequences. Unfortunately, writing
// implementations of that interface manually is painful and mutable (especially lazy
// implementations). Luckily, F# has "Sequence Expressions" to help write these and
// get the compiler to generate the necessary lazy and mutable machinery for you.
// Please read the "Sequence Expressions" section of the F# documentation and come back:
// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/sequences#sequence-expressions


// Using what you've learned about sequence expressions (seq {} and yield),
// implement functor's map for seq
let mapSeq : ('a -> 'b) -> 'a seq -> 'b seq =
  fun fn xs ->
    notImplemented ()


// Implement pure for seq
let pureSeq : 'a -> 'a seq =
  fun x ->
    notImplemented ()


// Implement apply for sequence using the cross-product implementation of applicative
let applySeq : ('a -> 'b) seq -> 'a seq -> 'b seq =
  fun fns xs ->
    notImplemented ()


// Reimplement 'loanInterestRateDimensions' but this time as a sequence to make sure
// your implementation produces consistent results with your list applicative
// implementation from above
let loanInterestRateDimensionsAsSeq : (Product * RiskGrade * LvrRange) seq =
  notImplemented ()


// "Zipping" two lists together can be visualised by thinking of how a zipper
// zips the two sides of the zip together into one strip.
// Example: zipToTupleList [1;2;3] [4;5] = [(1,4);(2,5)]
// Implement zipToTupleList yourself (don't use the built-in List.zip function)
let zipToTupleList : 'a list -> 'b list -> ('a * 'b) list =
  fun xs ys ->
    notImplemented ()


// Can you generalise your zipToTupleList function such that it can produce
// any structure, not just tuples?
let zipLists : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list =
  fun fn xs ys ->
    notImplemented ()


// Re-implement zipToTupleList in terms of the generalised zipLists function
let zipToTupleList' : 'a list -> 'b list -> ('a * 'b) list =
  fun xs ys ->
    notImplemented ()


// One of the powers of sequences is that they can be infinite since they can
// be lazily generated. Implement zipSequences so that it works with infinite
// sequences. This means you can't cheat and convert the sequences to lists!
// HINT: Unfortunately due to the design of the IEnumerable<T> interface,
// you'll need to use some mutability to achieve this. However, the mutability
// is unavoidable and limited to this function, and the function will still be
// a pure function, so we'll let it pass this time! :)
let zipSequences : ('a -> 'b -> 'c) -> 'a seq -> 'b seq -> 'c seq =
  fun fn xs ys ->
    notImplemented ()


// What are the disadvantages of your zipSequences implementation versus your zipLists
// implementation?

// What are the advantages of your zipSequences implementation versus your zipLists
// implementation?


// There are multiple ways you could implement the applicative pattern for
// sequences. Previously you implemented applicative by creating the cross product
// of the applied sequences. Another way is 'zipping' the applied sequences together.
// For example:
// (fun a b -> (a, b)) <!> [1;2] <*> [3;4;5] = [(1,3);(2,4)]
type ZipList<'a> = ZipList of 'a seq


// Implement map for ZipList
let mapZipList : ('a -> 'b) -> ZipList<'a> -> ZipList<'b> =
  fun fn x ->
    notImplemented()


// Implement apply for ZipList
let applyZipList : ZipList<'a -> 'b> -> ZipList<'a> -> ZipList<'b> =
  fun fns xs ->
    notImplemented ()


// Implement pure for ZipList
let pureZipList : 'a -> ZipList<'a> =
  fun x -> notImplemented ()


// One of the laws of applicatives is that functor's map and applicative's pure and apply
// must work consistently. More specifically
// fn <!> lst
// MUST produce the same results as
// pure fn <*> lst
// Does this law hold true for your implementation of map, pure and apply?
// Implement the following poor man's unit test to find out.
// Note: Change `lst` to be an empty list, or a list of one thing, and see if your
// test still passes. No matter what `lst` is, the test must pass.
// If your ZipList implementation fails the test, go back and fix it.
let applicativeAndFunctorConsistencyLawTest : unit =
  let fn x = x + 2
  let lst = ZipList [1;2]
  let (ZipList mapResults) = notImplemented ()
  let (ZipList pureThenApplyResults) = notImplemented ()
  if Seq.toList mapResults <> Seq.toList pureThenApplyResults then
    failwithf "Oh no, your implementation is wrong! Map: %A PureThenApply: %A" mapResults pureThenApplyResults


// Why do you think we can't implement ZipList using the F# list type internally
// (ie. why are we using seq?)


// Implement the indexes function such that it returns an ascending sequence
// of integers starting from 0 up to but not including the value of the count arg
// Example: indexes 5 = [0;1;2;3;4]
let indexes : int -> int seq =
  fun count ->
    notImplemented ()


// Implement Seq.take yourself using your ZipList functor and applicative functions
// HINT: you will need to use the indexes function you just implemented
let takeSeq : int -> 'a seq -> 'a seq =
  fun count xs ->
    notImplemented ()


// Implement Seq.filter yourself using sequence expressions
let filterSeq : ('a -> bool) -> 'a seq -> 'a seq =
  fun predicate xs ->
    notImplemented ()


// Implement Seq.skip yourself using your ZipList functor and applicative functions
// HINT: use your indexes and filterSeq function
let skipSeq : int -> 'a seq -> 'a seq =
  fun count xs ->
    notImplemented ()
