open System

let notImplemented () = raise <| NotImplementedException ()

// Try to do each of these without using the compiler, then check your result
// after you've written it. This means don't use VSCode with Ionide either ;)


// Implement bind for option
let bindOption : ('a -> 'b option) -> 'a option -> 'b option =
  fun fn x ->
    notImplemented ()


// Without using the compiler, what is the type of unhappinessLevel? What is its value?
let endgameSpoilers : string option = None
let aPersonListening : string -> int option = fun spoilers -> Some spoilers.Length
let unhappinessLevel = endgameSpoilers |> bindOption aPersonListening


// Copy in your implementation of mapOption, pureOption and applyOption from ApplicativeExercises
let mapOption : ('a -> 'b) -> 'a option -> 'b option =
  fun fn opt ->
    notImplemented ()

let pureOption : 'a -> 'a option =
  fun x ->
    notImplemented ()

let applyOption : ('a -> 'b) option -> 'a option -> 'b option =
  fun fn x ->
    notImplemented ()


// To show that every monad is an applicative, implement Applicative's apply for
// option solely using Monad's bind
let applyOptionViaMonad : ('a -> 'b) option -> 'a option -> 'b option =
  fun fn x ->
    notImplemented ()


// The fact that you can implement apply using bind is a law that must be satisfied
// in order for your type to be a valid Monad.
// Run the below test to ensure that your applyOptionViaMonad produces the same results
// as applyOption does:
let applicativeRelationLawForOptionTest : unit =
  let convertToString : int -> string = Convert.ToString
  let intOption = Some 42
  let applyResult = applyOption (pureOption convertToString) intOption
  let applyViaMonadResult = applyOptionViaMonad (pureOption convertToString) intOption
  if applyResult <> applyViaMonadResult then
    failwithf "Uh oh, applyResult (%A) is not the same as applyViaMonadResult (%A)!" applyResult applyViaMonadResult


// Refactor mkFullName to use monad bind. Put your solution in mkFullNameWithBind
let mkFullName : string option -> string option -> string option =
  fun firstName surname ->
    match firstName with
    | Some f ->
      match surname with
      | Some s ->
        Some <| sprintf "%s %s" f s
      | None -> None
    | None -> None

let mkFullNameWithBind : string option -> string option -> string option =
  fun firstName surname ->
    notImplemented ()


// The bind function is sometimes expressed as the operator >>=
// Implement the >>= operator, then refactor mkFullName to use it.
let mkFullNameWithOperator : string option -> string option -> string option =
  fun firstName surname ->
    let inline (>>=) x fn = notImplemented ()
    notImplemented ()


// Refactor mkFullName to use applicative apply instead of monad bind.
let refactoredMkFullName : string option -> string option -> string option =
  fun firstName surname ->
    notImplemented ()


// Why might it be preferrable to use applicative apply rather than
// monad bind in the case of mkFullName?


// Refactor fancierMkFullName to use monad's bind. Put your solution in
// fancierMkFullNameWithBind
let fancierMkFullName : string option -> string option -> string option =
  fun firstName surname ->
    match firstName with
    | None ->
      None
    | Some f when String.IsNullOrWhiteSpace f ->
      None
    | Some f ->
      match surname with
      | None ->
        None
      | Some s when String.IsNullOrWhiteSpace s ->
        None
      | Some s ->
        Some <| sprintf "%s %s" f s

let fancierMkFullNameWithBind : string option -> string option -> string option =
  fun firstName surname ->
    notImplemented ()


// Can you refactor fancierMkFullName to use applicative's apply instead of
// monad's bind? If you can, do so. If you can't, explain why.


// Flattening a nested monad is fairly common, and this operation is known
// generically as 'join'.
// Implement a join function for the option type, using bindOption:
let joinOption : 'a option option -> 'a option =
  fun nestedOption ->
    notImplemented ()


// Implement bind for Result
let bindResult : ('a -> Result<'b, 'e>) -> Result<'a, 'e> -> Result<'b, 'e> =
  fun fn result ->
    notImplemented ()


// Copy in your implementations of map, pure and apply for Result from Applicative Exercises
let mapResult : ('a -> 'b) -> Result<'a, 'e> -> Result<'b, 'e> =
  fun fn res ->
    notImplemented ()

let pureResult : 'a -> Result<'a, 'e> =
  fun x ->
    notImplemented ()

let applyResult : Result<'a -> 'b, 'e> -> Result<'a, 'e> -> Result<'b, 'e> =
  fun fn x ->
    match (fn, x) with
    | Ok fn,   Ok x    -> Ok <| fn x
    | Ok _,    Error e -> Error e
    | Error e, _       -> Error e


// Due to bugs in legacy software, you've noticed that sometimes a broker's commission
// percentage gets corrupted into a negative or zero percentage in the database.
// Implement checkCommissionPercentageForCorruption to detect and fail when this happens
type BrokerId = int
type LoanId = int
type SqlError =
  | QueryTimeout
  | OtherError of exn
  | CorruptCommission
let getCommissionPercentageForBrokerFromDb : BrokerId -> Result<decimal, SqlError> =
  fun id -> Ok 2.5m
let getLoanAmountFromDb : LoanId -> Result<decimal, SqlError> =
  fun id -> Ok 500000m

let checkCommissionPercentageForCorruption : decimal -> Result<decimal, SqlError> =
  fun commissionPercentage ->
    notImplemented ()


// Copy your calculateCommissionAmount implementation from your Applicative Exercises
let calculateCommissionAmount : decimal -> decimal -> decimal =
  fun commissionPercentage loanAmount ->
    notImplemented ()


// Enhance your implementation of getCommissionAmount from your Applicative Exercises
// to use your new checkCommissionPercentageForCorruption function
// Hint: Monad bind can help you!
let getCommissionAmount : BrokerId -> LoanId -> Result<decimal, SqlError> =
  fun brokerId loanId ->
    notImplemented ()


// We implemented the Validation type in the Applicative Exercises. However, Validation
// cannot be a Monad. To demonstrate this, we will try to implement Monad bind for
// Validation and see if it satisfies the Monad laws.
// Implement brokenBindValidation below.
type Validation<'a, 'e when 'e : comparison>  =
  | Success of 'a
  | Failure of 'e Set


let brokenBindValidation : ('a -> Validation<'b, 'e>) -> Validation<'a, 'e> -> Validation<'b, 'e> =
  fun fn x ->
    notImplemented ()


// Copy in your implementations of mapValidation, pureValidation, applyValidation
// and validateStringRequired from Applicative Exercises
let mapValidation : ('a -> 'b) -> Validation<'a, 'e> -> Validation<'b, 'e> =
  fun fn x ->
    notImplemented ()

let pureValidation : 'a -> Validation<'a, 'e> =
  fun x ->
    notImplemented ()

let applyValidation : Validation<('a -> 'b), 'e> -> Validation<'a, 'e> -> Validation<'b, 'e> =
  fun fn x ->
    notImplemented ()

type ValidationError =
  | Required of name : string

let validateStringRequired : string -> string -> Validation<string, ValidationError> =
  fun name str ->
    notImplemented ()


// Implement applyValidation using your brokenBindValidation implementation as
// applyValidationViaBrokenBind below
let applyValidationViaBrokenBind : Validation<('a -> 'b), 'e> -> Validation<'a, 'e> -> Validation<'b, 'e> =
  fun fn x ->
    notImplemented ()


// Remember, one of the Monad laws is that if you implement Applicative's apply using bind
// it must produce the same results as the original apply. If you can't, then your type cannot
// be a Monad!
// Run the below test to ensure that your applyValidationViaBrokenBind produces the same results
// as applyValidation does.
type FullName =
  { FirstName : string
    Surname : string }

let applicativeRelationLawForValidationTest : string =
  let inline (<!>) fn x = mapValidation fn x

  let mkFullName f s = { FirstName = f; Surname = s }
  let firstName = ""
  let surname = ""

  let testApply apply =
    let inline (<*>) fn x = apply fn x
    mkFullName
    <!> validateStringRequired "First Name" firstName
    <*> validateStringRequired "Surname" surname

  let applyResult = testApply applyValidation
  let applyViaMonadResult = testApply applyValidationViaBrokenBind

  if applyResult <> applyViaMonadResult then
    sprintf "Uh oh, applyResult (%A) is not the same as applyViaMonadResult (%A)!" applyResult applyViaMonadResult
  else
    "Test passed?! 😯🤔"


// Can you explain why the test failed?


// Could you think of a way to fix applyValidationViaBrokenBind so that the test passes?
// If so, demonstrate how. If not, explain why not.


// Implement bind for list
let bindList : ('a -> 'b list) -> 'a list -> 'b list =
  fun fn xs ->
    notImplemented ()


// Copy in your implementations for mapList, pureList and applyList (the cross-product one)
// from Applicative Exercises:
let mapList : ('a -> 'b) -> 'a list -> 'b list =
  fun fn lst ->
    notImplemented ()

let pureList : 'a -> 'a list =
  fun x ->
    notImplemented ()

let applyList : ('a -> 'b) list -> 'a list -> 'b list =
  fun fns xs ->
    notImplemented ()


// Another Monad law that must be satified that is the "right identity" law
// which states that (where 'm' is, in this instance, some list):
// m >>= pure  =  m
// In plain English, if you use pure with bind, you should get the same
// list back that you started with.
// Check your bindList implementation against this law using the poor man's test
// below. If you fail, fix your bindList implementation.
let rightIdentityMonadLawListTest : unit =
  let myList = [1;2;3]
  let result = bindList pureList myList
  if result <> myList then
    failwithf "Your bindList implementation is not lawful (right identity)! result: %A. myList: %A" result myList


// The "left identity" law is another Monad law that must be satisfied. It states that:
// pure a >>= fn  =  fn a
// In plain English, if you wrap a value in a list with pure, then use bind with some function
// the result should be the same as just using the function on the original a value.
// Check your bindList implementation against this law using the poor man's test below.
// If you fail, fix your bindList implementation.
let leftIdentityMonadLawListTest : unit =
  let twoSuccessors x = [x + 1; x + 2]
  let expected = twoSuccessors 1
  let result = pureList 1 |> bindList twoSuccessors
  if result <> expected then
    failwithf "Your bindList implementation is not lawful (left identity)! result: %A. expected: %A" result expected


// Another Monad law is the "associativity law". It states that:
// m >>= (fun x -> k x >>= h)  =  (m >>= k) >>= h
// In plain English, it shouldn't matter what associativity order you perform your binds in;
// the result should always be the same.
// Check your bindList implementation against this law using the poor man's test below.
// If you fail, fix your bindList implementation.
let associativityLawListTest : unit =
  let double x = [x*2]
  let twoSuccessors x = [x + 1; x + 2]
  let myList = [1;2]
  let expected = myList |> bindList (fun x -> twoSuccessors x |> bindList double)
  let result = (myList |> bindList twoSuccessors) |> bindList double
  if result <> expected then
    failwithf "Your bindList implementation is not lawful (associativity)! result: %A. expected: %A" result expected


// Given a map (dictionary) of Decades to Animes made in that decade ('animes')
// implement the getAnimesForDecades function that takes a list of decades
// and returns a list of Animes made in those decades.
// HINT: Use your bindList function
type Decade =
  | Nineties
  | Noughties
  | TwentyTens
  | TwentyTwenties

type Anime = Anime of string

let animes : Map<Decade, Anime list> =
  [ Nineties,
    [ Anime "Neon Genesis Evangelion"
      Anime "Cowboy Bebop"
      Anime "Trigun" ]

    Noughties,
    [ Anime "Fullmetal Alchemist: Brotherhood"
      Anime "Death Note" ]

    TwentyTens,
    [ Anime "Steins;Gate"
      Anime "Attack on Titan"
      Anime "Katanagatari"
      Anime "Erased" ] ]
  |> Map.ofList

let getAnimesForDecades : Decade list -> Anime list =
  fun decades ->
    notImplemented ()


// As list is a Monad, you can implement the nested-monad flattening
// function 'join' for it. In the case of lists, joinList is a function
// that takes a list of lists and concatenates them into a single list
// (sometimes known as 'concat' or 'flatten'):
// joinList [[1;2;3];[4;5;6]] = [1;2;3;4;5;6]
// Implement joinList using bindList
let joinList : 'a list list -> 'a list =
  fun lists ->
    notImplemented ()


// The built-in Async type in F# is a monad! In fact, the async computation
// expression desugars to simple calls to Async bind and Async pure.
// For example:
//
// async {
//   let! contents = readFile "file.txt"
//   return String.length contents
// }
//
// desugars to something very similar to:
//
// readFile "file.txt"
// |> bindAsync (fun contents ->
//   pureAsync contents.Length
// )
//
// Before continuing copy in your mapAsync, pureAsync and applyAsync implementations
// from Applicative Exercises
let pureAsync : 'a -> Async<'a> =
  fun x ->
    notImplemented ()

let applyAsync : Async<'a -> 'b> -> Async<'a> -> Async<'b> =
  fun fn x ->
    notImplemented ()

let mapAsync : ('a -> 'b) -> Async<'a> -> Async<'b> =
  fun fn x ->
    notImplemented ()

let bindAsync : ('a -> Async<'b>) -> Async<'a> -> Async<'b> = fun fn x -> async.Bind (x, fn)
let readFile : string -> Async<byte[]> = fun file -> notImplemented ()
let writeFile : string -> byte[] -> Async<unit> = fun filename contents -> notImplemented ()
let writeStringFile : string -> string -> Async<unit> = fun filename contents -> notImplemented ()


// For the sake of understanding what's going on under the covers with
// the async computation expression, and to see how async is really just a monad,
// desugar the copyFile function below into calls to bind and pure.
// Write your version in desugaredCopyFile. Try to keep the same names for things!
let copyFile : string -> string -> Async<int> =
  fun source destination -> async {
    let! contents = readFile source
    do! writeFile destination contents
    return contents.Length
  }

let desugaredCopyFile : string -> string -> Async<int> =
  fun source destination ->
    notImplemented ()


// As you can probably see, computation expressions are a great way of using
// monadic binding in a way that makes it much more readable! To really ram
// home how useful they are, desugar the refactorMe async value below into
// derefactorMe to make it less readable ;) Try to keep the same names for things!
let refactorMe = async {
  let! bytes = readFile @"C:\Temp\Nice file.txt"
  let decodedFile = System.Text.Encoding.UTF8.GetString bytes
  let wordsFromFile = decodedFile.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries)

  let! bytes2 = readFile @"C:\Temp\Another nice file.txt"
  let decodedFile2 = System.Text.Encoding.UTF8.GetString bytes2
  let wordsFromFile2 = decodedFile2.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries)

  let uniqueWords =
    Seq.append wordsFromFile wordsFromFile2
    |> Set.ofSeq
  do!
    String.Join (Environment.NewLine, uniqueWords)
    |> writeStringFile (@"C:\Temp\All unique words.txt")

  return Set.count uniqueWords
}

let derefactorMe : unit -> Async<int> =
  fun () ->
    notImplemented ()


// A very useful monad to have in your toolbox in the modern area of 'async-everything'
// is the AsyncResult monad, which is a combination of the Async and Result type.
// This monad is able to perform actions asynchronously, but also has Result's behaviour
// of short circuiting execution when something fails and capturing that error in the
// type
type AsyncResult<'Result, 'Error> = AsyncResult of Async<Result<'Result, 'Error>>


// Since AsyncResult is a Monad, it is also a Functor!
// Implement Functor's map for AsyncResult.
// HINT: You can reuse mapAsync and mapResult to do this!
let mapAsyncResult : ('a -> 'b) -> AsyncResult<'a, 'e> -> AsyncResult<'b, 'e> =
  fun fn x ->
    notImplemented ()


// Copy in your mapResultError implementation from Functor Exercises
let mapResultError : ('a -> 'b) -> Result<'c, 'a> -> Result<'c, 'b> =
  fun fn res ->
    notImplemented ()


// Let's also implement map for the Error side of the Result.
// HINT: Your mapResultError from above will be useful!
let mapErrorAsyncResult : ('e1 -> 'e2) -> AsyncResult<'a, 'e1> -> AsyncResult<'a, 'e2> =
  fun fn x ->
    notImplemented ()


// Since AsyncResult is a Monad, it is also an Applicative!
// Implement Applicative's pure for AsyncResult
// HINT: You can reuse pureAsync and pureResult to do this
let pureAsyncResult : 'a -> AsyncResult<'a, 'e> =
  fun x ->
    notImplemented ()


// We need a way of constructing an error AsyncResult value.
// Implement errorAsyncResult below
let errorAsyncResult : 'e -> AsyncResult<'a, 'e> =
  fun error ->
    notImplemented ()


// Implement Applicative's apply for AsyncResult
// HINT: You can reuse mapAsync, applyAsync, and applyResult to do this
let applyAsyncResult : AsyncResult<('a -> 'b), 'e> -> AsyncResult<'a, 'e> -> AsyncResult<'b, 'e> =
  fun fn x ->
    notImplemented ()


// Implement Monad's bind for AsyncResult
// HINT: You can use async computation expressions to do this
let bindAsyncResult : ('a -> AsyncResult<'b, 'e>) -> AsyncResult<'a, 'e> -> AsyncResult<'b, 'e> =
  fun fn x ->
    notImplemented ()


// We need a way of converting an Async into an AsyncResult, preferrably
// one that captures any IO errors in the AsyncResult error type.
// Implement asyncResultFromAsync
let asyncResultFromAsync : Async<'a> -> AsyncResult<'a, Exception> =
  fun a ->
    notImplemented ()


// We will also want a way of getting an AsyncResult from a Result
// Implement asyncResultFromResult
let asyncResultFromResult : Result<'a, 'e> -> AsyncResult<'a, 'e> =
  fun result ->
    notImplemented ()


// We're going to build a function that:
// - Reads from UTF-8 text from a file and breaks it into lines
// - Validates that each line is not empty
// - Counts the number of words on each line
// - Writes these counts out to another file
// This function will handle errors explicitly in the types.
// Let's build up a library of functions that we will compose together
// to do this.

// Adapt the readFile and writeStringFile functions from above into
// AsyncResult versions that capture any IO exceptions into the
// FileIOError error defined above.
// Put your implementations into readFileAsyncResult and writeStringFileAsyncResult
type Error =
  | FileIOError of filename : string * error : Exception
  | EmptyLines of lineNumbers : int Set

let readFileAsyncResult : string -> AsyncResult<byte[], Error> =
  fun filename ->
    notImplemented ()

let writeStringFileAsyncResult : string -> string -> AsyncResult<unit, Error> =
  fun filename contents ->
    notImplemented ()


// Write a function that reads from a file and returns an array of the lines in that file
let readLinesFromFile : string -> AsyncResult<string[], Error> =
  fun filename ->
    notImplemented ()


// Write a function that validates a line to ensure it is not empty (or whitespace).
// The error should be the line number if it fails
let validateLineIsNonEmpty : int -> string -> Validation<string, int> =
  fun lineNumber line ->
    notImplemented ()


// Implement traverse for validation. (You're going to need it shortly!)
let traverseValidation : ('a -> Validation<'b, 'e>) -> 'a list -> Validation<'b list, 'e> =
  fun fn xs ->
    notImplemented ()


// Write a function that can create a Result from a Validation
let resultFromValidation : Validation<'a, 'e> -> Result<'a, 'e Set> =
  fun validation ->
    notImplemented ()


// Implement a function that can validate a sequence of lines to ensure they are all
// non-empty. If any are non-empty, we should return an Error EmptyLines containing
// all the empty line numbers.
// HINT: You'll want to use Seq.mapi, and the functions you just defined above
let validateAllLinesAreNonEmpty : string seq -> Result<string list, Error> =
  fun lines ->
    notImplemented ()


// Implement a function that counts the number of words in a line of text
let countWordsInLine : string -> int =
  fun line ->
    notImplemented ()


// Implement a function that takes a sequence of lines, counts the words in each one
// then writes out each count number onto a different line in a single big string.
// eg. wordCountsPerLine ["this is"; "getting more complex"; "but challenge is good"] = "2\r\n3\r\n4"
let wordCountsPerLine : string seq -> string =
  fun lines ->
    notImplemented ()


// Let's now implement our main function, composing all our functions together.
// Remember, we want a function that:
// - Reads from UTF-8 text from a file and breaks it into lines
// - Validates that each line is not empty
// - Counts the number of words on each line
// - Writes these counts out to another file
// HINT: You're going to need the functions you implemented before, plus
// your AsyncResult bind function to do this!
let readFileAndSaveLineWordCounts : string -> string -> AsyncResult<unit, Error> =
  fun inputFilename outputFilename ->
    notImplemented ()


// We can implement our own computation expression for AsyncResult to make using it
// less noisy.
// To read all about how computation expressions work, check out the documentation:
// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/computation-expressions
// One vary rarely does this one's-self (one usually gets things like AsyncResult from libraries!)
// but let's go ahead and implement a very simple computation expression for AsyncResult.
// You'll need to implement the notImplemented () methods on the AsyncResultBuilder class below.
type AsyncResultBuilder () =
  member _this.Bind (x : AsyncResult<'a, 'e>, fn : 'a -> AsyncResult<'b, 'e>) : AsyncResult<'b, 'e> =
    notImplemented ()

  member _this.Return (x : 'a) : AsyncResult<'a, 'e> =
    notImplemented ()

  member _this.ReturnFrom (x : AsyncResult<'a, 'e>) : AsyncResult<'a, 'e> =
    x

  member _this.Zero () : AsyncResult<unit, 'e> =
    notImplemented ()

let asyncResult = AsyncResultBuilder ()


// Now let's use our new computation expression to rewrite readFileAndSaveLineWordCounts
// using let bangs!
let readFileAndSaveLineWordCountsWithComputationExpression : string -> string -> AsyncResult<unit, Error> =
  fun inputFilename outputFilename -> asyncResult {
    notImplemented ()
  }


// TODO: Reader monad
