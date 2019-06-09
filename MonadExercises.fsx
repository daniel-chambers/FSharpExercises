open System

let notImplemented () = raise <| NotImplementedException ()

// Try to do each of these without using the compiler, then check your result
// after you've written it. This means don't use VSCode with Ionide either ;)


// Implement bind for option
let bindOption : ('a -> 'b option) -> 'a option -> 'b option =
  fun fn x ->
    match x with
    | Some x' -> fn x'
    | None    -> None


// Without using the compiler, what is the type of unhappinessLevel? What is its value?
// Answer: None : int option
let endgameSpoilers : string option = None
let aPersonListening : string -> int option = fun spoilers -> Some spoilers.Length
let unhappinessLevel = endgameSpoilers |> bindOption aPersonListening


// Copy in your implementation of mapOption, pureOption and applyOption from ApplicativeExercises
let mapOption : ('a -> 'b) -> 'a option -> 'b option =
  fun fn opt ->
    match opt with
    | Some x -> Some <| fn x
    | None   -> None

let pureOption : 'a -> 'a option =
  fun x ->
    Some x

let applyOption : ('a -> 'b) option -> 'a option -> 'b option =
  fun fn x ->
    match (fn, x) with
    | Some fn, Some x -> Some <| fn x
    | Some _,  None   -> None
    | None,    _      -> None


// To show that every monad is an applicative, implement Applicative's apply for
// option solely using Monad's bind
let applyOptionViaMonad : ('a -> 'b) option -> 'a option -> 'b option =
  fun fn x ->
    fn |> bindOption (fun fn' ->
      x |> bindOption (Some << fn')
    )


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
    firstName |> bindOption (fun f ->
      surname |> bindOption (fun s ->
        Some <| sprintf "%s %s" f s
      )
    )


// The bind function is sometimes expressed as the operator >>=
// Implement the >>= operator, then refactor mkFullName to use it.
let mkFullNameWithOperator : string option -> string option -> string option =
  fun firstName surname ->
    let inline (>>=) x fn = bindOption fn x
    firstName >>= (fun f ->
      surname >>= (fun s ->
        Some <| sprintf "%s %s" f s
      )
    )


// Refactor mkFullName to use applicative apply instead of monad bind.
let refactoredMkFullName : string option -> string option -> string option =
  fun firstName surname ->
    let inline (<!>) fn x = mapOption fn x
    let inline (<*>) fn x = applyOption fn x
    sprintf "%s %s" <!> firstName <*> surname


// Why might it be preferrable to use applicative apply rather than
// monad bind in the case of mkFullName?
// Answer: Other than the code being more concise, it also clearly communicates
// that the resultant option being Some or None entirely depends upon
// firstName and surname, not upon what's being done with their values.
// Another way of saying that is that `sprintf "%s %s"` does not care about
// the option structure at all, only the values within, and the use of
// applicative communicates this.
// Bind implies that the action being performed may decide to change
// the option structure for whatever reason, which is not the case here.


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
    firstName |> bindOption (fun f ->
      surname |> bindOption (fun s ->
        if String.IsNullOrWhiteSpace f || String.IsNullOrWhiteSpace s then
          None
        else
          Some <| sprintf "%s %s" f s
      )
    )


// Can you refactor fancierMkFullName to use applicative's apply instead of
// monad's bind? If you can, do so. If you can't, explain why.
// Answer: You can't because the logic that uses the first name and surname
// inside the options changes the option structure based on some logic.
// Functions 'lifted' by applicative cannot change the applicative's structure.
// Only a monad can do this, hence the use of bind.


// Flattening a nested monad is fairly common, and this operation is known
// generically as 'join'.
// Implement a join function for the option type, using bindOption:
let joinOption : 'a option option -> 'a option =
  fun nestedOption ->
    bindOption id nestedOption


// Implement bind for Result
let bindResult : ('a -> Result<'b, 'e>) -> Result<'a, 'e> -> Result<'b, 'e> =
  fun fn result ->
    match result with
    | Ok x    -> fn x
    | Error e -> Error e


// Copy in your implementations of map, pure and apply for Result from Applicative Exercises
let mapResult : ('a -> 'b) -> Result<'a, 'e> -> Result<'b, 'e> =
  fun fn res ->
    match res with
    | Ok x    -> Ok <| fn x
    | Error e -> Error e

let pureResult : 'a -> Result<'a, 'e> =
  fun x ->
    Ok x

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
    if commissionPercentage <= 0m then
      Error CorruptCommission
    else
      Ok commissionPercentage


// Copy your calculateCommissionAmount implementation from your Applicative Exercises
let calculateCommissionAmount : decimal -> decimal -> decimal =
  fun commissionPercentage loanAmount ->
    max (commissionPercentage * loanAmount) 1000m


// Enhance your implementation of getCommissionAmount from your Applicative Exercises
// to use your new checkCommissionPercentageForCorruption function
// Hint: Monad bind can help you!
let getCommissionAmount : BrokerId -> LoanId -> Result<decimal, SqlError> =
  fun brokerId loanId ->
    let inline (<!>) fn x = mapResult fn x
    let inline (<*>) fn x = applyResult fn x
    let inline (>>=) x fn = bindResult fn x
    calculateCommissionAmount
    <!> (getCommissionPercentageForBrokerFromDb brokerId >>= checkCommissionPercentageForCorruption)
    <*> getLoanAmountFromDb loanId


// We implemented the Validation type in the Applicative Exercises. However, Validation
// cannot be a Monad. To demonstrate this, we will try to implement Monad bind for
// Validation and see if it satisfies the Monad laws.
// Implement brokenBindValidation below.
type Validation<'a, 'e when 'e : comparison>  =
  | Success of 'a
  | Failure of 'e Set


let brokenBindValidation : ('a -> Validation<'b, 'e>) -> Validation<'a, 'e> -> Validation<'b, 'e> =
  fun fn x ->
    match x with
    | Success a -> fn a
    | Failure e -> Failure e


// Copy in your implementations of mapValidation, pureValidation, applyValidation
// and validateStringRequired from Applicative Exercises
let mapValidation : ('a -> 'b) -> Validation<'a, 'e> -> Validation<'b, 'e> =
  fun fn x ->
    match x with
    | Success x -> Success <| fn x
    | Failure e -> Failure e

let pureValidation : 'a -> Validation<'a, 'e> =
  fun x ->
    Success x

let applyValidation : Validation<('a -> 'b), 'e> -> Validation<'a, 'e> -> Validation<'b, 'e> =
  fun fn x ->
    match (fn, x) with
    | Success fn, Success x  -> Success <| fn x
    | Success _,  Failure e  -> Failure e
    | Failure e,  Success _  -> Failure e
    | Failure e1, Failure e2 -> Failure (Set.union e1 e2)

type ValidationError =
  | Required of name : string

let validateStringRequired : string -> string -> Validation<string, ValidationError> =
  fun name str ->
    if String.IsNullOrWhiteSpace str then
      Failure << Set.singleton <| Required name
    else
      Success str


// Implement applyValidation using your brokenBindValidation implementation as
// applyValidationViaBrokenBind below
let applyValidationViaBrokenBind : Validation<('a -> 'b), 'e> -> Validation<'a, 'e> -> Validation<'b, 'e> =
  fun fn x ->
    fn |> brokenBindValidation (fun fn' ->
      x |> brokenBindValidation (fun x' ->
        Success <| fn' x'
      )
    )


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
// Answer: It fails because using bind to implement apply ends up throwing away
// the errors in the Failure case from either the fn or the argument in apply
// instead of accumulating the errors.


// Could you think of a way to fix applyValidationViaBrokenBind so that the test passes?
// If so, demonstrate how. If not, explain why not.
// Answer: No, you cannot. Using bind means that you cannot access the error information
// in the Failure case in order to combine it into a new Failure case, which makes it
// impossible to accumulate errors.


// Implement bind for list
let bindList : ('a -> 'b list) -> 'a list -> 'b list =
  fun fn xs ->
    let rec step lst acc =
      match lst with
      | h :: t -> step t <| fn h :: acc
      | []     -> acc

    step xs [] |> List.rev |> List.concat


// Copy in your implementations for mapList, pureList and applyList (the cross-product one)
// from Applicative Exercises:
let mapList : ('a -> 'b) -> 'a list -> 'b list =
  fun fn lst ->
    let rec step lst' result =
      match lst' with
      | x :: xs -> step xs (fn x :: result)
      | []      -> result
    step lst [] |> List.rev

let pureList : 'a -> 'a list =
  fun x -> [x]

let applyList : ('a -> 'b) list -> 'a list -> 'b list =
  fun fns xs ->
    let rec step f ys result =
      match ys with
      | y :: rest -> step f rest (f y result)
      | []        -> result

    let applyFns fn fnResults =
      step (fun x xResults -> fn x :: xResults) xs [] @ fnResults

    step applyFns fns []
    |> List.rev


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
    decades
    |> bindList (fun decade ->
      animes
      |> Map.tryFind decade
      |> Option.defaultValue []
    )


// As list is a Monad, you can implement the nested-monad flattening
// function 'join' for it. In the case of lists, joinList is a function
// that takes a list of lists and concatenates them into a single list
// (sometimes known as 'concat' or 'flatten'):
// joinList [[1;2;3];[4;5;6]] = [1;2;3;4;5;6]
// Implement joinList using bindList
let joinList : 'a list list -> 'a list =
  fun lists ->
    bindList id lists


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
let bindAsync : ('a -> Async<'b>) -> Async<'a> -> Async<'b> = fun fn x -> async.Bind (x, fn)
let pureAsync : 'a -> Async<'a> = async.Return
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
    readFile source
    |> bindAsync (fun contents ->
      writeFile destination contents
      |> bindAsync (fun _ ->
        pureAsync contents.Length
      )
    )


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
    readFile @"C:\Temp\Nice file.txt"
    |> bindAsync (fun bytes ->
      let decodedFile = System.Text.Encoding.UTF8.GetString bytes
      let wordsFromFile = decodedFile.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries)

      readFile @"C:\Temp\Another nice file.txt"
      |> bindAsync (fun bytes2 ->
        let decodedFile2 = System.Text.Encoding.UTF8.GetString bytes2
        let wordsFromFile2 = decodedFile2.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries)

        let uniqueWords =
          Seq.append wordsFromFile wordsFromFile2
          |> Set.ofSeq

        String.Join (Environment.NewLine, uniqueWords)
        |> writeStringFile (@"C:\Temp\All unique words.txt")
        |> bindAsync (fun _ ->
          pureAsync <| Set.count uniqueWords
        )
      )
    )


// TODO: Create AsyncResult monad
// TODO: Reader monad
