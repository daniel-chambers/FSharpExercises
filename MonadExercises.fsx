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


// TODO: Show that Validation is _not_ a monad
// TODO: List monad
// TODO: Async: computation expression desugaring
// TODO: Create AsyncResult monad
// TODO: Reader monad
