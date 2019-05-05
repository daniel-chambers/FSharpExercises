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


// TODO: Show that Validation is _not_ a monad
// TODO: List monad
// TODO: Async: computation expression desugaring
// TODO: Create AsyncResult monad
// TODO: Reader monad
