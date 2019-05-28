open System

let notImplemented () = raise <| NotImplementedException ()

// Try to do each of these without using the compiler, then check your result
// after you've written it. This means don't use VSCode with Ionide either ;)


// Implement map for option
let mapOption : ('a -> 'b) -> 'a option -> 'b option =
  fun fn opt ->
    notImplemented ()


// Implement map for list
let mapList : ('a -> 'b) -> 'a list -> 'b list =
  fun fn lst ->
    notImplemented ()


// Implement map for Result
let mapResult : ('a -> 'b) -> Result<'a, 'c> -> Result<'b, 'c> =
  fun fn res ->
    notImplemented ()


// Implement map for Result's error
let mapResultError : ('a -> 'b) -> Result<'c, 'a> -> Result<'c, 'b> =
  fun fn res ->
    notImplemented ()


// Implement bimap for Result. You should be able to implement it using functions you've defined previously
let bimapResult : ('a -> 'b) -> ('c -> 'd) -> Result<'a, 'c> -> Result<'b, 'd> =
  fun first second res ->
    notImplemented ()


// Implement map for choice 3
let mapChoice3 : ('a -> 'b) -> Choice<'a, 'c, 'd> -> Choice<'b, 'c, 'd> =
  fun fn choice ->
    notImplemented ()


// Implement map for Function
type Function<'Input, 'Output> = Function of ('Input -> 'Output)

let mapFunction : ('a -> 'b) -> Function<'x, 'a> -> Function<'x, 'b> =
  fun fn f ->
    notImplemented ()


// Implement map for Async using computation expressions
let mapAsync : ('a -> 'b) -> 'a Async -> 'b Async =
  fun fn a -> async {
    return notImplemented ()
  }


// Without using the compiler, what is the type of somethingElse?
let stringLength : string -> int = String.length
let something = None
let somethingElse = mapOption stringLength something


// What is another way of writing the lists value using less maps? (Implement it in listsWithLessMaps)
let addOne a = a + 1
let toString x = x.ToString()
let lists =
  [ 1; 2; 3; 4 ]
  |> mapList addOne
  |> mapList toString

let listsWithLessMaps : string list =
  notImplemented ()


// Is it possible to implement mapper below such that the length of outputList changes?
let mapper x = notImplemented ()
let outputList input =
  mapList mapper input


// What would be the value of endingResult below. Don't run it and find out, do it in your head
let startingResult : Result<string, unit> = Ok "Channa"
let id : 'a -> 'a = fun x -> x
let endingResult = mapResult id startingResult


// Is this function Functor's map function? Explain why yes or no.
let mapOptionInts : (int -> int) -> int option -> int option =
  fun fn opt -> notImplemented () // You don't need to see the implementation


// Given the following types and functions, write an implementation for lengthOfContent
// You don't need to implement mapMyAsync and httpGet. The actual implementation
// of these (and the MyAsync<'a> type) is irrelevant for the purposes of this exercise.
type MyAsync<'a> = YouDontNeedToKnow
type HttpResult =
  { Verb : string
    Uri : Uri
    Headers : Map<string, string list>
    Content : string }

let mapMyAsync : ('a -> 'b) -> MyAsync<'a> -> MyAsync<'b> = fun fn x -> notImplemented ()
let httpGet : Uri -> MyAsync<HttpResult> = fun uri -> notImplemented ()
let stringLength' : string -> int = String.length

let lengthOfContent : Uri -> MyAsync<int> =
  fun uri ->
    notImplemented ()


// How could you refactor refactorMe to use maps? Implement your refactoring in
// refactoredRefactorMe. You don't need to implement readFile and writeFile.
let readFile : string -> Async<byte[]> = fun filename -> notImplemented ()
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
  notImplemented()
