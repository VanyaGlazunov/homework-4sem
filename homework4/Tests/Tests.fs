module Tests

open NUnit.Framework
open FsUnit
open FsCheck
open homework4.PointFree
open System.IO
open homework4
open homework4.BracesBalance

let mutable tempFile = ""

[<SetUp>]
let Setup () =
    tempFile <- Path.GetTempFileName()

[<TearDown>]
let Teardown() =
    if File.Exists(tempFile) then File.Delete(tempFile)

[<Test>]
let ``All steps to achieve point-free should be equal`` () =
    let compareOriginAnd1 x ls = func x ls = func1 x ls
    let compare1and2 x ls = func1 x ls = func2 x ls
    let compare2and3 x ls = func2 x ls = func3 x ls
    Check.QuickThrowOnFailure compareOriginAnd1
    Check.QuickThrowOnFailure compare1and2
    Check.QuickThrowOnFailure compare2and3

[<Test>]
let ``Add should prepend new entry to book``() =
    let initialBook = []
    let newBook = PhoneBook.add initialBook "Alice" "123-4567"
    newBook |> should equal [("Alice", "123-4567")]

[<Test>]
let ``FindByName returns empty list for non-existent name``() =
    let book = [("Bob", "555-1234")]
    PhoneBook.findByName book "Alice" |> should be Empty

[<Test>]
let ``FindByName returns all phones for existing name``() =
    let book = [
        ("Alice", "111-1111")
        ("Bob", "222-2222")
        ("Alice", "333-3333")
    ]
    let result = PhoneBook.findByName book "Alice"
    result |> should equivalent ["111-1111"; "333-3333"]

[<Test>]
let ``FindByPhone returns empty list for non-existent phone``() =
    let book = [("Charlie", "444-4444")]
    PhoneBook.findByPhone book "555-5555" |> should be Empty

[<Test>]
let ``FindByPhone returns all names for existing phone``() =
    let book = [
        ("Dave", "555-5555")
        ("Eve", "555-5555")
        ("Frank", "666-6666")
    ]
    let result = PhoneBook.findByPhone book "555-5555"
    result |> should equivalent ["Dave"; "Eve"]

[<Test>]
let ``Save and load should preserve data integrity``() =
    let book = [
        ("Grace", "777-7777")
        ("Henry", "888-8888")
    ]
    PhoneBook.saveIn tempFile book |> should equal (Result<unit, string>.Ok ())
    match PhoneBook.loadFrom tempFile with
    | Ok loadedBook -> loadedBook |> should equal book
    | Error msg -> Assert.Fail($"Loading failed: {msg}")

[<Test>]
let ``Save should return error for invalid path``() =
    let invalidPath = "/invalid/path/book.json"
    let book = [("Invalid", "test")]
    let result = PhoneBook.saveIn invalidPath book
    match result with
    | Ok _ -> Assert.Fail("Expected error but got Ok")
    | Error msg -> Assert.Pass()

[<Test>]
let ``Load should return error for non-existent file``() =
    let nonExistentPath = "nonexistent.json"
    let result = PhoneBook.loadFrom nonExistentPath
    match result with
    | Ok _ -> Assert.Fail("Expected error but got Ok")
    | Error msg -> Assert.Pass()

[<Test>]
let ``Load should return error for invalid JSON``() =
    File.WriteAllText(tempFile, "invalid json")
    let result = PhoneBook.loadFrom tempFile
    match result with
    | Ok _ -> Assert.Fail("Expected error but got Ok")
    | Error msg -> Assert.Pass()


[<Test>]
let ``Simple matching pairs are balanced``() =
    checkBalance "()" |> should be True
    checkBalance "{}" |> should be True
    checkBalance "[]" |> should be True

[<Test>]
let ``Nested braces are balanced``() =
    checkBalance "([{}])" |> should be True
    checkBalance "{()[]}" |> should be True
    checkBalance "[({})]" |> should be True

[<Test>]
let ``Mismatched pairs are unbalanced``() =
    checkBalance "(]" |> should be False
    checkBalance "{)" |> should be False
    checkBalance "[)" |> should be False

[<Test>]
let ``Extra closing brace makes unbalanced``() =
    checkBalance "())" |> should be False
    checkBalance "()]" |> should be False
    checkBalance "())}" |> should be False

[<Test>]
let ``Valid with non-brace characters is balanced``() =
    checkBalance "a(b)c" |> should be True
    checkBalance "1[2{3}4]5" |> should be True
    checkBalance "test123" |> should be True

[<Test>]
let ``Complex balanced nesting``() =
    checkBalance "([{()}][])" |> should be True
    checkBalance "([]([]{()}))" |> should be True

[<Test>]
let ``Unbalanced with valid parts``() =
    checkBalance "()[{]" |> should be False
    checkBalance "({)}" |> should be False
    checkBalance "[(])" |> should be False

[<Test>]
let ``Unclosed opening braces are unbalanced``() =
    checkBalance "(()" |> should be False
    checkBalance "{[}" |> should be False
    checkBalance "[[[" |> should be False

[<Test>]
let ``Properly ordered but interleaved braces are unbalanced``() =
    checkBalance "([)]" |> should be False
    checkBalance "{[}]" |> should be False

[<Test>]
let ``Long valid sequence remains balanced``() =
    let longValid = String.replicate 1000 "()"
    checkBalance longValid |> should be True

[<Test>]
let ``Long invalid sequence remains unbalanced``() =
    let longInvalid = "(" + String.replicate 1000 ")"
    checkBalance longInvalid |> should be False
