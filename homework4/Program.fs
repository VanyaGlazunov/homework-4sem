// namespace homework4
// For more information see https://aka.ms/fsharp-console-apps
module Program

open System
open homework4

let help = 
    printfn """This is interactive phone book program.
               Comands:
               help
               exit
               add [name] [phone]
               findByName [name]
               findByPhone [phone]
               list
               save [filepath]
               load [filepath]"""

let rec mainLoop book = 
    let input = Console.ReadLine().Split()
    match input[0] with
    | "help" -> help
    | "add" when input.Length = 3 ->
        mainLoop (PhoneBook.add book input[1] input[2])
    | "findByName" when input.Length = 2 ->
        let find = PhoneBook.findByName book input[1]
        match find.Length with
        | 0 ->
            printfn "Not found"
        | _ ->
            List.iter (printfn "%s") find
        mainLoop book
    | "findByPhone" when input.Length = 2 ->
        let find = PhoneBook.findByPhone book input[1]
        match find.Length with
        | 0 ->
            printfn "Not found"
        | _ ->
            List.iter (printfn "%s") find
        mainLoop book
    | "list" ->
        PhoneBook.printAll book
        mainLoop book
    | "save" when input.Length = 2 ->
        match PhoneBook.saveIn input[1] book with
        | Ok _ -> printfn "saved"
        | Error e -> printfn "Error while saving: %s" e
        mainLoop book
    | "load" when input.Length = 2 ->
        match PhoneBook.loadFrom input[1] with
        | Ok loaded -> printfn "saved"
        | Error e -> printfn "Error while loading: %s" e
        mainLoop book
    | "exit" -> ()
    | _ -> printfn "Invalid operation"

[<EntryPoint>]
let main _ = 
    mainLoop []
    0