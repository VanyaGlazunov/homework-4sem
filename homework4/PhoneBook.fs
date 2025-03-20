namespace homework4

open System.IO
open System.Text.Json

module PhoneBook = 
    let add book name phone =
        (name, phone) :: book
    let findByName book name = 
        List.filter (fun (x, y) -> x = name) book |> List.map snd
    let findByPhone book phone = 
        List.filter (fun (x, y) -> y = phone) book |> List.map fst
    let printAll (book: list<string * string>) = 
        book |> List.iter (fun (x, y) -> printfn "name: %s phone: %s" x y)
    let saveIn filePath book = 
        try
            let json = JsonSerializer.Serialize(book)
            File.WriteAllText(filePath, json)
            Ok ()
        with
        | ex -> Error ex.Message
    let loadFrom filePath = 
        try
            let json = File.ReadAllText(filePath)
            let book = JsonSerializer.Deserialize<list<string * string>>(json)
            Ok book
        with
        | ex -> Error ex.Message