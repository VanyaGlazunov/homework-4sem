namespace homework4

open System.IO
open System.Text.Json

module PhoneBook = 
    /// Adds person with specified name and phone number to the specified phonebook
    let add book name phone =
        (name, phone) :: book
    /// Finds every phone number of every person with specified name in the phonebook
    let findByName book name = 
        List.filter (fun (x, y) -> x = name) book |> List.map snd
    /// Finds every person that has specified phone number in the phonebook
    let findByPhone book phone = 
        List.filter (fun (x, y) -> y = phone) book |> List.map fst
    /// Prints every record in the phonebook
    let printAll (book: list<string * string>) = 
        book |> List.iter (fun (x, y) -> printfn "name: %s phone: %s" x y)
    /// Saves phonebook to the specified file
    let saveIn filePath book = 
        try
            let json = JsonSerializer.Serialize(book)
            File.WriteAllText(filePath, json)
            Ok ()
        with
        | ex -> Error ex.Message
    /// Reads phonebook from the specified file
    let loadFrom filePath = 
        try
            let json = File.ReadAllText(filePath)
            let book = JsonSerializer.Deserialize<list<string * string>>(json)
            Ok book
        with
        | ex -> Error ex.Message