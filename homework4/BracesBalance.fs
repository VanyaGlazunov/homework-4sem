namespace homework4

module BracesBalance = 
    let checkBalance input = 
        let mapBrace closing = 
            match closing with
            | ')' -> '('
            | '}' -> '{'
            | ']' -> '['
            | _ -> '_'
    
        let isOpeningBrace (brace: char) = 
            brace = '(' || brace = '{' || brace = '['

        let rec check (input: string) (stack: char list) ind =
            if ind = input.Length then
                stack.IsEmpty
            else
                match input[ind] with
                | c when c |> isOpeningBrace -> 
                    check input (c :: stack) (ind + 1)
                | c when c |> (mapBrace >> isOpeningBrace) ->
                    match stack with
                    | h :: t when h = mapBrace c -> 
                        check input t (ind + 1)
                    | _ -> false
                | _ -> check input stack (ind + 1)
        check input [] 0