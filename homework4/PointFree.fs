namespace homework4

module PointFree =
    let func x l = List.map (fun y -> y * x) l
    let func1 x = List.map (fun y -> (*) x y)
    let func2 x = List.map ((*) x)
    let func3: int -> list<int> -> list<int> = (*) >> List.map