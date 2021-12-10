module Stack
    type Stack<'a> = 'a list * 'a list

    let empty: Stack<'a> = ([], [])
    let push ((f, s): Stack<'a>) (i: 'a) = (i::f, s)
    let pop (s: Stack<'a>) = 
        match s with
        | (x::xs, y)  -> (x, (xs, y))
        | ([], x::xs) -> (x, (xs, []))
        | ([], [])    -> failwith "stack empty"
    
    let isEmpty s =
        match s with
        | ([], []) -> true
        | _        -> false

    let head stack = match pop stack with (head, _) -> head
    let tail stack = match pop stack with (_, tail) -> tail
    