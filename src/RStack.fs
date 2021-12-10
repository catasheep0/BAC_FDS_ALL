module rec RandomAccessStack

    type RandomAccessStack<'a> = 
        | Next of RandomAccessStack<'a> * RandomAccessStack<'a> * int * 'a
        | End

    let empty = End

    let length rs =
        match rs with
        | End -> 0
        | Next (_, _, l, _) -> l

    let push (s: RandomAccessStack<'a>) (i: 'a) =
        match s with
        | End -> Next (End, End, 0, i)
        | Next (n, j, l, _) -> Next(s, calc s, l + 1, i)

    let private jumplen (s: RandomAccessStack<'a>) =
        match s with
        | End -> 0
        | Next (_, j, _, _) -> length j

    let rec findn s index =
        match s with
        | End -> failwith "index out of bounds"
        | Next (_, _, n, i) when n = index -> i
        | Next (_, j, _, _) when (jumplen s) > index -> findn j index
        | Next (n, _, _, _) -> findn n index

    let private jump st =
        match st with
        | End -> End
        | Next (_, j, _, _) -> j
    
    let pop (Next (n, _, _, _)) = n
    let top (Next (_, _, _, i)) = i

    
    let private calc (Next (n, j, l, i) as st: RandomAccessStack<'a>) = if l - (length j) = (length j) - (jumplen j) then jump j else st
    
        
