module RDeque
    open Stack
    
    type Transfer<'a> = 'a list * int * Stack<'a> * int

    (*

    step 1: reverse the big list onto auxilliary stack
    step 1: reverse the small list onto auxilliary stack
    step 2: reverse the auxiliarry B onto new stack

    RevB: current, Big, aux, count
    
    *)

    type DSide<'a> =
        | Normal  of Stack<'a> * int
        | RevB    of Transfer<'a> * Stack<'a> * 'a list * int
        | RevS1   of Transfer<'a> * Stack<'a> * 'a list
        | Copy    of Transfer<'a> * 'a list * 'a list * int
        | RevS2   of Transfer<'a> * 'a list * Stack<'a> * 'a list * int

    let private put i ((extra, added, old, remained): Transfer<'a>) = (i::extra, added + 1, old, remained)
    let private get c =
        match c with
        | ([], added, old, remained) -> (head old, ([], added, tail old, remained - 1))
        | (x::xs, added, old, remained) -> (x, (xs, added - 1, old, remained))

    let private top c = match get c with (element, _) -> element
    let private bot c = match get c with (_, current) -> current
    
    let private normalize c =
        match c with
        | Copy ((extra, added, _, rem), _, new', mov) when mov = rem ->
            Normal ((extra, new'), added + mov)
        | _ -> c
    
    let private tick state =
        match state with
        | Normal _ -> state
        | RevB (current, big, auxB, count) ->
            RevB (current, tail big, (head big)::auxB, count - 1)
        | RevS1 (_, small, _) when isEmpty small -> state
        | RevS1 (current, small, auxS) ->
            RevS1 (current, tail small, (head small)::auxS)
        | RevS2 (current, auxS, big, newS, count) when isEmpty big ->
            normalize (Copy (current, auxS, newS, count))
        //| RevS2 (current, auxS, big, newS, count) when not (isEmpty big) ->
        //    RevS2 (current, auxS, tail big, (head big)::newS, count + 1)
            //normalize (Copy (current, auxS, newS, count))
        | RevS2 (current, auxS, big, newS, count) ->
            RevS2 (current, auxS, tail big, (head big)::newS, count + 1)
        | Copy ((_, _, _, rem) as current, aux, new', moved) when moved < rem ->
            normalize (Copy (current, List.tail aux, (List.head aux)::new', moved + 1))
        | Copy _ as s -> normalize s
    // Copy current aux new moved
    let private ticks state =
        match state with
        |            (RevB (currentB, big,  auxB, 0), RevS1 (currentS, _,    auxS)) ->
          (normalize (Copy (currentB, auxB, [], 0)),  RevS2 (currentS, auxS, big, [], 0))

        | (RevS1 (currentS, _   , auxS),                  RevB (currentB, big, auxB, 0)) ->
          (RevS2 (currentS, auxS, big, [], 0), normalize (Copy (currentB, auxB, [],  0)))
        | (l, r) -> (tick l, tick r)

    let rec steps n pair =
        //printfn "[[%A]]" pair
        match n with
        | 0 -> pair
        | _ -> steps (n - 1) (ticks pair)
    
    type Side = RHS | LHS
    type Deque<'a> =
        | List of 'a list
        | Pair of DSide<'a> * DSide<'a>

    let empty = List []

    let isEmpty q = match q with List [] -> true | _ -> false

    let swap q = 
        match q with 
        | Pair (l, r) -> Pair (r, l) 
        | List l -> List (List.rev l)

    let private pop' s =
        match s with
        | Normal (a, b)           -> (head a, Normal  (tail a, b - 1))
        | RevB (a, b, c, d)     -> (top  a, RevB  (bot a, b, c, d))
        | RevS1 (a, b, c)       -> (top  a, RevS1 (bot a, b, c))
        | RevS2 (a, b, c, d, e) -> (top  a, RevS2 (bot a, b, c, d, e))
        | Copy (a, b, c, d)     -> (top  a, Copy  (bot a, b, c, d))

    let private concatPair (x, y) = List.concat [x; y]

    let private dec' (e, n, s, r) = (e, n, s, r - 1)

    let private dec s =
        match s with
        
        //| RevB (a, b, c, d)     -> RevB  (dec' a, b, c, d)
        | RevS1 (a, b, c)       -> RevS1 (dec' a, b, c)
        | RevS2 (a, b, c, d, e) -> RevS2 (dec' a, b, c, d, e)
        //| Copy (a, b, c, d)     -> Copy  (dec' a, b, c, d)
        | _ -> s

    // CHANGED TO (- 2) !!!
    let rec pop side (q: Deque<'a>): 'a * Deque<'a> =
        match (side, q) with
        | (_, List ([])) -> failwith "empty"
        //| (LHS, List (x::xs)) -> (x, List xs)
        | (LHS, List l) -> (List.head l, List (List.tail l))
        | (LHS, Pair (Normal (L, l), Normal (R, r)) ) ->
            let n, stack = Stack.pop L
            if (l - 1) * 3 >= r 
            then (n, Pair (Normal (stack, l - 1), Normal (R, r)))
            elif l >= 2 
            then (n, Pair (steps 6 (RevS1 (([], 0, stack, 2 * l - 1), stack, []), 
                                    RevB (([], 0, R, r - l), R, [], r - l))))
            else (n, List (List.rev (concatPair R)))
        | (LHS, Pair (L, R)) ->
            let e, L = pop' L
            (e, Pair (steps 4 (L, R)))
        | (RHS, d) ->
            let res, d = pop LHS (swap d)
            (res, swap d)
    let private push' (z: 'a) (d: DSide<'a>) =
        match d with
        | Normal (a, b) -> Normal (Stack.push a z, b + 1)
        | RevB (a, b, c, d) -> RevB (put z a, b, c, d)
        | RevS1 (a, b, c) -> RevS1 (put z a, b, c)
        | RevS2 (a, b, c, d, e) -> RevS2 (put z a, b, c, d, e)
        | Copy (a, b, c, d) -> Copy (put z a, b, c, d)

    let private lenght' (s: DSide<'a>) =
        match s with
        | Normal (_, l) -> l
        | RevB ((_, a, _, r), _, _, _) -> a + r
        | RevS1 ((_, a, _, r), _, _) -> a + r
        | RevS2 ((_, a, _, r), _, _, _, _) -> a + r
        | Copy ((_, a, _, r), _, _, _) -> a + r

    let lenght (q: Deque<'a>) =
        match q with
        | List l -> List.length l
        | Pair (l, r) -> (lenght' l) + (lenght' r)

    let rec push side (q: Deque<'a>) (z: 'a): Deque<'a> = 
        match (side, q) with
        | (LHS, List l) when lenght q <= 2 -> List (z::l)
        | (LHS, List l) -> 
            Pair (Normal (([z; List.head l], []), 2), 
                  Normal ( ([List.head (List.tail (List.tail l));  List.head (List.tail l)], []), 2))
        | (LHS, (Pair (Normal (L, l), Normal (R, r)))) ->
            let L = Stack.push L z
            if 3 * r >= l + 1 then Pair (Normal (L, l + 1), Normal (R, r))
            else Pair (steps 6 (RevB (([], 0, L, l - r), L, [], l - r),
                                RevS1 (([], 0, R, 2*r + 1), R, [])))
        | (LHS, Pair (L, R)) -> Pair (steps 4 (push' z L, R))
        | (RHS, d) -> swap (push LHS (swap d) z)

    let pushRight q i = push RHS q i
    let pushLeft  q i = push LHS q i
    let popLeft q : 'a * Deque<'a> =  pop LHS q
    let popRight q = pop RHS q
