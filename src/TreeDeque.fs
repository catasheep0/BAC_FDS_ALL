module rec TreeDeque

    type Color = GREEN = 0 | YELLOW = 1 | RED = 2
    type Side = LEFT | RIGHT
    type Tree<'a> =
        | Leaf of 'a
        | Pair of Tree<'a> * Tree<'a>

    type Buffer<'a> = int * Tree<'a> list
    
    (*            Color   Prefix       Suffix       Yellow      NonYellow *)
    type TreeDeque<'a> =
        | Just of Color * Buffer<'a> * Buffer<'a> * TreeDeque<'a> * TreeDeque<'a>
        | None
    

    let empty = Just (Color.RED, (0, []), (0, []), None, None)

    let private pushb (n, xs) x = (n + 1, x::xs)
    let private popb (n, x::xs) = (x, (n - 1, xs))
    let private injectb (n, xs) x = (n + 1, xs @ [x])
    
    let rec private append' ls i =
        match ls with
        | x::xs -> x::(append' xs i)
        | []    -> [i]
        
    

    let isEnd (lv: TreeDeque<'a>): bool =
        match lv with
        | Just (_, _, _, None, None) -> true
        | _                          -> false
    
    //let inject ((n, ls): Buffer<'a>) (i: Tree<'a>) = (n + 1, ls @ [i])
    let private ejectb ((n, ls): Buffer<'a>) = (List.last ls ,(n - 1, List.take (n - 1) ls))
    let private color (dq: TreeDeque<'a>) = 
        match dq with
        | Just (c, _, _, _, _) -> c
        | None -> Color.GREEN
    let private toColor n =
        match n with
        | 0 -> Color.RED
        | 1 -> Color.YELLOW
        | 2 -> Color.GREEN
        | 3 -> Color.GREEN
        | 4 -> Color.YELLOW
        | 5 -> Color.RED

    let isEmpty (l: TreeDeque<'a>) = 
        match l with
        | None | Just (_, (0, _), (0, _), _, _) -> true
        | _                                     -> false

    let calc (l: TreeDeque<'a>) =
        match l with
        | Just (_, (p, pl), (s, sl), y, ny) when (p = 0 || s = 0) && child l = None//|> dqSum = 0
            -> Just (min (toColor p) (toColor s), (p, pl), (s, sl), y, ny)
        | Just (_, (p, pl), (s, sl), y, ny) 
            -> Just (max (toColor p) (toColor s), (p, pl), (s, sl), y, ny)
    let private color_simple (Just (_, (p, pl), (s, sl), y, ny) : TreeDeque<'a>) =
        max (toColor p) (toColor s)
    
    let private getLeft (le: TreeDeque<'a>) =
        match le with
        | Just (_, l, _, _, _) -> l
        | None -> failwith "cannot get left side of an empty TreeDeque"

    let private child (l: TreeDeque<'a>) =
        match l with
        | Just (_, _, _, Just (a, b, c, d, e), _) -> Just (a, b, c, d, e)
        | Just (_, _, _, _, Just (a, b, c, d, e)) -> Just (a, b, c, d, e)
        | _ -> None
    let private childOrNew (l: TreeDeque<'a>) =
        match child l with
        | None -> empty
        | ch   -> ch

    let private putleft (lv: TreeDeque<'a>) (i: Buffer<'a>) =
        match lv with
        | Just (a, _, b, c, d) -> Just (a, i, b, c, d)
        | _ -> failwith ":("
    let private putright (lv: TreeDeque<'a>) (i: Buffer<'a>) =
        match lv with
        | Just (a, b, _, c, d) -> Just (a, b, i, c, d)
        | _ -> failwith ":("
    let private getprefix (Just (_, b, _, _, _): TreeDeque<'a>) = b
    let private getsuffix (Just (_, _, b, _, _): TreeDeque<'a>) = b

    let private eject2 (b: Buffer<'a>) =
        let (i0, b0) = ejectb b
        let (i1, b1) = ejectb b0
        (Pair (i1, i0), b1)
    let private pop2 ((n, a::b::xs): Buffer<'a>) =
        (Pair (a, b), (n - 2, xs))
    let private push2 ((n, l): Buffer<'a>) (Pair (a, b)) = (n + 2, a::b::l)
    

    let private dqSum (lv: TreeDeque<'a>) =
        match lv with
        | Just (color, (f, _), (s, _), ny, y) -> f + s
        | None -> 0


    let private balance (lv: TreeDeque<'a>) =
        match lv with
        | Just (clr, (_, []), (n, x::xs), ny, y) when n >= 1 -> 
            Just (clr, (1, [x]), (n - 1, xs), ny, y)
        | Just (clr, pr, (_, []), ny, y) when fst pr >= 1 ->
            let (i, np) = ejectb pr
            Just (clr, np, (1, [i]) , ny, y)
        | _ -> lv

    let private insertNonYellow (dq0: TreeDeque<'a>) (dq1: TreeDeque<'a>) =
        match dq0 with
        | Just (a, b, c, d, e) -> Just (a, b, c, d, dq1)
    let insertYellow (dq0: TreeDeque<'a>) (dq1: TreeDeque<'a>) =
        match dq0 with
        | Just (a, b, c, d, e) -> Just (a, b, c, dq1, e)
    let nonYellow dq =
        match dq with
        | Just (a, b, c, d, e) -> e
    let yellow dq =
        match dq with
        | Just (a, b, c, d, e) -> d
    let divide (lv: TreeDeque<'a>) yChild =
        
        match lv with 
        | Just (a, b, c, (Just (Color.YELLOW, d, e, f, g)), h) when (color g) <> Color.YELLOW && g <> None ->
            Just (a, b, c, (Just (Color.YELLOW, d, e, f, None)), insertNonYellow g h)
        | _ -> lv

    let private printtree (t: Tree<'a>) =
        match t with
        | Leaf f -> printf "%A " f
        | Pair (a, b) ->
            printtree a
            printtree b

    let private printYellow (dq: TreeDeque<'a>) =
        match dq with
        | Just (a, (_, b), (_,c), d, e) ->
            for t in b do
                printtree t
            printYellow d
        | None -> ()
    let private printYellowBackwards (dq: TreeDeque<'a>) =
        match dq with
        | Just (a, (_, b), (_,c), d, e) ->
            printYellowBackwards d
            for t in c do
                printtree t
        | None -> ()
    let printItems (dq: TreeDeque<'a>) =
        match dq with
        | Just (a, (_, b), (_,c), d, e) ->
            for t in b do
                printtree t
            printYellow d
            printItems e
            printYellowBackwards d
            for t in c do
                printtree t
        | None -> ()
    let private replace (pr: Buffer<'a>) (su: Buffer<'a>) (nw: TreeDeque<'a>) (old: TreeDeque<'a>): TreeDeque<'a> =
        match (old |> child |> color, color nw) with
        | _ when nw = None ->
            Just (Color.GREEN, pr, su, None, None)
        | (Color.YELLOW, c) when c <> Color.YELLOW ->
            Just (Color.GREEN, pr, su, None, insertNonYellow nw (nonYellow old))
        | (c, Color.YELLOW) when c <> Color.YELLOW ->
            Just (Color.GREEN, pr, su, insertNonYellow nw None, nonYellow nw)
        | (Color.YELLOW, Color.YELLOW) ->
            Just (Color.GREEN, pr, su, nw, nonYellow old)
        | (Color.GREEN, Color.GREEN) ->
            Just (Color.GREEN, pr, su, yellow old, nw)
        | (Color.GREEN, Color.RED) ->
            Just (Color.GREEN, pr, su, yellow old, nw)
        | (c0, c1) ->
            failwithf "Weird Colors %A %A" c0 c1

    //let ejectb ((n, b): Buffer<'a>): Tree<'a> * Buffer<'a> =
    //    (List.last b, (n - 1, List.take (List.length b - 1) b))
    let private setColor (Just (_, a, b, c, d): TreeDeque<'a>) (clr: Color) = Just (clr, a, b, c, d)
        
    let private regularp (pr: Buffer<'a>) (ch: TreeDeque<'a>) =
        if fst pr >= 4 then
            let (p, b0) = eject2 pr
            let chs = putleft ch (pushb (getLeft ch) p) |> calc
            (b0, chs)
        else if fst pr <= 1 then
            let (Pair (a, b), b0) = popb (getprefix ch)
            let ch = putleft ch b0
            ((fst pr + 2, List.concat [(snd pr); [a; b]]), ch)
        else 
            (pr, ch)

    let private regularph (pr: Buffer<'a>) (ch: TreeDeque<'a>) =
        if fst pr >= 4 then
            let (p, b0) = eject2 pr
            let chs = putleft ch (pushb (getLeft ch) p) |> calc
            (b0, chs)
        else
            (pr, ch)
    let private regularpl (pr: Buffer<'a>) (ch: TreeDeque<'a>) =
        if fst pr <= 1 then
            let (Pair (a, b), b0) = popb (getprefix ch)
            let ch = putleft ch b0
            ((fst pr + 2, List.concat [(snd pr); [a; b]]), ch)
        else
            (pr, ch)

    let private regulars (su: Buffer<'a>) (ch: TreeDeque<'a>) =
        if fst su <= 1 then
            let (p, b0) = ejectb (getsuffix ch)
            let chs = putright ch (b0) |> calc
            (push2 su p, chs)
        else if fst su >= 4 then
            let (p, b0) = pop2 su
            let (n, xs) = getsuffix ch
            let ch = putright (ch) (n + 1, xs @ [p])
            (b0, ch)
        else 
            (su, ch)
    
    let private regulars1l (su: Buffer<'a>) (ch: TreeDeque<'a>) =
        if fst su <= 1 then
            let (p, b0) = ejectb (getprefix ch)
            let chs = putleft ch (b0) |> calc
            (push2 su p, chs)    
        else
            (su, ch)

    let private regulars1h (su: Buffer<'a>) (ch: TreeDeque<'a>) =
        if fst su >= 4 then
            let (p, b0) = pop2 su
            let (n, xs) = getprefix ch
            let ch = putleft (ch) (n + 1, xs @ [p])
            (b0, ch)
        else
            (su, ch)

    let regulars1 (su: Buffer<'a>) (ch: TreeDeque<'a>) =
        if fst su <= 1 then
            let (p, b0) = ejectb (getprefix ch)
            let chs = putleft ch (b0) |> calc
            (push2 su p, chs)
        else if fst su >= 4 then
            let (p, b0) = pop2 su
            let (n, xs) = getsuffix ch
            let ch = putright (ch) (n + 1, xs @ [p])
            (b0, ch)
        else 
            (su, ch)
    let private popinject (dq: TreeDeque<'a>) =
        match dq with
        | Just (c, (_, []), (1, xs), a, b) -> Just (c, (1, xs), (0, []), a, b)
        | _ -> dq

    let private regular (lv: TreeDeque<'a>): TreeDeque<'a> =
        match lv with
        | _ when child lv |> dqSum >= 2 ->
            let balanced = childOrNew lv |> balance

            match lv with
            | Just (clr, f, s, y, ny) ->
                let (p, ch0) = regularp f balanced
                let (sf, ch) = regulars s ch0
                let ch_clr = if child y = None && ny <> None && y <> None then color_simple ch else color (ch |> calc)
                if isEmpty ch && child ch = None && ny = None then
                    Just (Color.GREEN, p, sf, None, None)
                else if isEmpty ch && child ch = None && child lv |> color <> Color.YELLOW then
                    Just (Color.GREEN, p, sf, None, None)
                else
                    replace p sf (setColor ch ch_clr) lv

            | _ -> failwith "no match"
        | _ when child lv |> dqSum <= 1 ->
            match lv with
            | Just (clr, f, s, ny, y) when fst f >= 2 || fst s >= 2 ->
                let ch0 = childOrNew lv |> popinject
                let (p0, ch1) = regularph f ch0
                let (s0, ch2) = regulars1h s ch1
                let (p, ch3) = regularpl p0 ch2
                let (su, ch) = regulars1l s0 ch3
                replace p su (if getprefix ch |> fst = 0 && getsuffix ch |> fst = 0 then None else ch |> calc) lv
            | Just (_, (n, a), b, _, _) ->
                let ch = child lv
                let (_, [Pair (t, u)]) = if fst (getprefix ch) > 0 then getprefix ch else getsuffix ch
                Just (Color.GREEN, (n + 2, a @ [t; u]), b, None, None)
        | _ -> failwith "no match"
    
    let private regtest (lv: TreeDeque<'a>) =
        match lv with
        | Just (Color.RED, _, _, _, _) ->
            regular lv
        | Just (Color.YELLOW, a, b, c, ny) when (color ny) = Color.RED ->
            Just (Color.YELLOW, a, b, c, regular ny)
        | Just (Color.YELLOW, b, c, Just (Color.YELLOW, f, g, h, i), d) when (color i) = Color.RED ->
            Just (Color.YELLOW, b, c, d, Just (Color.YELLOW, f, g, h, regular i))
        | _ -> lv

    let push lv i =
        let res = push' lv (Leaf i)
        res

    let pop lv =
        let (Leaf a, nw) = pop' lv
        (a, nw)

    let private pop' lv =
        match lv with
        | Just (clr, (c, x::xs), s, a, b) when isEnd lv ->
            (x, Just (clr, (c - 1, xs), s, a, b) |> calc)
        | Just (clr, s, (c, x::xs), a, b) when isEnd lv -> 
            (x, Just (clr, s, (c - 1, xs), a, b) |> calc)
        | Just (color, f, s, ny, y) ->
            let (i, f0) = popb f
            (i, putleft lv (f0) |> calc |> regtest)
    let private push' (lv: TreeDeque<'a>) (i: Tree<'a>) =
        match lv with
        | Just (_, (_, []), (n, xs), a, b) when isEnd lv && n < 3 ->
            Just (Color.RED, (0, []), pushb (n, xs) (i), a, b) |> calc
        | Just (_, (n, xs), x, a, b) when isEnd lv && n < 3 ->
            Just (Color.RED, pushb (n, xs) (i), x, a, b) |> calc
        | Just (color, f, s, ny, y) ->
            putleft lv (pushb f i) |> calc |> regtest
    
    let inject dq i = inject' dq (Leaf i)
    let private inject' (dq: TreeDeque<'a>) (i: Tree<'a>) =
        match dq with
        | Just (a, (n, xs), (_, []), d, e) when isEnd dq && n < 3 ->
            Just (a, injectb (n, xs) i, (0, []), d, e) |> calc
        | Just (a, pr, (n, xs), d, e) when isEnd dq && n < 3 ->
            Just (a, pr, injectb (n, xs) i, d, e) |> calc
        | Just (a, b, c, d, e) ->
            Just (a, b, injectb c i, d, e) |> calc |> regtest
    
    let eject dq = 
        let (Leaf a, dq0) = eject' dq
        (a, dq0)
    let private eject' (dq: TreeDeque<'a>) =
        match dq with
        | Just (a, b, (_, []), d, e) when isEnd dq ->
            let (i0, bf) = ejectb b
            (i0, Just (a, bf, (0, []), d, e))
        | Just (a, (_, []), c, d, e) when isEnd dq ->
            let (i0, bf) = ejectb c
            (i0, Just (a, (0, []), bf, d, e))
        | Just (a, b, c, d, e) ->
            let (i0, bf) = ejectb c
            (i0, Just (a, b, bf, d, e) |> calc |> regtest)