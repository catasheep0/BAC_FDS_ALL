module RealTimeQueue
    type NormalQueue<'a> =
        'a list * 'a list * int

    type RecopyQueue<'a> = { copied: NormalQueue<'a>; copy: int;  newFront: 'a list; reversed: 'a list; added: 'a list; toRemove: 'a list;}

    type RealTimeQueue<'a> = 
        | Normal of NormalQueue<'a>
        | Recopy of RecopyQueue<'a>

    let empty = Normal ([], [], 0)

    let private RecopyEmpty : RecopyQueue<'a> = {copied = ([], [], 0); copy = 0; newFront = []; reversed = []; toRemove = []; added = []; }
    
    let recopy (Recopy x) = x

    let isRecopy (q: RealTimeQueue<'a>) =
        match q with
        | Normal _ -> false
        | Recopy _ -> true

    let copy (q: RealTimeQueue<'a>) = 
        match q with
        | Normal x -> Normal x
        | Recopy { copied = (x::xs, y::ys, d); reversed = r; newFront = n; copy = c } ->
            Recopy { recopy q with newFront = y::n; reversed = x::r; copy = c + 1; copied = (xs, ys, d + 1) }
        | Recopy { copied = ([], [], d); copy = c; newFront = f; added = a} when c = 0 ->
            Normal (f, a, d )
        | Recopy { copied = ([], x::xs, d); newFront = f; added = a; copy = c} ->
            Recopy { recopy q with copied = ([], xs, d + 1); newFront = x::f; copy = c + 1 }
        | Recopy { copied = ([], [], d); copy = c; reversed = x::xs; newFront = n; added = a } when c = 1 ->
            Normal (x::n, a, d + 1)
            //Recopy { recopy q with newFront = x::n; copy = c - 1; copied = ([], [], d + 1); reversed = xs }
        | Recopy { copied = ([], [], d); copy = c; reversed = x::xs; newFront = n } when c > 1 ->
            Recopy { recopy q with newFront = x::n; copy = c - 1; copied = ([], [], d + 1); reversed = xs }


    let enqueue (i: 'a) (q: RealTimeQueue<'a>) : RealTimeQueue<'a> =
        match q with
        | Normal (f, b, n) when n > 0 ->
            Normal (f, i::b, n - 1)
        | Normal (f, b, n) ->
            Recopy { RecopyEmpty with copied = (f, b, n + 1);  toRemove = f; newFront = [i]} |> copy |> copy
        | Recopy { copied = (f, b, n); added = a} ->
            Recopy { recopy q with copied = (f, b, n - 1); added = i::a} |> copy |> copy

    let dequeue (q: RealTimeQueue<'a>) : 'a * RealTimeQueue<'a> =
        match q with
        | Normal (x::xs, back, n) when n > 1 ->
            (x, Normal (xs, back, n - 1))
        | Normal (x::xs, back, n) when n = 1 ->
            (x, Recopy { RecopyEmpty with copied = (xs, back, n); toRemove = xs} |> copy |> copy)
        | Recopy { toRemove = x::xs; copy = c } ->
            (x, Recopy { recopy q with  toRemove = xs; copy = c - 1 } |> copy |> copy)
        | _ -> failwith ":("
