open RandomAccessStack
open RealTimeQueue
open RDeque
open TreeDeque

let RAPrint () = 
    let rs = RandomAccessStack.empty;
    let rs = RandomAccessStack.push rs 1
    printfn "%A" rs

[<EntryPoint>]
let main argv =
    let test = 0
    printfn "%A" (test)
    
    0
