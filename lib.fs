
module Lib 

open System.Collections.Generic
open Android.App

type Mailbox<'a> = MailboxProcessor<'a>

type Dictionary<'a,'b> with 
    member this.TryFind n =
        let h, v = this.TryGetValue(n)
        if h then Some v else None

type ModelAndUpdater<'state, 'msg, 'event when 'event : equality and 'state: equality>
        (update, msgToName : 'msg -> 'event, state : 'state, nullevent : 'event, activity : Activity) =
    let tree = Dictionary<'event, HashSet<'event * ('state -> unit) * ('state -> bool)>>()
    
    let rec treewalk n state =
        if n = nullevent then ()
        else
            match tree.TryFind n with
            | None -> ()
            | Some os ->
                for (o, f, proj) in os do
                    if proj state then 
                        activity.RunOnUiThread(fun () -> f state)
                        treewalk o state

    let stateProc (inbox : Mailbox<'msg>) =
        let rec stateloop state =
            async {
                let! msg = inbox.Receive()
                let state' = update state msg
                if state <> state' then  
                    treewalk (msgToName msg) state' 
                return! stateloop state'
            }
        stateloop state

    let stateFSM = Mailbox.Start(stateProc)
    member __.Dispatch m = stateFSM.Post(m)
    member __.Connect(e1, e2, f, condition) =
        match tree.TryFind e1 with 
        | None -> tree.Add(e1, (HashSet [ e2, f, condition ]))
        | Some branches -> branches.Add(e2,f,condition) |> ignore

    member m.Connect(e1,f, condition) = m.Connect(e1,nullevent, f, condition)

    member m.Connect(e1,f) = m.Connect(e1,nullevent, f, fun _ -> true)

    member m.Connect(e1,e2, f) = m.Connect(e1,e2, f, fun _ -> true)
     
type MaybeBuilder() =
    member __.Bind(x, f) =
        match x with
        | Some(x) -> f(x)
        | _ -> None
    member __.Delay(f) = f()
    member __.Return(x) = Some x
    member __.ReturnFrom(x) = x

let maybe = MaybeBuilder()