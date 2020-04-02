module Model
 
open System 

type Msg = 
    | SolarTextChanged of int * string  
    | BatteryTextChanged of int * string  
    | CheckChanged of bool
    | TabChanged of int
    | GridChanged of int * string
    | GridSave
     
type State = {
    SolarVolts : float option
    SolarAmps : float option
    BatteryVolts : float option
    BatteryWatts : float option  
    BatteryFull : bool
    CurrentTab : int  
    ElectricitykWhLeft : float option
    ElectricityWatts : float option
}  

[<Literal>] 
let SOLARVOLTS = 0

[<Literal>] 
let SOLARAMPS = 1

[<Literal>] 
let BATTERYVOLTS = 2

[<Literal>] 
let BATTERYWATTS = 3

let msgToName = function
    | SolarTextChanged _ -> "SolarTextChanged" 
    | BatteryTextChanged _ -> "BatteryTextChanged"
    | CheckChanged _ -> "CheckChanged"
    | TabChanged _ -> "TabChanged" 
    | GridChanged _ -> "GridChanged"
    | GridSave -> "GridSave"
        
let update (st:State) (m:Msg) = 
    
    let tryFloat (x:string) =
        let t, f = Double.TryParse x
        if t then Some f 
        else None
    
    match m with 
    | BatteryTextChanged(i, s)
    | SolarTextChanged(i, s) ->
        match i with
        | SOLARVOLTS ->
            {st with SolarVolts = tryFloat s}
        | SOLARAMPS -> 
            {st with SolarAmps = tryFloat s} 
        | BATTERYVOLTS ->
            {st with BatteryVolts = tryFloat s}
        | BATTERYWATTS -> 
            {st with BatteryWatts = tryFloat s}
        | _ -> st
    | CheckChanged b -> {st with BatteryFull = b}
    | TabChanged i -> {st with CurrentTab = i}
    | GridChanged (i, s) -> 
        match i with 
        | 0 ->
            {st with ElectricitykWhLeft = tryFloat s}
        | 1 -> {st with ElectricityWatts = tryFloat s} 
        | _ -> st
    | GridSave -> st
