module Model
 
open System 
open Android.OS
open Newtonsoft.Json
open System.Collections.Generic
open Lib 
open Android.Content

[<Literal>] 
let SOLARVOLTS = 0

[<Literal>] 
let SOLARAMPS = 1

[<Literal>] 
let BATTERYVOLTS = 2

[<Literal>] 
let BATTERYWATTS = 3

type Msg = 
    | SolarTextChanged of int * string   
    | CheckChanged of bool
    | CheckLoadChanged of bool
    | ElectricityRenewalChecked of bool
    | TabChanged of int
    | ElectricityChanged of int * string 
    | Init of Context 
    | ElectricitySave  
    | SolarSave 

let msgToName = function
    | SolarTextChanged _ -> "SolarTextChanged"  
    | CheckChanged _ -> "CheckChanged"
    | TabChanged _ -> "TabChanged" 
    | ElectricityChanged _ -> "GridChanged"
    | CheckLoadChanged _ -> "CheckLoadChanged"
    | ElectricityRenewalChecked _ -> "ElectricityRenewalChecked"
    | Init _ -> "Init"
    | m -> string m

type EnergyState =
    { DateTime : DateTime
      EnergyLeft : float
      PowerInUse : float }
    static member New() =
        { DateTime = DateTime.MinValue
          EnergyLeft = -1.
          PowerInUse = -1. }

type SolarState =
    { DateTime : DateTime
      SolarVolts : float
      SolarAmps : float
      BatteryVolts : float
      BatteryWatts : float
      HasLoad : bool
      BatteryFull : bool }
    static member New() =
        { DateTime = DateTime.Now
          SolarVolts = 0.
          SolarAmps = 0.
          BatteryVolts = 0.
          BatteryWatts = 0.
          HasLoad = false
          BatteryFull = false }

type State =
    { SolarHistory : SolarState * Dictionary<int, SolarState ResizeArray>
      EnergyHistory : EnergyState * ResizeArray<EnergyState ResizeArray>
      SolarVolts : float option
      SavePath : string
      SolarAmps : float option
      BatteryVolts : float option
      BatteryWatts : float option
      BatteryFull : bool
      HasLoad : bool
      CurrentTab : int
      IsElectricityRenewal : bool
      LastSaveSuccessful : bool
      ElectricitykWhLeft : float option
      ElectricityWatts : float option }

    member s.ExtractSolarState() =
        maybe {
            let! sv = s.SolarVolts
            let! sa = s.SolarAmps
            let! bv = s.BatteryVolts
            let! bw = s.BatteryWatts
            return { DateTime = DateTime.Now
                     SolarVolts = sv
                     SolarAmps = sa
                     BatteryVolts = bv
                     BatteryWatts = bw
                     HasLoad = s.HasLoad
                     BatteryFull = s.BatteryFull }
        }

    static member New() =
        { SolarHistory = SolarState.New(), Dictionary()
          EnergyHistory = EnergyState.New(), ResizeArray()
          SolarVolts = None
          SolarAmps = None
          BatteryVolts = None
          BatteryWatts = None
          HasLoad = true
          BatteryFull = false
          SavePath = ""
          CurrentTab = 0
          LastSaveSuccessful = false
          IsElectricityRenewal = false
          ElectricitykWhLeft = None
          ElectricityWatts = None }

let docdir (context : Context) =
    context.GetExternalFilesDir(Environment.DirectoryDocuments).AbsolutePath //System.Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments) // Environment.GetExternalStoragePublicDirectory(Environment.DirectoryDocuments).AbsolutePath

let solardat dir = IO.Path.Combine(dir, "solar.txt")
let energydat dir = IO.Path.Combine(dir, "energyuse.txt")

let getElectricityData dir =
    let file = energydat dir
    if IO.File.Exists file then
        JsonConvert.DeserializeObject<EnergyState * ResizeArray<EnergyState ResizeArray>>
            (IO.File.ReadAllText(file))
    else EnergyState.New(), ResizeArray()

let getSolarData dir =
    let file = solardat dir
    if IO.File.Exists file then
        JsonConvert.DeserializeObject<SolarState * Dictionary<int, SolarState ResizeArray>>
            (IO.File.ReadAllText(file))
    else SolarState.New(), Dictionary()

let update (st : State) (m : Msg) =
    let tryFloat (x : string) =
        let t, f = Double.TryParse x
        if t then Some f
        else None
    match m with
    | SolarTextChanged(i, s) ->
        match i with
        | SOLARVOLTS -> { st with SolarVolts = tryFloat s }
        | SOLARAMPS -> { st with SolarAmps = tryFloat s }
        | BATTERYVOLTS -> { st with BatteryVolts = tryFloat s }
        | BATTERYWATTS -> { st with BatteryWatts = tryFloat s }
        | _ -> st
    | CheckChanged b -> { st with BatteryFull = b }
    | CheckLoadChanged b -> { st with HasLoad = b }
    | ElectricityRenewalChecked b -> { st with IsElectricityRenewal = b }
    | TabChanged i -> { st with CurrentTab = i }
    | ElectricityChanged(i, s) ->
        match i with
        | 4 -> { st with ElectricitykWhLeft = tryFloat s }
        | 5 -> { st with ElectricityWatts = tryFloat s }
        | _ -> st
    | ElectricitySave ->
        match (st.ElectricityWatts, st.ElectricitykWhLeft) with
        | Some w, Some kwh ->
            match st.EnergyHistory with
            | prev, h when (prev.PowerInUse, prev.EnergyLeft) <> (w, kwh) ->
                if st.IsElectricityRenewal || h.Count = 0 then
                    h.Add(ResizeArray())
                let currentindex = h.Count - 1

                let newpoint =
                    { DateTime = DateTime.Now
                      EnergyLeft = kwh
                      PowerInUse = w }
                h.[currentindex].Add(newpoint)
                let json = JsonConvert.SerializeObject((newpoint, h))
                IO.File.WriteAllText(energydat st.SavePath, json)
                { st with EnergyHistory = (newpoint, h)
                          LastSaveSuccessful = true }
            | _ -> { st with LastSaveSuccessful = false }
        | _ -> { st with LastSaveSuccessful = false }
    | SolarSave ->
        match st.ExtractSolarState() with
        | None -> { st with LastSaveSuccessful = false }
        | Some currentSolar ->
            match st.SolarHistory with
            | (prev, h) when { prev with DateTime = currentSolar.DateTime }
                             <> currentSolar ->
                let now = DateTime.Now
                match h.TryFind now.Hour with
                | None -> h.Add(now.Hour, ResizeArray([ currentSolar ]))
                | Some ds -> ds.Add currentSolar
                let json = JsonConvert.SerializeObject((currentSolar, h))
                IO.File.WriteAllText(solardat st.SavePath, json)
                { st with SolarHistory = (currentSolar, h)
                          LastSaveSuccessful = true }
            | _ -> { st with LastSaveSuccessful = false }
    | Init c ->
        let dir = docdir c
        let prev, _ as eh = getElectricityData dir

        let kwh, w =
            if prev.EnergyLeft = -1. then None, None
            else Some prev.EnergyLeft, Some prev.PowerInUse
        { st with SolarHistory = (getSolarData dir)
                  SavePath = dir
                  EnergyHistory = eh
                  ElectricityWatts = w
                  ElectricitykWhLeft = kwh }
