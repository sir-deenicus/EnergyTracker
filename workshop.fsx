#r @"packages\Newtonsoft.Json.12.0.3\lib\netstandard2.0\Newtonsoft.Json.dll"

open System
open System.IO
open Newtonsoft.Json
open System.Collections.Generic

type SolarState =
    { DateTime : DateTime
      SolarVolts : float
      SolarAmps : float
      BatteryVolts : float
      BatteryWatts : float
      HasLoad : bool
      BatteryFull : bool }

type SolarState2 =
    { DateTime : DateTime
      SolarVolts : float
      SolarAmps : float
      BatteryVolts : float
      BatteryWatts : float
      BatteryFull : bool }

let doUpgrade () =
    let txt = IO.File.ReadAllText (Path.Combine(__SOURCE_DIRECTORY__, @"solar.txt"))

    let prev, h = JsonConvert.DeserializeObject<SolarState * Dictionary<int, SolarState ResizeArray>>(txt)

    let upgrade (s:SolarState) =
        {   DateTime = s.DateTime
            SolarVolts = s.SolarVolts
            SolarAmps = s.SolarAmps
            BatteryVolts = s.BatteryVolts
            BatteryWatts = s.BatteryWatts
            BatteryFull = s.BatteryFull }

    let keys = h.Keys |> Seq.toArray

    let h' = Dictionary<int, SolarState2 ResizeArray>()

    for k in keys do h'.Add(k, h.[k] |> Seq.toArray |> Array.map upgrade |> ResizeArray)

    IO.File.WriteAllText(Path.Combine(__SOURCE_DIRECTORY__, @"solar.txt"), JsonConvert.SerializeObject((upgrade prev, h')))

//////////////////////////////////////////////////////////////////////////////////////////

type EnergyState =
    { DateTime : DateTime
      EnergyLeft : float
      PowerInUse : float }

let txt = IO.File.ReadAllText (Path.Combine(__SOURCE_DIRECTORY__, @"energyuse.txt"))

let _, h = JsonConvert.DeserializeObject<EnergyState * ResizeArray<EnergyState ResizeArray>>(txt)

let simpleStats (vec : float []) (vec2 : float []) =
    let xm, ym = vec |> Array.average, vec2 |> Array.average
    let cov_, varx_ =
        (vec, vec2)
        ||> Array.fold2
                (fun (cov, varx) x y ->
                cov + (x - xm) * (y - ym), varx + ((x - xm)**2.))
                (0., 0.)
    let len = float vec.Length
    let cov, varx = cov_ / len, varx_ / len
    let beta = cov / varx
    {|Slope = beta; Intercept = ym - beta * xm; Covariance = cov; VarianceX = varx|}

let toPoints (es : EnergyState []) =
    let points =
        es
        |> Array.map (fun e -> e.DateTime, e.EnergyLeft)
        |> Array.sortBy fst
    let d1,_ = points.[0]
    d1, points |> Array.map (fun (d,x) -> (d - d1).TotalDays, x)

let getZero(es : EnergyState []) =
    let d1,points = toPoints es
    let stats =
        points
        |> Array.unzip
        ||> simpleStats
    d1, stats.Slope, stats.Intercept, (5. - stats.Intercept) / stats.Slope

let recent take (h:ResizeArray<EnergyState ResizeArray>) =
        let last = h.[h.Count - 1]
        let len = last.Count
        last.ToArray().[max 0 (len - take)..]

recent 20 h |> toPoints
recent 20 h |> getZero

#load @"C:\Users\cybernetic\Jupyter-Notebooks\maths.fsx"

open Prelude.Math
open Prelude.Common
open XPlot.Plotly

Maths.adjustGrapher()

let data = recent 20 h |> toPoints |> snd

let m, b = data |> Array.unzip ||> Stats.simpleStats |> fst_snd5

let rline = [|for x in 0.0..0.5..1.5 -> x , m * x + b|]

let combined data rline =
    let xs, ys = Array.unzip data
    let xr, yr = Array.unzip rline

    [ Scatter(x = xs, y = ys, mode = "markers")
      Scatter(x = xr, y = yr, mode = "line") ]
    |> Chart.Plot

combined data rline

[rline; data] |> Chart.Line

Maths.clearFiles()