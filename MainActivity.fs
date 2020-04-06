namespace EnergyTracker

open System 
open Android.App
open Android.Content
open Android.OS
open Android.Runtime
open Android.Views
open Android.Widget  
open System.Collections.Generic 
open Lib
open Model 
open Android.Text

[<Activity (Label = "EnergyTracker", MainLauncher = true, Icon = "@mipmap/icon")>]
type MainActivity() =
    inherit Activity()
    override this.OnCreate(bundle) =
        base.OnCreate(bundle)

        let model =
            ModelAndUpdater<State, Msg, string>
                (update, msgToName, State.New(), "", this)

        let editTexts = ResizeArray<EditText>()

        let addLabelEditPair (textChangedEvent, tid, lbltxt, container : LinearLayout) =
            let edit = new EditText(this, Id = tid)
            let lbl = new TextView(this, Text = lbltxt)
            edit.InputType <- InputTypes.ClassNumber ||| InputTypes.NumberFlagDecimal
            edit.TextChanged.Add
                (fun _ -> model.Dispatch(textChangedEvent (tid, edit.Text)))
            container.AddView lbl
            container.AddView edit  
            editTexts.Add edit
        
        let createTab(f, txt:string) =
            let tab = this.ActionBar.NewTab() 
            tab.TabSelected.Add f
            this.ActionBar.AddTab(tab.SetText txt) 
        
        
        this.ActionBar.NavigationMode <- ActionBarNavigationMode.Tabs 

        let parent = new LinearLayout(this)

        let solarTab = new LinearLayout(this, Orientation = Orientation.Vertical)
        let solarLabel = new TextView(this)
        let batteryLabel = new TextView(this) 

        let button = new Button(this, Text = "Save")
        button.Click.Add(fun _ -> model.Dispatch SolarSave)
        
        let space = new View(this)
        space.SetBackgroundColor(Android.Graphics.Color.Gray)
        space.LayoutParameters <- new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MatchParent,
                                                                height = 2, BottomMargin = 15,
                                                                TopMargin = 15)

        solarTab.LayoutParameters <- new ViewGroup.LayoutParams(LinearLayout.LayoutParams.MatchParent,
                                                              LinearLayout.LayoutParams.MatchParent)
 
        solarTab.AddView button
        solarTab.AddView solarLabel
        solarTab.AddView batteryLabel
        solarTab.AddView space
        addLabelEditPair (SolarTextChanged, SOLARVOLTS, "Solar Volts", solarTab)
        addLabelEditPair (SolarTextChanged, SOLARAMPS, "Solar Amps", solarTab)
        addLabelEditPair (SolarTextChanged, BATTERYVOLTS, "Battery Volts", solarTab)
        addLabelEditPair (SolarTextChanged, BATTERYWATTS, "Battery Watts", solarTab)

        let buildCheckBox (txt,ischecked, f, parent:LinearLayout) =
            let checkbox = new CheckBox(this, Text = txt, Checked = ischecked)
            checkbox.CheckedChange.Add (fun e -> model.Dispatch (f e.IsChecked)) 
            parent.AddView checkbox

        buildCheckBox("Battery full or stabilizing", false, CheckChanged, solarTab)
        buildCheckBox("Allow updating of Solar Volts text", false, CheckSVChanged, solarTab)
        
        let smsg = new TextView(this)         
        solarTab.AddView smsg

        //---

        let electricityTab = new LinearLayout(this, Orientation = Orientation.Vertical)
        electricityTab.LayoutParameters <- solarTab.LayoutParameters
                
        let button2 = new Button(this, Text = "Save")
        button2.Click.Add(fun _ -> model.Dispatch ElectricitySave)
        electricityTab.AddView button2 

        addLabelEditPair (ElectricityChanged, ELECTRICITYLEFT, "kWh Left: ", electricityTab)
        addLabelEditPair (ElectricityChanged, WATTSUSE, "Power use: ", electricityTab)

        let emsg = new TextView(this)         
        electricityTab.AddView emsg

        buildCheckBox
            ("Renewing", false, ElectricityRenewalChecked, electricityTab)
                
        createTab ((fun _ -> model.Dispatch (TabChanged 0)), "Solar")
        createTab ((fun _ -> model.Dispatch (TabChanged 1)), "Grid")
        
        //----
        
        let updateSolarMsg (s : State) =
            let prev, h = s.SolarHistory

            let lookup hr =
                match h.TryFind hr with
                | None -> ""
                | Some vs ->
                    [| for v in vs do
                           if v.DateTime.Date >= DateTime.Now.Date.AddDays(-1.) 
                              && (DateTime.Now - v.DateTime).TotalHours <= 4. then
                               yield v.DateTime,
                                     string v.DateTime + " | "
                                     + fstring v.BatteryVolts + "V | "
                                     + fstring v.SolarAmps + "A" + " | "
                                     + fstring v.BatteryWatts + "W" |]
                    |> Array.sortByDescending fst
                    |> Array.map snd
                    |> String.concat "\n"
            if prev.SolarAmps <> -1. then
                let recentVolts =
                    [ for h in 0.0..4.0 ->
                          lookup (DateTime.Now.AddHours(-h).Hour) ]
                    |> String.concat "\n"
                smsg.Text <- String.Format
                                 ("Previously at {0}: Battery: {1} V, Solar Amps: {2} A\n\nRecently:\n{3}",
                                  prev.DateTime, prev.BatteryVolts,
                                  prev.SolarAmps, recentVolts)

        let updateElectricityMsg (s : State) =
            let table() =
                match recent 20 s with
                | Some es ->
                    let str =
                        es.[max 0 (es.Length - 10)..]
                        |> Array.map
                               (fun e ->
                               String.Format
                                   ("{0} | {1} | {2}", string e.DateTime, fstring e.EnergyLeft,
                                    fstring e.PowerInUse))
                        |> Array.rev
                        |> String.concat "\n"
                    let day1, slope, zeroLoc = getZero es
                    String.Format("{0}\n\nSlope: {3} kWh/day, 5 kWh in ~{1} days, {2}",str, fstring zeroLoc,day1.AddDays zeroLoc, fstring slope)
                | None -> ""
            match s.ElectricitykWhLeft, s.ElectricityWatts with
            | Some kwh, Some w ->
                emsg.Text <- String.Format
                                ("Previously at {2}: Energy Left: {0} kWh, Power: {1} kW\n\nRecent:\n{3}",
                                kwh, w, (fst s.EnergyHistory).DateTime, table())
            | _ -> ()

        let changeTab (st : State) =
            match st.CurrentTab with
            | 0 ->
                parent.RemoveView(electricityTab)
                parent.AddView(solarTab)
            | 1 ->
                parent.RemoveView(solarTab)
                parent.AddView(electricityTab)
            | _ -> ()

        let tryChangeSolarVolts (s:State) =
            if s.AllowSVChange then 
                maybe { 
                    let! sa = s.SolarAmps
                    let! bw = s.BatteryWatts 
                    do editTexts.[SOLARVOLTS].Text <- fstring (bw / sa) }   
                |> ignore 

        let saveToast (s:State) =  
            if s.LastSaveSuccessful then
                Toast.MakeText(this, "Saved", ToastLength.Long).Show()

        model.Connect("TabChanged", changeTab) 
        model.Connect ("SolarSave", saveToast)
        model.Connect ("SolarSave", updateSolarMsg)
        model.Connect ("ElectricitySave", saveToast)
        model.Connect ("ElectricitySave", updateElectricityMsg)
        
        model.Connect("SolarTextChanged",
            (fun s -> 
                maybe { let! sv = s.SolarVolts
                        let! sa = s.SolarAmps 
                        let updatetxt =  "Solar Watts: " + fstring (sa * sv)
                        if solarLabel.Text <> updatetxt then solarLabel.Text <- updatetxt} 
                |> ignore 
                maybe { let! bv = s.BatteryVolts
                        let! bw = s.BatteryWatts
                        let solartxt = match s.SolarAmps with 
                                        | None -> ""
                                        | Some sa -> " (Solar Watts: " + fstring(bv * sa) + ")"
                        let utxt = "Battery Amps: " + fstring (bw / bv) + solartxt
                        if utxt <> batteryLabel.Text then batteryLabel.Text <- utxt } 
                |> ignore
                
                tryChangeSolarVolts s))

        parent.AddView solarTab

        this.SetContentView(parent)
        model.Connect ("Init", "ShowPrevious", ignore)
        model.Connect ("ShowPrevious", updateElectricityMsg)
        model.Connect ("ShowPrevious", updateSolarMsg)
        model.Dispatch (Init this)