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
                                                                height = 5, BottomMargin = 15,
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

        let checkbox = new CheckBox(this, Text = "Battery full or stabilizing")
        checkbox.CheckedChange.Add (fun e -> model.Dispatch (CheckChanged e.IsChecked)) 
        solarTab.AddView checkbox 
        
        let checkboxLoad = new CheckBox(this, Text = "Has Load", Checked = true)
        checkboxLoad.CheckedChange.Add (fun e -> model.Dispatch (CheckLoadChanged e.IsChecked)) 
        solarTab.AddView checkboxLoad 

        let electricityTab = new LinearLayout(this, Orientation = Orientation.Vertical)
        electricityTab.LayoutParameters <- solarTab.LayoutParameters
                
        let button2 = new Button(this, Text = "Save")
        button2.Click.Add(fun _ -> model.Dispatch ElectricitySave)
        electricityTab.AddView button2 

        addLabelEditPair (ElectricityChanged, 4, "kWh Left: ", electricityTab)
        addLabelEditPair (ElectricityChanged, 5, "Power use: ", electricityTab)

        let emsg = new TextView(this)

        let checkboxRenewing = new CheckBox(this, Text = "Renewing")
        checkboxRenewing.CheckedChange.Add (fun e -> model.Dispatch (ElectricityRenewalChecked e.IsChecked)) 
        electricityTab.AddView checkboxRenewing 
        electricityTab.AddView emsg
                
        createTab ((fun _ -> model.Dispatch (TabChanged 0)), "Solar")
        createTab ((fun _ -> model.Dispatch (TabChanged 1)), "Grid")
         
        let updateE (s:State) =
            match s.ElectricitykWhLeft, s.ElectricityWatts with  
            | Some kwh, Some w -> 
                 emsg.Text <- String.Format("Previously at {2}: Energy Left: {0} kWh, Power: {1} kW", kwh, w, (fst s.EnergyHistory).DateTime)
            | _ -> ()
        
        let changeTab (st : State) =
            match st.CurrentTab with 
            | 0 ->  parent.RemoveView(electricityTab); parent.AddView(solarTab)
            | 1 -> parent.RemoveView(solarTab); parent.AddView(electricityTab)  
            | _ -> ()

        let tryChangeBatteryVolts (s:State) =
            match s.SolarVolts with 
            | None -> 
                maybe { 
                    let! sa = s.SolarAmps
                    let! bw = s.BatteryWatts 
                    do editTexts.[SOLARVOLTS].Text <- string (bw / sa) }   
                |> ignore
            | _ -> ()

        let saveToast (s:State) =  
            if s.LastSaveSuccessful then
                Toast.MakeText(this, "Saved", ToastLength.Long).Show()

        model.Connect("TabChanged", changeTab) 
        model.Connect ("SolarSave", saveToast)
        model.Connect ("ElectricitySave", saveToast)
        model.Connect ("ElectricitySave", updateE)
        
        model.Connect("SolarTextChanged",
            (fun s -> 
                maybe { let! sv = s.SolarVolts
                        let! sa = s.SolarAmps 
                        let updatetxt =  "Solar Watts: " + string (sa * sv)
                        if solarLabel.Text <> updatetxt then solarLabel.Text <- updatetxt} 
                |> ignore 
                maybe { let! bv = s.BatteryVolts
                        let! bw = s.BatteryWatts
                        let solartxt = match s.SolarAmps with 
                                        | None -> ""
                                        | Some sa -> " (Solar Watts: " + string(bv * sa) + ")"
                        let utxt = "Battery Amps: " + string (bw / bv) + solartxt
                        if utxt <> batteryLabel.Text then batteryLabel.Text <- utxt } 
                |> ignore
                
                tryChangeBatteryVolts s))

        parent.AddView solarTab

        this.SetContentView(parent)
        model.Connect ("Init", "ShowPrev", ignore)
        model.Connect ("ShowPrev", updateE)
        model.Dispatch (Init this)