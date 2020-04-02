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
                (update, msgToName,
                 { SolarVolts = None
                   SolarAmps = None
                   BatteryVolts = None 
                   BatteryWatts = None
                   BatteryFull = false
                   CurrentTab = 0 
                   ElectricitykWhLeft = None
                   ElectricityWatts = None}, "", this)

        let addLabelEditPair (textChangedEvent, tid, lbltxt, container : LinearLayout) =
            let edit = new EditText(this, Id = tid)
            let lbl = new TextView(this, Text = lbltxt)
            edit.InputType <- InputTypes.ClassNumber ||| InputTypes.NumberFlagDecimal
            edit.TextChanged.Add
                (fun _ -> model.Dispatch(textChangedEvent (tid, edit.Text)))
            container.AddView lbl
            container.AddView edit  
        
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
        
        let space = new View(this)
        space.SetBackgroundColor(Android.Graphics.Color.Gray)
        space.LayoutParameters <- new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MatchParent,
                                                                13, BottomMargin = 15,
                                                                TopMargin = 15)

        solarTab.LayoutParameters <- new ViewGroup.LayoutParams(LinearLayout.LayoutParams.MatchParent,
                                                              LinearLayout.LayoutParams.MatchParent)
 
        solarTab.AddView button
        solarTab.AddView solarLabel
        solarTab.AddView batteryLabel
        solarTab.AddView space
        addLabelEditPair (SolarTextChanged, SOLARVOLTS, "Solar Volts", solarTab)
        addLabelEditPair (SolarTextChanged, SOLARAMPS, "Solar Amps", solarTab)
        addLabelEditPair (BatteryTextChanged, BATTERYVOLTS, "Battery Volts", solarTab)
        addLabelEditPair (BatteryTextChanged, BATTERYWATTS, "Battery Watts", solarTab)

        let checkbox = new CheckBox(this, Text = "Battery full")

        checkbox.CheckedChange.Add (fun e -> model.Dispatch (CheckChanged e.IsChecked)) 

        solarTab.AddView checkbox 
        
        let electricityTab = new LinearLayout(this, Orientation = Orientation.Vertical)
        electricityTab.LayoutParameters <- solarTab.LayoutParameters

        
        addLabelEditPair (GridChanged, 0, "kWh Left: ", electricityTab)
        addLabelEditPair (GridChanged, 1, "Power use: ", electricityTab)
        
        createTab ((fun _ -> model.Dispatch (TabChanged 0)), "Solar")
        createTab ((fun _ -> model.Dispatch (TabChanged 1)), "Grid")
         
        let changeTab (st : State) =
            match st.CurrentTab with 
            | 0 ->  parent.RemoveView(electricityTab); parent.AddView(solarTab)
            | 1 -> parent.RemoveView(solarTab); parent.AddView(electricityTab)  
            | _ -> ()

        model.Connect("TabChanged", changeTab)

        model.Connect("SolarTextChanged",
            (fun s ->
                let res =
                    maybe { let! sv = s.SolarVolts
                            let! sa = s.SolarAmps
                            return "Solar Watts: " + string (sa * sv) }
                match res with
                | None -> ()
                | Some sv -> solarLabel.Text <- sv))

        model.Connect("BatteryTextChanged",
            (fun s ->
                let res =
                    maybe { let! bv = s.BatteryVolts
                            let! bw = s.BatteryWatts
                            return "Battery Amps: " + string (bw / bv) }
                match res with
                | None -> ()
                | Some sv -> batteryLabel.Text <- sv))

        parent.AddView solarTab

        this.SetContentView(parent)