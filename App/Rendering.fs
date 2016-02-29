module Rendering

open System
open System.Windows.Controls
open System.Windows.Media

let run (control:Control) rate update =
    let rate = TimeSpan.FromSeconds(rate)
    let focus = ref true
    let pause = TimeSpan.FromSeconds(0.5)
    let lastUpdate = ref (DateTime.Now + pause)
    let residual = ref (TimeSpan.Zero)
    let gotFocus _ =
        focus := true
        let pause = TimeSpan.FromSeconds(0.5)
        lastUpdate := DateTime.Now + pause
        residual := TimeSpan.Zero
    let lostFocus _ = 
        focus := false
    let subscriptions = [
        control.GotFocus.Subscribe(gotFocus)
        control.LostFocus.Subscribe(lostFocus)
        CompositionTarget.Rendering.Subscribe (fun _ ->
            let now = DateTime.Now
            if now >= !lastUpdate then
                residual := !residual + (now - !lastUpdate)
                if !focus then
                    while !residual > rate do
                        update(); residual := !residual - rate
                lastUpdate := now
        )]
    { new IDisposable with
        member this.Dispose() =
            subscriptions |> List.iter (fun d -> d.Dispose())
    }