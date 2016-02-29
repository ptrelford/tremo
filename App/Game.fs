namespace Game

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Input
open System.Windows.Media
open System.Windows.Shapes
open Microsoft.Xna.Framework.Audio
open Rendering
open Sound
open Utils

[<AutoOpen>]
module Constants =
    let width, height = 400.0, 600.0

type GameControl () as control =
    inherit Viewport(width, height, IsTabStop=true)   

    let ball = Ball(width, height)

    let ballEllipse = Ellipse(Width=ball.Radius*2.0,Height=ball.Radius*2.0)
    do ballEllipse.Visibility <- Visibility.Collapsed

    do ballEllipse.Fill <- SolidColorBrush Colors.Yellow
    do control.Canvas.Children.Add(ballEllipse)

    let rand = Random()

    let mutable lines : Line list = []

    let shape freq duration i = 
        sineWave freq i * fadeOut duration i * tremolo 500. 0.5 i

    let duration = 0.5
    let effects = 
        [
         233.08 // A#
         277.18 // C#
         311.13 // D#
         369.99 // F#
         415.30 // G#
        ]
        |> List.map (fun tone -> shape tone duration)
        |> List.map (fun shape -> createEffect shape duration)

    let update () =
        ballEllipse.Visibility <- Visibility.Visible
        let walls = [|for line in lines -> line.X1, line.Y1, line.X2, line.Y2|]
        let hitBorder, hitWall, x, y = ball.Update walls
        Canvas.SetLeft(ballEllipse, x - ball.Radius)
        Canvas.SetTop(ballEllipse, y - ball.Radius)

        control.Border <-
            SolidColorBrush(if hitBorder then Colors.Gray else Colors.Black)

        if hitWall then 
            let fx = effects.[rand.Next(effects.Length)].CreateInstance()
            fx.Play()

    let transparentGray = 
        SolidColorBrush(Color.FromArgb(128uy, 164uy, 164uy, 164uy))

    let createLine (x,y) =
        let line = Line(X1=x, Y1=y, X2=x, Y2=y)
        line.Stroke <- transparentGray
        line.StrokeThickness <- 2.0
        line

    let circleRadius = 4.0 
    let mutable circles = []

    let createCircle (x,y) color =
        let diameter = circleRadius * 2.0
        let ellipse = Ellipse(Width=diameter,Height=diameter)
        ellipse.Stroke <- SolidColorBrush color
        Canvas.SetLeft(ellipse, x - circleRadius)
        Canvas.SetTop(ellipse, y - circleRadius)
        control.Canvas.Children.Add(ellipse)
        ellipse
    
    let addCircle line f (x,y) color =
        let ellipse = createCircle (x,y) color
        circles <- (line, ellipse, f) :: circles 
        ellipse

    let findCircle (x,y) =
        circles |> List.tryFind (fun (line, ellipse, (g,s)) ->
            let x',y' = g()
            let d = sqrt ((x-x')*(x-x') + (y-y')*(y-y'))
            d < circleRadius
        )

    let getPosition (args:#MouseEventArgs) =
        let position = args.GetPosition(control.Canvas)
        position.X, position.Y

    let rec waiting() = async {
        let! md = Async.AwaitObservable(control.MouseLeftButtonDown)
        let x,y = getPosition md
        match findCircle(x,y) with
        | Some(line:Line, ellipse, (g,s)) ->
            line.Stroke <- transparentGray
            do! drawing(line, ellipse, s)
        | None ->        
            let line = createLine(x,y)
            lines <- line :: lines
            let get1 () = line.X1, line.Y1
            let set1 (x,y) = line.X1 <- x; line.Y1 <- y
            let _ = addCircle line (get1,set1) (x,y) Colors.Red
            let get2 () = line.X2, line.Y2
            let set2 (x,y) = line.X2 <- x; line.Y2 <- y
            let ellipse = addCircle line (get2,set2) (x,y) Colors.Cyan 
            control.Canvas.Children.Add(line) 
            do! drawing(line, ellipse, set2) 
        }

    and drawing(line:Line, ellipse:Ellipse, f) = async {
        let! evt = Async.AwaitObservable(control.MouseLeftButtonUp, control.MouseMove)
        match evt with
        | Choice1Of2(up) ->
            line.Stroke <- SolidColorBrush(Colors.White)
            do! waiting() 
        | Choice2Of2(move) ->
            let x,y = getPosition move
            f (x,y)            
            Canvas.SetLeft(ellipse, x - 4.0)
            Canvas.SetTop(ellipse, y - 4.0)
            do! drawing(line, ellipse, f) }

    do  waiting() |> Async.StartImmediate

    let start () = async { run control (1.0/60.0) update |> ignore }

    do  control.Loaded.Subscribe(fun _ ->
            start() |> Async.StartImmediate
        ) |> ignore
        (*
        if Application.Current.IsRunningOutOfBrowser 
        then start() |> Async.StartImmediate
        else
            let handle = control.AddText "Click To Start"
            async { 
                do! control.MouseLeftButtonDown |> Async.AwaitEvent |> Async.Ignore               
                handle.Dispose()
                do! start ()
            } |> Async.StartImmediate
        ) |> ignore
        *)
     