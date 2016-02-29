namespace Game 

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media

type Viewport(width, height) as control =
    inherit UserControl()

    let grid = Grid(Background = SolidColorBrush Colors.Gray)
    let canvas = Canvas(Background = SolidColorBrush Colors.Black)
    do  canvas.Width <- width; canvas.Height <- height
    let clip = RectangleGeometry(Rect=Rect(Width=canvas.Width,Height=canvas.Height))
    do  canvas.Clip <- clip
    let transform =
        ScaleTransform(
            ScaleX=1.0,
            ScaleY=1.0,
            CenterX=width/2.0,
            CenterY=height/2.0
        )
    
    do  canvas.RenderTransform <- transform
    do  grid.Children.Add(canvas) |> ignore
    do  control.Content <- grid

    member control.Canvas = canvas

    member control.Border
        with get () = grid.Background
        and set brush = grid.Background <- brush

    member control.AddText(text) =
        let message = 
            TextBlock(
                Text=text,
                HorizontalAlignment = HorizontalAlignment.Center,
                VerticalAlignment = VerticalAlignment.Center,
                Foreground = SolidColorBrush Colors.White,
                FontSize = 16.0)        

        grid.Children.Add(message)
        { new IDisposable with
            member __.Dispose() = grid.Children.Remove(message) |> ignore
        }

    override control.MeasureOverride(size) =
        let mutable scale = 1.0
        while (width * scale) < size.Width && 
              (height * scale) < size.Height do 
              scale <- scale + 0.5
        let scale = scale - 0.5
        let scale = if scale < 1.0 then 1.0 else scale       
        if transform.ScaleX <> scale then
            transform.ScaleX <- scale
            transform.ScaleY <- scale       
        size