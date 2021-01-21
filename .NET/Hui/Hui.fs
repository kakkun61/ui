namespace Hui

open System.Runtime.InteropServices
open Microsoft.UI.Xaml.Controls

type private UIWindow = Microsoft.UI.Xaml.Window
type private HorizontalAlignment = Microsoft.UI.Xaml.HorizontalAlignment
type private VerticalAlignment = Microsoft.UI.Xaml.VerticalAlignment

module Haskell =
    [<DllImport("HuiLib.dll")>]
    extern void HuiStart()

    [<DllImport("HuiLib.dll")>]
    extern void HuiEnd()

    [<DllImport("HuiLib.dll")>]
    extern int32 Add(int32, int32)

type Window() as self =
    inherit UIWindow()

    let button = new Button(Content = "Click me")
    let panel = new StackPanel(Orientation = Orientation.Horizontal,
                               HorizontalAlignment = HorizontalAlignment.Center,
                               VerticalAlignment = VerticalAlignment.Center)

    do
        self.Content <- panel
        panel.Children.Add(button)
        button.Click.Add(fun _ ->
            button.Content <- Haskell.Add(1, 2)
        )

type Application() =
    member _.Initialize () =
        Haskell.HuiStart()

    member _.Deinitialize () = ()

    member _.OnLaunched (_ : Microsoft.UI.Xaml.LaunchActivatedEventArgs) =
        (new Window()).Activate()
