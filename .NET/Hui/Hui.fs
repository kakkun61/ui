namespace Hui

open Microsoft.UI.Xaml.Controls

type private UIWindow = Microsoft.UI.Xaml.Window
type private HorizontalAlignment = Microsoft.UI.Xaml.HorizontalAlignment
type private VerticalAlignment = Microsoft.UI.Xaml.VerticalAlignment

type Window() as self =
    inherit UIWindow()

    let button = new Button(Content = "Click me")
    let panel = new StackPanel(Orientation = Orientation.Horizontal,
                               HorizontalAlignment = HorizontalAlignment.Center,
                               VerticalAlignment = VerticalAlignment.Center)

    do
        self.Content <- panel
        panel.Children.Add(button)

type Application() =
    member _.Initialize () = ()

    member _.Deinitialize () = ()

    member _.OnLaunched (_ : Microsoft.UI.Xaml.LaunchActivatedEventArgs) =
        (new Window()).Activate()
