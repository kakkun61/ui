using Example.Protobuf;

using System;
using System.Runtime.InteropServices;
using Microsoft.UI.Xaml;
using Windows.ApplicationModel;

using Google.Protobuf;
using System.Linq;

namespace Example
{
    unsafe public partial class App : Application
    {
        readonly Hui.Application<Flags> app = new(new Hui.Program(Logic.Program), flags => flags.ToByteArray(), View.Decode);

        public App()
        {
            InitializeComponent();
            Suspending += OnSuspending;
            Logic.Start();
        }

        protected override void OnLaunched(LaunchActivatedEventArgs args)
        {
            app.Initialize(new Flags());
        }

        void OnSuspending(object sender, SuspendingEventArgs e)
        {
            // Save application state and stop any background activity
        }
    }

    static class View
    {
        public static Hui.View Decode(byte[] bytes)
        {
            var pb = Component.Parser.ParseFrom(bytes);
            return Convert(pb);
        }

        static Hui.View Convert(Component component)
        {
            switch (component.ComponentCase)
            {
                case Component.ComponentOneofCase.View:
                    return Convert(component.View);
                case Component.ComponentOneofCase.Button:
                    return Convert(component.Button);
            }
            throw new Exception("can't reach here");
        }

        static Hui.View Convert(Component.Types.View view)
        {
            return Hui.View.NewView(view.Children.Select(Convert));
        }

        static Hui.View Convert(Component.Types.Button button)
        {
            return Hui.View.NewButton(button.Content);
        }
    }

    static class Logic
    {
        [DllImport("HuiLib.dll", EntryPoint = "HuiStart")]
        public static extern void Start();

        [DllImport("HuiLib.dll", EntryPoint = "HuiEnd")]
        public static extern void End();

        [DllImport("HuiLib.dll", EntryPoint = "HuiMain")]
        public static extern unsafe void Program(Hui.InTag inTag, byte* flags, int flagsSize, void* model, byte* view, int viewSize, int* writtenViewSizePtr, Hui.Give give);
    }
}
