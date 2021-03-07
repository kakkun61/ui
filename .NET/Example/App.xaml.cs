using PB = Example.Protobuf;

using System;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Runtime.InteropServices;
using Microsoft.UI.Xaml;
using Windows.ApplicationModel;

using Google.Protobuf;
using Optional;
using Optional.Linq;

namespace Example
{
    unsafe public partial class App : Application
    {
        readonly Hui.Application<PB.Flags, Message, Command> app =
            new(new Hui.Program(Logic.Program), flags => flags.ToByteArray(), Message.Encode, View.Decode, Command.Decode, OnCommand);

        public App()
        {
            InitializeComponent();
            Logic.Start();
        }

        protected override void OnLaunched(LaunchActivatedEventArgs args) => app.Run(new PB.Flags());

        static Optional.Option<Message> OnCommand(Command command) => Option.None<Message>();
    }

    static class View
    {
        [return: NotNull]
        public static Hui.View<Message> Decode([DisallowNull] byte[] bytes) =>
            Convert(PB.Component.Parser.ParseFrom(bytes));

        [return: NotNull]
        static Hui.View<Message> Convert([DisallowNull] PB.Component component) =>
            component.ComponentCase switch
            {
                PB.Component.ComponentOneofCase.View => ConvertIn(component.View),
                PB.Component.ComponentOneofCase.Button => ConvertIn(component.Button),
                _ => throw new InvalidOperationException("it can't reach here"),
            };

        [return: NotNull]
        static Hui.View<Message> ConvertIn([DisallowNull] PB.Component.Types.View view) =>
            Hui.View<Message>.NewView(view.Children.Select(Convert));

        [return: NotNull]
        static Hui.View<Message> ConvertIn([DisallowNull] PB.Component.Types.Button button) =>
            Hui.View<Message>.NewButton(button.Content, button.OnClick.SomeNotNull().Select(Message.ConvertIn));
    }

    class Message
    {
        public sealed class ButtonClicked : Message { }

        [return: NotNull]
        public static byte[] Encode([DisallowNull] Message message) => ConvertOut(message).ToByteArray();

        [return: NotNull]
        public static Message ConvertIn([DisallowNull] PB.Message message) =>
            message.MessageCase switch
            {
                PB.Message.MessageOneofCase.ButtonClicked => new ButtonClicked(),
                _ => throw new InvalidOperationException("it can't reach here"),
            };

        [return: NotNull]
        static PB.Message ConvertOut([DisallowNull] Message message) =>
            message switch
            {
                ButtonClicked _ => new PB.Message
                {
                    ButtonClicked = new PB.Message.Types.ButtonClicked()
                },
                _ => throw new InvalidOperationException("it can't reach here"),
            };
    }

    class Command
    {
        public sealed class NoOp : Command { }

        [return: NotNull]
        public static Command Decode([DisallowNull] byte[] bytes) => new NoOp();
    }

    static class Logic
    {
        [DllImport("Logic.dll")]
        public static extern void Start();

        [DllImport("Logic.dll")]
        public static extern void End();

        [DllImport("Logic.dll")]
        public static extern unsafe void Program(byte* flags, int flagsSize, void* model, byte* view, int viewSize, int* writtenViewSizePtr, byte* messagePtr, int messageSize, byte* commandPtr, int commandSize, int* writtenCommandSizePtr);
    }
}
