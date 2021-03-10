using PB = Example.Protobuf;

using System;
using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Runtime.InteropServices;
using Microsoft.UI.Xaml;

using Google.Protobuf;
using Optional;
using Optional.Linq;

namespace Example
{
    public unsafe partial class App : Application
    {
        readonly Hui.Application<PB.Flags, Message, Command> app =
            new(new Hui.Program(Logic.Program), flags => flags.ToByteArray(), Message.Encode, View.Decode, Commands.Decode, OnCommand);

        public App()
        {
            InitializeComponent();
            Logic.Start();
        }

        protected override void OnLaunched(LaunchActivatedEventArgs args) => app.Run(new PB.Flags());

        [return: NotNull]
        static Optional.Option<Message> OnCommand([DisallowNull] Command command) =>
            command switch
            {
                Command.NoOp => Option.None<Message>(),
                Command.GetDotNetDescription => Option.Some<Message>(new Message.DotNetDescription(RuntimeInformation.FrameworkDescription)),
                _ => throw new InvalidOperationException("it can't reach here"),
            };
    }

    static class View
    {
        [return: NotNull]
        public static Hui.View<Message> Decode([DisallowNull] byte[] bytes) =>
            ConvertIn(PB.Component.Parser.ParseFrom(bytes));

        [return: NotNull]
        static Hui.View<Message> ConvertIn([DisallowNull] PB.Component component) =>
            component.ComponentCase switch
            {
                PB.Component.ComponentOneofCase.View => ConvertIn(component.View),
                PB.Component.ComponentOneofCase.Button => ConvertIn(component.Button),
                _ => throw new InvalidOperationException("it can't reach here"),
            };

        [return: NotNull]
        static Hui.View<Message> ConvertIn([DisallowNull] PB.Component.Types.View view) =>
            Hui.View<Message>.NewView(view.Children.Select(ConvertIn));

        [return: NotNull]
        static Hui.View<Message> ConvertIn([DisallowNull] PB.Component.Types.Button button) =>
            Hui.View<Message>.NewButton(button.Content, button.OnClick.SomeNotNull().Select(Message.ConvertIn));
    }

    class Message
    {
        public sealed class ButtonClicked : Message { }

        public sealed class DotNetDescriptionButtonClicked : Message { }

        public sealed class DotNetDescription : Message
        {
            [NotNull]
            public string Description { get; }

            public DotNetDescription([DisallowNull] string description)
            {
                Description = description;
            }
        }

        [return: NotNull]
        public static byte[] Encode([DisallowNull] Message message) => ConvertOut(message).ToByteArray();

        [return: NotNull]
        public static Message ConvertIn([DisallowNull] PB.Message message) =>
            message.MessageCase switch
            {
                PB.Message.MessageOneofCase.ButtonClicked => new ButtonClicked(),
                PB.Message.MessageOneofCase.DotNetDescriptionButtonClicked => new DotNetDescriptionButtonClicked(),
                _ => throw new InvalidOperationException("it can't reach here"),
            };

        [return: NotNull]
        static PB.Message ConvertOut([DisallowNull] Message message) =>
            message switch
            {
                ButtonClicked => new PB.Message
                {
                    ButtonClicked = new PB.Message.Types.ButtonClicked()
                },
                DotNetDescriptionButtonClicked => new PB.Message
                {
                    DotNetDescriptionButtonClicked = new PB.Message.Types.DotNetDescriptionButtonClicked()
                },
                DotNetDescription m => new PB.Message
                {
                    DotNetDescription = new PB.Message.Types.DotNetDescription()
                    {
                        Description = m.Description
                    }
                },
                _ => throw new InvalidOperationException("it can't reach here"),
            };
    }

    class Command
    {
        public sealed class NoOp : Command { }

        public sealed class GetDotNetDescription : Command { }

        [return: NotNull]
        public static Command ConvertIn([DisallowNull] PB.Commands.Types.Command command) =>
            command.CommandCase switch
            {
                PB.Commands.Types.Command.CommandOneofCase.NoOp => new NoOp(),
                PB.Commands.Types.Command.CommandOneofCase.GetDotNetDescription => new GetDotNetDescription(),
                _ => throw new InvalidOperationException("it can't reach here"),
            };
    }

    class Commands
    {
        [return: NotNull]
        public static IEnumerable<Command> Decode([DisallowNull] byte[] bytes) =>
            ConvertIn(PB.Commands.Parser.ParseFrom(bytes));

        [return: NotNull]
        static IEnumerable<Command> ConvertIn([DisallowNull] PB.Commands commands) => commands.Commands_.Select(Command.ConvertIn);
    }

    static class Logic
    {
        [DllImport("Logic.dll")]
        public static extern void Start();

        [DllImport("Logic.dll")]
        public static extern void End();

        [DllImport("Logic.dll")]
        public static extern unsafe void Program(byte* flags, int flagsSize, void* model, byte* view, int viewSize, int* writtenViewSizePtr, byte* messagePtr, int messageSize, byte* commandsPtr, int commandsSize, int* writtenCommandsSizePtr);
    }
}
