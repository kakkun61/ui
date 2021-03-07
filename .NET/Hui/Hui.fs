﻿#nowarn "9" // Possible unverifiable code

namespace Hui

open System
open System.Collections.Generic
open FSharp.NativeInterop
open Microsoft.UI.Xaml

type UIStackPanel = Microsoft.UI.Xaml.Controls.StackPanel
type UIButton = Microsoft.UI.Xaml.Controls.Button

/// <param name="flags">a pinter to a flags memory area</param>
/// <param name="flagsSize">a size of a flags memory area</param>
/// <param name="model">a pointer to a pointer to a model</param>
/// <param name="view">a pointer to a view memory area</param>
/// <param name="viewSize">a size of a view memory area</param>
/// <param name="writtenViewSizePtr">a pointer to a size of a written view memory area</param>
/// <param name="messagePtr">a pointer to a message memory area</param>
/// <param name="messageSize">a size of a message memory area</param>
/// <param name="command">a pointer to a command memory area</param>
/// <param name="commandSize">a size of a command memory area</param>
/// <param name="writtenCommandSizePtr">a pointer to a size of a written command memory area</param>
type Program =
  delegate of
    flags : byte nativeptr
    * flagsSize : int
    * model : voidptr
    * view : byte nativeptr
    * viewSize : int
    * writtenViewSizePtr : int nativeptr
    * messagePtr : byte nativeptr
    * messageSize : int
    * command : byte nativeptr
    * commandSize : int
    * writtenCommandSizePtr : int nativeptr
    -> unit

type ('flags, 'message) Argument =
    | InitArgument of flasg : 'flags
    | CommandArgument of message : 'message

type 'message View =
    | View of children : ('message View) IEnumerable
    | Button of content : string * onClick : 'message Optional.Option

type 'a Encode = delegate of 'a -> byte array

type 'a Decode = delegate of byte array -> 'a

type ('command, 'message) OnCommand = delegate of 'command -> 'message Optional.Option

module View =
    let rec instanciate onEvent prev now : UIElement =
        match prev with
        | None ->
            match now with
            | View children ->
                let panel = UIStackPanel ()
                for child in children do
                    panel.Children.Add (instanciate onEvent None child)
                upcast panel
            | Button (content, onClick)->
                upcast onClick.Match(
                    (fun onClick ->
                        let button = UIButton (Content = content)
                        button.Click.Add (onEvent onClick)
                        button),
                    (fun () -> UIButton (Content = content)))
        | Some prev -> raise (NotImplementedException ())

type
    ('flags, 'message, 'command)
    Application
    (program : Program,
     encodeFlags : 'flags Encode,
     encodeMessage : 'message Encode,
     decodeView : ('message View) Decode,
     decodeCommand : 'command Decode,
     onCommand : ('command, 'message) OnCommand) =

    let viewSize = 512

    let viewBytes = Array.zeroCreate viewSize

    let commandSize = 512

    let commandBytes = Array.zeroCreate commandSize

    /// <remarks>This is an array to use <c>fixed</c> operator.</remarks>
    let modelPtr : nativeint array = Array.zeroCreate 1

    let nullptr = NativePtr.ofNativeInt IntPtr.Zero

    let mutable window = Unchecked.defaultof<Window>

    member this.Run (flags : 'flags) =
        let (view, command) = this.Program (InitArgument flags)
        window <- Window (Content = View.instanciate this.OnEvent None view)
        window.Activate ()
        (onCommand.Invoke command).MatchSome (fun message -> this.OnMessage message)

    member this.OnEvent message _ = this.OnMessage message

    member this.OnMessage message =
        let mutable next = Optional.Option.Some<'message> message
        while next.HasValue do
            next.MatchSome (fun message ->
                let (view, command) = this.Program (CommandArgument message)
                window.Content <- View.instanciate this.OnEvent None view
                next <- onCommand.Invoke command)

    member _.Program arg =
        use modelPtrPtr = fixed modelPtr
        use viewPtr = fixed viewBytes
        let writtenViewSizes = Array.zeroCreate 1
        use writtenViewSizePtr = fixed writtenViewSizes
        use commandPtr = fixed commandBytes
        let writtenCommandSizes = Array.zeroCreate 1
        use writtenCommandSizePtr = fixed writtenCommandSizes
        match arg with
        | InitArgument flags ->
            let flagsBytes = encodeFlags.Invoke flags
            use flagsPtr = fixed flagsBytes
            program.Invoke
              (flagsPtr,
               flagsBytes.Length,
               NativePtr.toVoidPtr modelPtrPtr,
               viewPtr,
               viewSize,
               writtenViewSizePtr,
               nullptr,
               0,
               commandPtr,
               commandSize,
               writtenCommandSizePtr)
            let writtenViewSize = writtenViewSizes.[0]
            let view = decodeView.Invoke viewBytes.[0..writtenViewSize-1]
            let writtenCommandSize = writtenCommandSizes.[0]
            let command = decodeCommand.Invoke commandBytes.[0..writtenCommandSize-1]
            (view, command)
        | CommandArgument message ->
            let messageBytes = encodeMessage.Invoke message
            use messagePtr = fixed messageBytes
            program.Invoke
              (nullptr,
               -1,
               NativePtr.toVoidPtr modelPtrPtr,
               viewPtr,
               viewSize,
               writtenViewSizePtr,
               messagePtr,
               messageBytes.Length,
               commandPtr,
               commandSize,
               writtenCommandSizePtr)
            let writtenViewSize = writtenViewSizes.[0]
            let view = decodeView.Invoke viewBytes.[0..writtenViewSize-1]
            let writtenCommandSize = writtenCommandSizes.[0]
            let command = decodeCommand.Invoke commandBytes.[0..writtenCommandSize-1]
            (view, command)
