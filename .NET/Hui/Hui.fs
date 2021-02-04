#nowarn "9" // Possible unverifiable code

namespace Hui

open System
open System.Collections.Generic
open FSharp.NativeInterop
open Microsoft.UI.Xaml

type UIStackPanel = Microsoft.UI.Xaml.Controls.StackPanel
type UIButton = Microsoft.UI.Xaml.Controls.Button

type InTag =
    | Init = 0
    | Cmd = 1

type OutTag =
    | Noop = 0

type Give = delegate of OutTag -> unit

/// <param name="inTag">a type of an input call (F# → Haskell)</param>
/// <param name="flags">a pinter to a flags memory area</param>
/// <param name="flagsSize">a size of a flags memory area</param>
/// <param name="model">a pointer to a pointer to a model</param>
/// <param name="view">a pointer to a view memory area</param>
/// <param name="viewSize">a size of a view memory area</param>
/// <param name="writtenViewSizePtr">a pointer to a size of a written view memory area</param>
/// <param name="messagePtr">a pointer to a message memory area</param>
/// <param name="messageSize">a size of a message memory area</param>
/// <param name="give">a pointer to a continuation</param>
type Program =
  delegate of
    inTag : InTag
    * flags : byte nativeptr
    * flagsSize : int
    * model : voidptr
    * view : byte nativeptr
    * viewSize : int
    * writtenViewSizePtr : int nativeptr
    * messagePtr : byte nativeptr
    * messageSize : int
    * give : Give
    -> unit

type ('flags, 'message) Argument =
  | InitArgument of flasg : 'flags
  | CommandArgument of message : 'message

type 'message View =
    | View of children : ('message View) IEnumerable
    | Button of content : string * onClick : 'message Optional.Option

type 'a Encode = delegate of 'a -> byte array

type 'a Decode = delegate of byte array -> 'a

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
    ('flags, 'message)
    Application
    (program : Program,
     encodeFlags : 'flags Encode,
     encodeMessage : 'message Encode,
     decodeView : ('message View) Decode) =

    let viewSize = 512

    let viewBytes = Array.zeroCreate viewSize

    /// <remarks>This is an array to use <c>fixed</c> operator.</remarks>
    let modelPtr : nativeint array = Array.zeroCreate 1

    let nullptr = NativePtr.ofNativeInt IntPtr.Zero

    let mutable window = Unchecked.defaultof<Window>

    member this.Initialize (flags : 'flags) =
        let (view, outTag) = this.Program (InitArgument flags)
        window <- Window (Content = View.instanciate this.OnEvent None view)
        window.Activate ()

    member this.OnEvent message _ = 
        let (view, outTag) = this.Program (CommandArgument message)
        window.Content <- View.instanciate this.OnEvent None view

    member _.Program arg =
        match arg with
        | InitArgument flags ->
            let flagsBytes = encodeFlags.Invoke flags
            use flagsPtr = fixed flagsBytes
            use modelPtrPtr = fixed modelPtr
            use viewPtr = fixed viewBytes
            let writtenViewSizes = Array.zeroCreate 1
            use writtenViewSizePtr = fixed writtenViewSizes
            let mutable outTag = OutTag.Noop
            let give = Give (fun t -> outTag <- t)
            program.Invoke
              (InTag.Init,
               flagsPtr,
               flagsBytes.Length,
               NativePtr.toVoidPtr modelPtrPtr,
               viewPtr,
               viewSize,
               writtenViewSizePtr,
               nullptr,
               0,
               give)
            let writtenViewSize = writtenViewSizes.[0]
            let view = decodeView.Invoke viewBytes.[0..writtenViewSize-1]
            (view, outTag)
        | CommandArgument message ->
            use modelPtrPtr = fixed modelPtr
            let messageBytes = encodeMessage.Invoke message
            use messagePtr = fixed messageBytes
            use viewPtr = fixed viewBytes
            let writtenViewSizes = Array.zeroCreate 1
            use writtenViewSizePtr = fixed writtenViewSizes
            let mutable outTag = OutTag.Noop
            let give = Give (fun t -> outTag <- t)
            program.Invoke
              (InTag.Cmd,
               nullptr,
               0,
               NativePtr.toVoidPtr modelPtrPtr,
               viewPtr,
               viewSize,
               writtenViewSizePtr,
               messagePtr,
               messageBytes.Length,
               give)
            let writtenViewSize = writtenViewSizes.[0]
            let view = decodeView.Invoke viewBytes.[0..writtenViewSize-1]
            (view, outTag)
