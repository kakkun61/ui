#nowarn "9" // Possible unverifiable code

namespace Hui

open System.Collections.Generic
open Microsoft.UI.Xaml
open Microsoft.UI.Xaml.Controls
open FSharp.NativeInterop

type InTag =
    | Init = 0

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
/// <param name="give">a pointer to a continuation</param>
type Program = delegate of inTag : InTag * flags : byte nativeptr * flagsSize : int * model : voidptr * view : byte nativeptr * viewSize : int * writtenViewSizePtr : int nativeptr * give : Give -> unit

type View =
    | View of children : View IEnumerable
    | Button of content : string

type 'a Encode = delegate of 'a -> byte array

type 'a Decode = delegate of byte array -> 'a

exception Unimplemented of string

module View =
    let rec instanciate (prev : View option) now : UIElement =
        match prev with
        | None ->
            match now with
            | View children ->
                let panel = new StackPanel ()
                for child in children do
                    panel.Children.Add (instanciate None child)
                upcast panel
            | Button content ->
                upcast new Button (Content = content)
        | Some prev -> raise (Unimplemented "")

type 'flags Application (program : Program, encodeFlags : 'flags Encode, decodeView : View Decode) =
    let viewSize = 512

    /// <remarks>This is an array to use <c>fixed</c> operator.</remarks>
    let modelPtr : nativeint array = Array.zeroCreate 1

    let viewBytes = Array.zeroCreate viewSize

    member this.Initialize (flags : 'flags) =
        let (view, outTag) = this.Program InTag.Init flags
        (new Window (Content = View.instanciate None view)).Activate ()

    member _.Program inTag flags =
        let flagsBytes = encodeFlags.Invoke flags
        use flagsPtr = fixed flagsBytes
        use modelPtrPtr = fixed modelPtr
        use viewPtr = fixed viewBytes
        let writtenViewSizes = Array.zeroCreate 1
        use writtenViewSizePtr = fixed writtenViewSizes
        let mutable outTag = OutTag.Noop
        let give = Give (fun t -> outTag <- t)
        program.Invoke (inTag, flagsPtr, flagsBytes.Length, NativePtr.toVoidPtr modelPtrPtr, viewPtr, viewSize, writtenViewSizePtr, give)
        let writtenViewSize = writtenViewSizes.[0]
        let view = decodeView.Invoke viewBytes.[0..writtenViewSize-1]
        (view, outTag)
