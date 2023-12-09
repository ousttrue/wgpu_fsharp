module GlfwUtil

open Silk.NET.GLFW

// NativePtr
open Microsoft.FSharp.NativeInterop
#nowarn "9"


type GlfwWindow(glfw: Glfw, window: nativeptr<WindowHandle>) =
    let glfw = glfw
    let window = window

    member _.GetNativeWindow() = GlfwNativeWindow(glfw, window)

    member _.GetSize() =
        let mutable width: int = 0
        let mutable height: int = 0
        glfw.GetWindowSize(window, &width, &height)
        (uint width, uint height)

    member _.IsClosing() : bool = glfw.WindowShouldClose window

    interface System.IDisposable with
        member _.Dispose() =
            printfn "[glfw] DestroyWindow()"
            glfw.DestroyWindow(window)


type GlfwApp() =
    let glfw = GlfwProvider.GLFW.Value

    do
        if not <| glfw.Init() then
            failwith "Failed to initialize GLFW"

        printfn "[glfw] Init()"


    member _.CreateWindow(width, height, title) : Option<GlfwWindow> =
        do glfw.WindowHint(WindowHintClientApi.ClientApi, ClientApi.NoApi)

        let window =
            glfw.CreateWindow(width, height, title, NativePtr.nullPtr, NativePtr.nullPtr)

        if window = NativePtr.nullPtr then
            printfn "[glfw] CreateWindow() fail"
            None
        else
            printfn "[glfw] CreateWindow()"
            Some(new GlfwWindow(glfw, window))


    member _.Poll() = glfw.PollEvents()

    interface System.IDisposable with
        member _.Dispose() =
            printfn "[glfw] Terminate()"
            glfw.Terminate()
