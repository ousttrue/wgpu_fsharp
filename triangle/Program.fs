module Program

open System
open Silk.NET.GLFW


let rec Loop
    (app: GlfwUtil.GlfwApp)
    (window: GlfwUtil.GlfwWindow)
    (_renderer: WgpuUtil.Renderer)
    (pipeline: WGPU.Native.RenderPipeline)
    =
    if not <| window.IsClosing() then
        app.Poll()

        let width, height = window.GetSize()

        let renderer =
            if width = _renderer.SwapchainWidth
               && height = _renderer.SwapchainHeight then
                _renderer
            else
                let newSwapchain = _renderer.SwapchainRecreate width height

                { _renderer with
                    Swapchain = newSwapchain
                    SwapchainWidth = width
                    SwapchainHeight = height }

        let nextTexture = renderer.Swapchain.GetCurrentTextureView()

        if nextTexture.handle = IntPtr.Zero then
            printfn "Failed to acquire SwapChain texture"

        let mutable encoderDescriptor =
            WGPU.Native.CommandEncoderDescriptor(label = "Command Encoder")

        let encoder = renderer.Device.CreateCommandEncoder &encoderDescriptor

        let mutable colorAttachment =
            WGPU.Native.RenderPassColorAttachment(
                view = nextTexture,
                resolveTarget = Unchecked.defaultof<WGPU.Native.TextureView>,
                loadOp = WGPU.Native.LoadOp.Clear,
                storeOp = WGPU.Native.StoreOp.Store,
                clearValue = WGPU.Native.Color(r = 0.01, g = 0.01, b = 0.01, a = 1)
            )

        let colorAttachmentPtr = WgpuUtil.intptr.alloc colorAttachment

        let mutable renderPassDescriptor =
            WGPU.Native.RenderPassDescriptor(colorAttachments = colorAttachmentPtr.ptr, colorAttachmentCount = 1u)

        let renderPass = encoder.BeginRenderPass &renderPassDescriptor
        renderPass.SetPipeline pipeline
        renderPass.Draw(3u, 1u, 0u, 0u)
        renderPass.End()
        let queue = renderer.Device.GetQueue()
        let mutable commandBufferDescriptor = WGPU.Native.CommandBufferDescriptor()
        let mutable commandBuffer = encoder.Finish &commandBufferDescriptor
        queue.Submit(1u, &&commandBuffer)
        renderer.Swapchain.Present()

        Loop app window renderer pipeline


let GetNativeValue (nativeWindow: GlfwNativeWindow) : Option<WgpuUtil.NativeWindowValue> =
    if System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Windows) then
        let hwnd, _hdc, hinstance = nativeWindow.Win32.Value.ToTuple()
        printfn "[Win32]"
        Some(WgpuUtil.Win32(hinstance, hwnd))

    else if
        System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Linux)
    then
        if System.String.IsNullOrEmpty(System.Environment.GetEnvironmentVariable("WAYLAND_DISPLAY")) then
            let d, w = nativeWindow.X11.Value.ToTuple()
            printfn "[X11]"
            Some(WgpuUtil.X11(d, w))
        else
            let d, s = nativeWindow.Wayland.Value.ToTuple()
            printfn "[Wayland]"
            Some(WgpuUtil.Wayland(d, s))
    else if
        System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.OSX)
    then
        let nw = nativeWindow.Cocoa.Value
        printfn "[OSX]"
        Some(WgpuUtil.Cocoa(nw))
    else
        None


[<EntryPoint>]
let main (argv: string []) : int =
    use app = new GlfwUtil.GlfwApp()

    match (app.CreateWindow(600, 600, "triangle")) with
    | Some (window) ->
        use window = window

        match (GetNativeValue(window.GetNativeWindow())) with
        | Some (native) ->
            let (width, height) = window.GetSize()

            match (WgpuUtil.CreateDevice native width height) with
            | Some (renderer) ->

                match (WgpuUtil.CreatePipeline renderer.Device renderer.SwapchainFormat) with
                | Some (pipeline) ->
                    Loop app window renderer pipeline
                    0
                | None -> 5
            | None -> 4
        | None -> 2
    | None -> 1
