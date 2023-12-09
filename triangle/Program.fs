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


let GetNativeValue (nativeWindow: GlfwNativeWindow) : WgpuUtil.NativeWindowValue =
    if System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Windows) then
        let hwnd, _hdc, hinstance = nativeWindow.Win32.Value.ToTuple()
        WgpuUtil.Win32(hinstance, hwnd)

    else if
        System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Linux)
    then
        let d, w = nativeWindow.X11.Value.ToTuple()
        WgpuUtil.X11(d, w)
    else
        let nw = nativeWindow.Cocoa.Value
        WgpuUtil.Cocoa(nw)


[<EntryPoint>]
let main (argv: string []) : int =
    use app = new GlfwUtil.GlfwApp()

    match (app.CreateWindow(600, 600, "triangle")) with
    | Some (window) ->
        use window = window

        match (GetNativeValue(window.GetNativeWindow())) with
        | WgpuUtil.Win32 (_, _) as native ->
            let (width, height) = window.GetSize()

            match (WgpuUtil.CreateDevice native width height) with
            | Some (renderer) ->

                match (WgpuUtil.CreatePipeline renderer.Device renderer.SwapchainFormat) with
                | Some (pipeline) ->
                    Loop app window renderer pipeline
                    0
                | None -> 5
            | None -> 4
        | WgpuUtil.X11 (_, _) -> 2
        | WgpuUtil.Cocoa (_) -> 3
    | None -> 1
