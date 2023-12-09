module WgpuUtil

open System

// NativePtr
open Microsoft.FSharp.NativeInterop

#nowarn "9"


[<System.Runtime.CompilerServices.IsByRefLike>]
type intptr<'a> =
    struct
        val ptr: nativeint
        new internal ptr = { ptr = ptr }
    end
    static member alloc(item: 'a) =
        let ptr =
            System.Runtime.InteropServices.Marshal.AllocHGlobal(System.Runtime.InteropServices.Marshal.SizeOf(item))

        System.Runtime.InteropServices.Marshal.StructureToPtr(item, ptr, false)
        new intptr<'a> (ptr)

    interface IDisposable with
        member this.Dispose() =
            System.Runtime.InteropServices.Marshal.FreeHGlobal this.ptr


type NativeWindowValue =
    | Win32 of hinstance: nativeint * hwnd: nativeint
    | X11 of display: nativeint * window: unativeint
    | Cocoa of layer: nativeint


let CreateSurface (instance: WGPU.Native.Instance) (native: NativeWindowValue) : Option<WGPU.Native.Surface> =
    match (native) with
    | Win32 (hinstance, hwnd) ->
        printfn "[WGPU] [Win32] surface"

        let mutable info =
            WGPU.Native.SurfaceDescriptorFromWindowsHWND(
                WGPU.Native.ChainedStruct(sType = WGPU.Native.SType.SurfaceDescriptorFromWindowsHWND),
                hinstance,
                hwnd
            )

        let mutable surfaceDescriptor =
            WGPU.Native.SurfaceDescriptor(NativePtr.toNativeInt &&info, "SurfaceDescriptor")

        let surface = instance.CreateSurface &surfaceDescriptor
        Some(surface)
    | X11 (d, w) ->
        printfn "[WGPU] [X11] surface"

        let mutable info =
            WGPU.Native.SurfaceDescriptorFromXlibWindow(
                WGPU.Native.ChainedStruct(sType = WGPU.Native.SType.SurfaceDescriptorFromXlibWindow),
                d,
                w.ToUInt32()
            )

        let mutable surfaceDescriptor =
            WGPU.Native.SurfaceDescriptor(NativePtr.toNativeInt &&info, "SurfaceDescriptor")

        let surface = instance.CreateSurface &surfaceDescriptor
        Some(surface)
    | Cocoa (nw) ->
        printfn "[WGPU] Created Metal surface descriptor"

        let mutable info =
            WGPU.Native.SurfaceDescriptorFromMetalLayer(
                WGPU.Native.ChainedStruct(sType = WGPU.Native.SType.SurfaceDescriptorFromMetalLayer),
                nw
            )

        let mutable surfaceDescriptor =
            WGPU.Native.SurfaceDescriptor(NativePtr.toNativeInt &&info, "SurfaceDescriptor")

        let surface = instance.CreateSurface &surfaceDescriptor
        Some(surface)


type Renderer =
    { Device: WGPU.Native.Device
      Swapchain: WGPU.Native.SwapChain
      SwapchainFormat: WGPU.Native.TextureFormat
      SwapchainWidth: uint
      SwapchainHeight: uint
      SwapchainRecreate: uint -> uint -> WGPU.Native.SwapChain }


let CreateDevice (native: NativeWindowValue) (width: uint) (height: uint) : Option<Renderer> =
    printfn "[WGPU] WGPU instance"
    let instance = WGPU.Native.Instance()

    match (CreateSurface instance native) with
    | Some (surface) ->

        printfn "[WGPU] graphics adapter"

        let adapter =
            let mutable adapterOptions =
                WGPU.Native.RequestAdapterOptions(
                    compatibleSurface = surface,
                    powerPreference = WGPU.Native.PowerPreference.Undefined
                )

            let mutable adapter = WGPU.Native.Adapter()

            let callback
                (_status: WGPU.Native.RequestAdapterStatus)
                (_adapter: WGPU.Native.Adapter)
                (_message: string)
                (_userdata: nativeint)
                =
                printfn $"{_message}"
                adapter <- _adapter

            instance.RequestAdapter(&&adapterOptions, WGPU.Native.RequestAdapterCallback callback, IntPtr.Zero)

            adapter

        let adapterProps =
            let mutable props = WGPU.Native.AdapterProperties()
            adapter.GetProperties &props
            props

        printfn
            $"[WGPU] Adapter Properties{Environment.NewLine}{adapterProps.Name} {adapterProps.AdapterType}, {adapterProps.BackendType}, {adapterProps.DriverDescription}, 0x{adapterProps.DeviceID:X16}, 0x{adapterProps.VendorID:X16}"

        let deviceExtras =
            WGPU.Native.DeviceExtras(
                WGPU.Native.ChainedStruct(sType = enum<WGPU.Native.SType> (int WGPU.Native.NativeSType.DeviceExtras)),
                enum<WGPU.Native.NativeFeature> 0,
                "Device",
                ""
            )

        use deviceExtrasPtr = intptr.alloc deviceExtras

        let mutable requiredLimits =
            WGPU.Native.RequiredLimits(limits = WGPU.Native.Limits())

        let mutable deviceDescriptor =
            WGPU.Native.DeviceDescriptor(
                deviceExtrasPtr.ptr,
                "Device",
                0u,
                IntPtr.Zero,
                Microsoft.FSharp.NativeInterop.NativePtr.toNativeInt &&requiredLimits
            )

        let mutable device = WGPU.Native.Device()
        let deviceCallback _status _device _message _userdata = device <- _device
        printfn "[WGPU] Creating device"

        let swapchainFormat = surface.GetPreferredFormat(adapter)

        let recreateSwapchain width height =
            printfn "[WGPU] Creating swapchain"

            let mutable swapchainDescriptor =
                WGPU.Native.SwapChainDescriptor(
                    usage = WGPU.Native.TextureUsage.RenderAttachment,
                    format = swapchainFormat,
                    width = width,
                    height = height,
                    presentMode = WGPU.Native.PresentMode.Fifo
                )

            device.CreateSwapChain(surface, &swapchainDescriptor)

        adapter.RequestDevice(&deviceDescriptor, WGPU.Native.RequestDeviceCallback deviceCallback, IntPtr.Zero)

        let swapchain = recreateSwapchain width height

        let renderer =
            { Renderer.Device = device
              Swapchain = swapchain
              SwapchainFormat = swapchainFormat
              SwapchainWidth = width
              SwapchainHeight = height
              SwapchainRecreate = recreateSwapchain }

        Some renderer
    | None -> None


let CreatePipeline
    (device: WGPU.Native.Device)
    (swapchainFormat: WGPU.Native.TextureFormat)
    : Option<WGPU.Native.RenderPipeline> =

    printfn "[WGPU] Creating shader"

    let shaderSource =
        """
@stage(vertex)
fn vs_main(@builtin(vertex_index) in_vertex_index: u32) -> @builtin(position) vec4<f32> {
    let x = f32(i32(in_vertex_index) - 1);
    let y = f32(i32(in_vertex_index & 1u) * 2 - 1);
    return vec4<f32>(x, y, 0.0, 1.0);
}

@stage(fragment)
fn fs_main() -> @location(0) vec4<f32> {
    return vec4<f32>(1.0, 0.0, 0.0, 1.0);
}
"""

    let wgslDescriptor =
        WGPU.Native.ShaderModuleWGSLDescriptor(
            WGPU.Native.ChainedStruct(sType = WGPU.Native.SType.ShaderModuleWGSLDescriptor),
            shaderSource
        )

    let wgslDescriptorPtr = intptr.alloc wgslDescriptor

    let mutable shaderDescriptor =
        WGPU.Native.ShaderModuleDescriptor(wgslDescriptorPtr.ptr, "shader.wgsl")

    let shader = device.CreateShaderModule &shaderDescriptor

    if shader.handle = IntPtr.Zero then
        printfn "Failed to create shader module"
        exit 1

    printfn "[WGPU] Creating pipeline layout"

    let mutable pipelineLayoutDescriptor =
        WGPU.Native.PipelineLayoutDescriptor(bindGroupLayoutCount = 0u)

    let pipelineLayout = device.CreatePipelineLayout(&pipelineLayoutDescriptor)

    let mutable blendState =
        WGPU.Native.BlendState(
            color =
                WGPU.Native.BlendComponent(
                    WGPU.Native.BlendOperation.Add,
                    WGPU.Native.BlendFactor.One,
                    WGPU.Native.BlendFactor.Zero
                ),
            alpha =
                WGPU.Native.BlendComponent(
                    WGPU.Native.BlendOperation.Add,
                    WGPU.Native.BlendFactor.One,
                    WGPU.Native.BlendFactor.Zero
                )
        )

    let blendStatePtr = intptr.alloc blendState

    let mutable colorTargetState =
        WGPU.Native.ColorTargetState(
            format = swapchainFormat,
            blend = blendStatePtr.ptr,
            writeMask = WGPU.Native.ColorWriteMask.All
        )

    let colorTargetStatePtr = intptr.alloc colorTargetState

    let mutable fragmentState =
        WGPU.Native.FragmentState(
            ``module`` = shader,
            entryPoint = "fs_main",
            targetCount = 1u,
            targets = colorTargetStatePtr.ptr
        )

    let fragmentStatePtr = intptr.alloc fragmentState

    let mutable renderPipelineDescriptor =
        WGPU.Native.RenderPipelineDescriptor(
            label = "Render pipeline",
            layout = pipelineLayout,
            vertex = WGPU.Native.VertexState(``module`` = shader, entryPoint = "vs_main", bufferCount = 0u),
            primitive =
                WGPU.Native.PrimitiveState(
                    topology = WGPU.Native.PrimitiveTopology.TriangleList,
                    stripIndexFormat = WGPU.Native.IndexFormat.Undefined,
                    frontFace = WGPU.Native.FrontFace.CCW,
                    cullMode = WGPU.Native.CullMode.None
                ),
            multisample =
                WGPU.Native.MultisampleState(count = 1u, mask = UInt32.MaxValue, alphaToCoverageEnabled = false),
            fragment = fragmentStatePtr.ptr
        )

    printfn "[WGPU] Creating pipeline"
    let renderPipeline = device.CreateRenderPipeline &renderPipelineDescriptor
    Some(renderPipeline)
