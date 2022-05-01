module Instance

type WasmModule<'g> =
    {
        name: string
        globals: Map<string,'g ref>
        functions: Map<string, obj -> obj>
        code: byte[]
    }

