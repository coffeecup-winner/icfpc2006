module Generic

open System.IO
    
type IMachine =
    abstract member R0 : uint32 with get, set
    abstract member R1 : uint32 with get, set
    abstract member R2 : uint32 with get, set
    abstract member R3 : uint32 with get, set
    abstract member R4 : uint32 with get, set
    abstract member R5 : uint32 with get, set
    abstract member R6 : uint32 with get, set
    abstract member R7 : uint32 with get, set
    abstract member Arrays : System.Collections.Generic.List<uint32 array> with get
    abstract member SetIOStreams : Stream -> Stream -> unit
    abstract member Run : uint32 array -> unit
