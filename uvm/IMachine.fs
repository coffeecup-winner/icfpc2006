module Generic
    
    type IMachine =
        abstract member R0 : uint32 with get, set
        abstract member R1 : uint32 with get, set
        abstract member R2 : uint32 with get, set
        abstract member R3 : uint32 with get, set
        abstract member R4 : uint32 with get, set
        abstract member R5 : uint32 with get, set
        abstract member R6 : uint32 with get, set
        abstract member R7 : uint32 with get, set
        abstract member Run : unit -> unit
