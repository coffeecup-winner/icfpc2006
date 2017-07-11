namespace JitTests

open NUnit.Framework
open JitCompiler

[<TestFixture>]
module JitTests =

    [<Test>]
    let empty_script () =
        jit_compile([||]) |> ignore
