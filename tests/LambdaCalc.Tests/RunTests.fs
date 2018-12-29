namespace LambdaCalc.Tests

open Expecto

module RunTests =

    [<EntryPoint>]
    let main args =

        [Tests.runTestsWithArgs defaultConfig args Tests.testCond]
        |> List.sum


