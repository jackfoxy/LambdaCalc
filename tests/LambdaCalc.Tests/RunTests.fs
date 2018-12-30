namespace LambdaCalc.Tests

open Expecto

module RunTests =

    [<EntryPoint>]
    let main args =

        [
            Tests.runTestsWithArgs defaultConfig args Tests.testCond
            Tests.runTestsWithArgs defaultConfig args Tests.testNumbers
            Tests.runTestsWithArgs defaultConfig args Tests.testLists
            Tests.runTestsWithArgs defaultConfig args Tests.testRecursion
        ]
        |> List.sum
