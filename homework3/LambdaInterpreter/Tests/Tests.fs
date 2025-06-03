namespace LambdaInterpreter.Tests

open NUnit.Framework
open FsUnit
open LambdaInterpreter.LambdaInterpreter 

module L =
    let v name = Var name
    let app f x = App (f, x)
    let abs x body = Abs (x, body)

    let I = abs "x" (v "x")
    let K = abs "x" (abs "y" (v "x"))
    let S = abs "x" (abs "y" (abs "z" (app (app (v "x") (v "z")) (app (v "y") (v "z")))))
    let K_star = abs "x" (abs "y" (v "y"))


module Tests =
    open L 

    let fvTestCases =
        [|
            v "x", Set.singleton "x", "FV_SingleVariable"
            app (v "x") (v "y"), Set.ofList ["x"; "y"], "FV_Application"
            abs "x" (v "y"), Set.singleton "y", "FV_Abstraction_FreeVarRemains"
            abs "x" (v "x"), Set.empty, "FV_Abstraction_VarBecomesBound"
            abs "x" (app (v "y") (abs "z" (app (v "x") (v "w")))), Set.ofList ["y"; "w"], "FV_Abstraction_Nested"
        |]
        |> Array.map (fun (term, expected, name) -> TestCaseData(term, expected).SetName(name))

    [<TestCaseSource("fvTestCases")>]
    let ``FV Tests`` (term: Term, expected: Set<string>) =
        FV term |> should equal expected

    let newNameTestCases =
        [|
            Set.empty, "x0", "NewName_Simple"
            Set.singleton "x0", "x1", "NewName_AvoidsConflict"
            Set.ofList ["x0"; "x1"; "x2"], "x3", "NewName_AvoidsMultipleConflicts"
        |]
        |> Array.map (fun (fvSet, expected, name) -> TestCaseData(fvSet, expected).SetName(name))

    [<TestCaseSource("newNameTestCases")>]
    let ``NewName Tests`` (fvSet: Set<string>, expected: string) =
        newName "x" fvSet |> should equal expected

    let substituteTestCases =
        [|
            v "x", "x", v "y", v "y", "Substitute_VarForVar_Match"
            v "z", "x", v "y", v "z", "Substitute_VarForVar_NoMatch"
            app (v "x") (v "z"), "x", v "y", app (v "y") (v "z"), "Substitute_InApplication"
            abs "y" (v "x"), "x", v "z", abs "y" (v "z"), "Substitute_InAbstraction_VarNotBound"
            I, "x", v "z", I, "Substitute_InAbstraction_VarIsBound" 
            abs "y" (v "x"), "x", v "y", abs "y0" (v "y"), "Substitute_CaptureAvoidance_Simple"
            abs "y" (app (v "x") (v "y")), "x", abs "z" (v "y"), abs "y0" (app (abs "z" (v "y")) (v "y0")), "Substitute_CaptureAvoidance_Complex"
            abs "y" (v "z"), "x", v "w", abs "y" (v "z"), "Substitute_NoFreeX_InAbs"
            abs "y" (v "z"), "x", v "y", abs "y" (v "z"), "Substitute_BoundVarInT_NotCaptured_NoFV_x_in_S"
        |]
        |> Array.map (fun (term, varName, replacement, expected, name) ->
            TestCaseData(term, varName, replacement, expected).SetName(name))

    [<TestCaseSource("substituteTestCases")>]
    let ``Substitute Tests`` (term: Term, varName: string, replacement: Term, expected: Term) =
        substitute term varName replacement |> should equal expected

    let reduceOnceTestCases =
        [|
            app (abs "x" (v "x")) (v "y"), Some (v "y"), "ReduceOnce_SimpleBetaReduction"
            v "x", None, "ReduceOnce_NoRedex_Var"
            I, None, "ReduceOnce_NoRedex_Abs"
            app (v "x") (v "y"), None, "ReduceOnce_NoRedex_App_NormalArgs"
            app (app (abs "x" (v "x")) (v "y")) (v "z"), Some (app (v "y") (v "z")), "ReduceOnce_InFunctionOfApp"
            app (v "x") (app (abs "y" (v "y")) (v "z")), Some (app (v "x") (v "z")), "ReduceOnce_InArgumentOfApp"
            abs "z" (app (abs "x" (v "x")) (v "y")), Some (abs "z" (v "y")), "ReduceOnce_InBody_Of_Abs"
        |]
        |> Array.map (fun (term, expected, name) -> TestCaseData(term, expected).SetName(name))

    [<TestCaseSource("reduceOnceTestCases")>]
    let ``ReduceOnce Tests`` (term: Term, expected: Option<Term>) =
        reduceOnce term |> should equal expected

    let p = v "p"
    let q = v "q"
    let id_z = abs "z" (v "z") 
    let id_w = abs "w" (v "w") 

    let evalTestCases =
        [|
            app I (v "y"), v "y", "Eval_Identity"
            app (app K p) q, p, "Eval_K_Combinator"
            app (app K_star p) q, q, "Eval_KI_Combinator"
            app (app (abs "x" (abs "y" (app (v "x") (v "y")))) id_z) id_w, id_w, "Eval_MultipleSteps"
            app (app S K) K, abs "z" (v "z"), "SKK = I"
            v "x", v "x", "Eval_AlreadyNormalForm_Var"
            I, I, "Eval_AlreadyNormalForm_Abs"
        |]
        |> Array.map (fun (term, expected, name) -> TestCaseData(term, expected).SetName(name))

    [<TestCaseSource("evalTestCases")>]
    let ``Eval Tests`` (term: Term, expected: Term) =
        eval term |> should equal expected