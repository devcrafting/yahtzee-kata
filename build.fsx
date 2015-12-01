//-------------------------
// FAKE Targets
//-------------------------

#r @"packages/FAKE/tools/FakeLib.dll"
open Fake
open Fake.FscHelper
open Fake.FileUtils
open Fake.Testing.XUnit2

let references = !! "packages/FsUnit.xUnit/**/*.dll"
                  ++ "packages/FSharp.Core/lib/net40/*.dll"
                  ++ "packages/xunit.abstractions/lib/net35/*.dll"
                  ++ "packages/xunit.assert/lib/dotnet/*.dll"
                  ++ "packages/xunit.extensibility.core/lib/dotnet/*.dll"
                  ++ "packages/xunit.extensibility.execution/lib/dotnet/*.dll"
                  ++ "packages/System.Runtime/ref/dotnet/*.dll"

Target "Build" (fun _ ->
    !! "*.fs"
    |> Seq.toList
    |> Fsc (fun p ->
              { p with Output = "build/Lib.dll"; FscTarget = Library;
                    References = references |> Seq.toList })
)

Target "Test" (fun _ ->
    !! ("build/Lib.dll")
    |> xUnit2 (fun p ->
      { p with ErrorLevel = DontFailBuild; NoAppDomain = true; ToolPath = "packages/xunit.runner.console/tools/xunit.console.exe"; })
)

Target "CopyReferencesToBuildOutputDir" (fun _ ->
    references |> Seq.iter (fun r -> cp r "build/")
)

Target "Watch" (fun _ ->
    use watcher =
        !! "*.fs"
        |> WatchChanges (fun changes ->
            ExecutedTargetTimes.Clear()
            tracefn "%A" changes
            Run "Test"
        )

    System.Console.ReadLine() |> ignore //Needed to keep FAKE from exiting

    watcher.Dispose() // Use to stop the watch from elsewhere, ie another task.
)


"CopyReferencesToBuildOutputDir"
  ==> "Build"

"Build"
  ==> "Test"

"Test"
  ==> "Watch"

RunTargetOrDefault "Watch"
