﻿
module Build.Tasks

open BlackFox.Fake
open System.IO
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.DotNet.Testing
open Fake.Tools

// Information about the project is used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docsrc/tools/generate.fsx"

// The name of the project
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "LambdaCalc"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "Stripped-down lambda calculus"

// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description = "Stripped-down lambda calculus parser using fslexyacc"

// List of author names (for NuGet package)
let authors = "Jack Fox"

// Tags for your project (for NuGet package)
let tags = "F# fsharp lambdacalculus parse parsing interpretter"

// File system information
let solutionFile  = "LambdaCalc.sln"

// Default target configuration
let configuration = "Release"

// Pattern specifying assemblies to be tested using Expecto
let testAssemblies = "tests/**/bin/Release/net47/*Tests.exe"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "jackfoxy"
let gitHome = sprintf "%s/%s" "https://github.com" gitOwner

// The name of the project on GitHub
let gitName = project

let website = sprintf "/%s" project

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
let releaseNotes = ReleaseNotes.load "RELEASE_NOTES.md"

// Helper active pattern for project types
let (|Fsproj|Csproj|Vbproj|Shproj|) (projFileName:string) =
    match projFileName with
    | f when f.EndsWith("fsproj") -> Fsproj
    | f when f.EndsWith("csproj") -> Csproj
    | f when f.EndsWith("vbproj") -> Vbproj
    | f when f.EndsWith("shproj") -> Shproj
    | _                           -> failwith (sprintf "Project file %s not supported. Unknown project type." projFileName)

let createAndGetDefault () =
    let clean = BuildTask.create "Clean" [] {
        Shell.cleanDirs ["bin"; "temp"]
        }

    let cleanDocs = BuildTask.create "CleanDocs" [] {
        Shell.cleanDirs ["docs/reference"; "docs"]
        }

    // Generate assembly info files with the right version & up-to-date information
    let assemblyInfo = BuildTask.create "AssemblyInfo" [clean] {
        let getAssemblyInfoAttributes projectName =
            [   AssemblyInfo.Title (projectName)
                AssemblyInfo.Product project
                AssemblyInfo.Description summary
                AssemblyInfo.Version releaseNotes.AssemblyVersion
                AssemblyInfo.FileVersion releaseNotes.AssemblyVersion
                AssemblyInfo.Configuration configuration ]

        let getProjectDetails projectPath =
            let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
            ( projectPath,
                projectName,
                System.IO.Path.GetDirectoryName(projectPath),
                (getAssemblyInfoAttributes projectName)
            )

        !! "src/**/*.??proj"
        |> Seq.map getProjectDetails
        |> Seq.iter (fun (projFileName, _, folderName, attributes) ->
            match projFileName with
            | Fsproj -> AssemblyInfoFile.createFSharp (folderName </> "AssemblyInfo.fs") attributes
            | Csproj -> AssemblyInfoFile.createCSharp ((folderName </> "Properties") </> "AssemblyInfo.cs") attributes
            | Vbproj -> AssemblyInfoFile.createVisualBasic ((folderName </> "My Project") </> "AssemblyInfo.vb") attributes
            | Shproj -> ()
            )
    }

    let buildConfiguration = DotNet.Custom <| Environment.environVarOrDefault "configuration" configuration

    // --------------------------------------------------------------------------------------
    // Build library & test project

    let build = BuildTask.create "Build" [assemblyInfo] {
        solutionFile 
        |> DotNet.build (fun p -> 
            { p with
                Configuration = buildConfiguration })
    }

    // Copies binaries from default VS location to expected bin folder
    // But keeps a subdirectory structure for each project in the
    // src folder to support multiple project outputs
    //Target.create "CopyBinaries" (fun _ ->
    let copyBinaries = BuildTask.create "CopyBinaries" [build] {
        !! "src/**/*.??proj"
        -- "src/**/*.shproj"
        |>  Seq.map (fun f -> ((System.IO.Path.GetDirectoryName f) </> "bin" </> configuration, "bin" </> (System.IO.Path.GetFileNameWithoutExtension f)))
        |>  Seq.iter (fun (fromDir, toDir) -> Shell.copyDir toDir fromDir (fun _ -> true))
    }


    // --------------------------------------------------------------------------------------
    // Run the unit tests using test runner

    let tests() = 
        !! testAssemblies
        |> Seq.filter (fun x -> x.ToLower().Contains("benchmark") |> not)
        |> Expecto.run id

    let runTests = BuildTask.create "RunTests" [copyBinaries] {
        tests()
    }

    let runTestsOnly = BuildTask.create "RunTestsOnly" [] {
        "tests/LambdaCalc.Tests/LambdaCalc.Tests.fsproj"
        |> DotNet.build (fun p -> 
            { p with
                Configuration = buildConfiguration })
        tests()
    }

    let runBenchmarks = BuildTask.create "RunBenchmarks" [copyBinaries] {
        !! testAssemblies
        |> Seq.filter (fun x -> x.ToLower().Contains("benchmark") )
        |> Expecto.run id
    }

    let runBenchmarksOnly = BuildTask.create "RunBenchmarksOnly" [] {
        !! testAssemblies
        |> Seq.filter (fun x -> x.ToLower().Contains("benchmark") )
        |> Expecto.run id
    }

    // --------------------------------------------------------------------------------------
    // Build a NuGet package

    let nuGet = BuildTask.create "NuGet" [copyBinaries] {
        let release = releaseNotes.Notes |> String.toLines

        Paket.pack(fun p -> 
            { p with
                OutputPath = "bin"
                Version = releaseNotes.NugetVersion
                ReleaseNotes = release})
    }

    let publishNuget = BuildTask.create "PublishNuget" [] {
        Paket.push(fun p ->
            { p with
                PublishUrl = "https://www.nuget.org"
                WorkingDir = "bin" })

    }

    // --------------------------------------------------------------------------------------
    // Generate the documentation

    // Paths with template/source/output locations
    let bin        = __SOURCE_DIRECTORY__ @@ "..\\..\\bin"
    let content    = __SOURCE_DIRECTORY__ @@ "..\\..\\docsrc\\content"
    let output     = __SOURCE_DIRECTORY__ @@ "..\\..\\docs"
    let files      = __SOURCE_DIRECTORY__ @@ "..\\..\\docsrc\\files"
    let templates  = __SOURCE_DIRECTORY__ @@ "..\\..\\docsrc\\tools\\templates"
    let formatting = __SOURCE_DIRECTORY__ @@ "..\\..\\packages\\formatting\\FSharp.Formatting"
    let docTemplate = "docpage.cshtml"

    let github_release_user = Environment.environVarOrDefault "github_release_user" gitOwner
    let githubLink = sprintf "https://github.com/%s/%s" github_release_user gitName

    let info =
        [ 
            "project-name", project
            "project-author", authors
            "project-summary", summary
            "project-github", githubLink
            "project-nuget", sprintf "http://nuget.org/packages%s" website 
        ]

    let root = website

    let referenceBinaries = []

    let layoutRootsAll = new System.Collections.Generic.Dictionary<string, string list>()
    layoutRootsAll.Add("en",[   templates; 
                                formatting @@ "templates"
                                formatting @@ "templates/reference" ])

    let copyFiles () =
        Shell.copyRecursive files output true 
        |> Trace.logItems "Copying file: "
        Directory.ensure (output @@ "content")
        Shell.copyRecursive (formatting @@ "styles") (output @@ "content") true 
        |> Trace.logItems "Copying styles and scripts: "
        
    let replace t r (lines:seq<string>) =
        seq {
            for s in lines do
                if s.Contains(t) then 
                    yield s.Replace(t, r)
                else yield s }

    let postProcessDocs () =
        let dirInfo = DirectoryInfo.ofPath output

        let filePath = System.IO.Path.Combine(dirInfo.FullName, "operationalSemantics.html")
        let newContent =
            File.ReadAllLines filePath
            |> Array.toSeq
            |> replace "t1X2B62Xt1" "t<sub>1</sub> → t<sub>1</sub>"
            |> replace "t1Xt2X2B62Xt1xXt2" "t<sub>1</sub> t<sub>2</sub> → t<sub>1</sub>&#39; t<sub>2</sub>"
            |> replace "t2X2B62Xt2" "t<sub>2</sub> → t<sub>2</sub>"
            |> replace "v1Xt2X2B62Xv1Xt2x" "v<sub>1</sub> t<sub>2</sub> → v<sub>1</sub> t<sub>2</sub>&#39;"
            |> replace "Yt12Y" "t<sub>12</sub>"
            |> replace "Xv2X2B62" " v<sub>2</sub> →"
            |> replace "Yv2Y" "v<sub>2</sub>"
            |> replace @"22A6" "⊢"
            |> replace @"21B6" "↦"
        File.WriteAllLines(filePath, newContent)

        let filePath = System.IO.Path.Combine(dirInfo.FullName, "index.html")
        let newContent =
            File.ReadAllLines filePath
            |> Array.toSeq
            |> replace "<h2>global Namespace</h2>" ""
        File.WriteAllLines(filePath, newContent)

    let postProcessReferenceDocs () =
        let dirInfo = DirectoryInfo.ofPath <| sprintf "%s/%s" output "reference"

        let filePath = System.IO.Path.Combine(dirInfo.FullName, "index.html")
        let newContent =
            File.ReadAllLines filePath
            |> Array.toSeq
            |> replace "<h2>global Namespace</h2>" ""
        File.WriteAllLines(filePath, newContent)

    let docsOnly = BuildTask.create "DocsOnly" [cleanDocs] {
        File.delete "docsrc/content/release-notes.md"
        Shell.copyFile "docsrc/content/" "RELEASE_NOTES.md"
        Shell.rename "docsrc/content/release-notes.md" "docsrc/content/RELEASE_NOTES.md"

        File.delete "docsrc/content/license.md"
        Shell.copyFile "docsrc/content/" "LICENSE.txt"
        Shell.rename "docsrc/content/license.md" "docsrc/content/LICENSE.txt"

        DirectoryInfo.getSubDirectories (DirectoryInfo.ofPath templates)
        |> Seq.iter (fun d ->
                        let name = d.Name
                        if name.Length = 2 || name.Length = 3 then
                            layoutRootsAll.Add(
                                    name, [ templates @@ name
                                            formatting @@ "templates"
                                            formatting @@ "templates/reference" ]))
        copyFiles ()
    
        for dir in  [ content; ] do
            let langSpecificPath(lang, path:string) =
                path.Split([|'/'; '\\'|], System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.exists(fun i -> i = lang)
            let layoutRoots =
                let key = layoutRootsAll.Keys |> Seq.tryFind (fun i -> langSpecificPath(i, dir))
                match key with
                | Some lang -> layoutRootsAll.[lang]
                | None -> layoutRootsAll.["en"] // "en" is the default language

            FSFormatting.createDocs (fun args ->
                { args with
                    Source = content
                    OutputDirectory = output 
                    LayoutRoots = layoutRoots
                    ProjectParameters = ("root", root)::info
                    Template = docTemplate } )

        postProcessDocs()
    }

    let docs = BuildTask.createEmpty "Docs" [copyBinaries; docsOnly]

    let referenceDocs = BuildTask.create "ReferenceDocs" [cleanDocs; copyBinaries] {
        Directory.ensure (output @@ "reference")
        
        let binaries () =
            let manuallyAdded = 
                referenceBinaries 
                |> List.map (fun b -> bin @@ b)
           
            let conventionBased = 
                DirectoryInfo.getSubDirectories <| DirectoryInfo bin
                |> Array.filter (fun d -> 
                    d.Name = "LambdaCalc"
                    || d.Name = "Untyped"
                    || d.Name = "UntypedRecurs"
                )
                |> Array.collect (fun d ->
                    printfn "%s" d.FullName
                    let name, dInfo = 
                            d.Name, 
                                (DirectoryInfo.getSubDirectories d 
                                 |> Array.filter(fun x -> 
                                    x.FullName.ToLower().Contains("netcoreapp2.1")
                                    || x.FullName.ToLower().Contains("netstandard2.0")
                                 )
                                ).[0]
                    dInfo.GetFiles()
                    |> Array.filter (fun x -> 
                        x.Name.ToLower() = (sprintf "%s.dll" name).ToLower())
                    |> Array.map (fun x -> x.FullName) 
                    )
                |> List.ofArray
        
            conventionBased @ manuallyAdded
        
        binaries()
        |> FSFormatting.createDocsForDlls (fun args ->
            { args with
                OutputDirectory = output @@ "reference"
                LayoutRoots =  layoutRootsAll.["en"]
                ProjectParameters = ("root", root)::info
                SourceRepository = githubLink @@ "tree/master" }
                   )

        postProcessReferenceDocs ()
    }

    let generateDocs = BuildTask.createEmpty "GenerateDocs" [docs; referenceDocs] 

    let release = BuildTask.create "Release" [] {
        Git.Staging.stageAll ""
        Git.Commit.exec "" (sprintf "Bump version to %s" releaseNotes.NugetVersion)
        Git.Branches.push ""

        Git.Branches.tag "" releaseNotes.NugetVersion
        Git.Branches.pushTag "" "origin" releaseNotes.NugetVersion
    }

    BuildTask.createEmpty "All" [runTests; runBenchmarks; generateDocs; nuGet]

let listAvailable() = BuildTask.listAvailable()
