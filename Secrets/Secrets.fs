module Secrets
open System
open System.IO

let prefix =
    match Environment.GetEnvironmentVariable("SECRETS_PATH") with
    | s when not (s = null) -> s
    | _ -> "/var/openfaas/secrets/"

let getSecret (secretName: string) =
    let secret =
        Path.Combine (prefix, secretName)
        |> File.ReadAllText
    secret.Trim()

[<EntryPoint>]
let main argv =
    0