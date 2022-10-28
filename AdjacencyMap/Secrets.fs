module Secrets
open System
open System.IO

let prefix =
    match Environment.GetEnvironmentVariable("SECRETS_PATH") with
    | s when not (s = null) -> s
    | _ -> "/var/openfaas/secrets/"

let getSecret (secretName: string) =
    let secret =
        IO.Path.Combine (prefix, secretName)
        |> IO.File.ReadAllText
    secret.Trim()