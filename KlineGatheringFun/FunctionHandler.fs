module Function

open Secrets
open BinanceApi
open Npgsql.FSharp
open Npgsql
open FSharp.Json
open TimeSeriesDb
open DomainTypes
open KlineImplementation

let binanceApiKey = getSecret "binance-api-key"
let binanceApiSecret = getSecret "binance-api-secret"
let apiContext = {
    ApiKey = BinanceApiKey binanceApiKey ;
    ApiSecret = BinanceApiSecret binanceApiSecret ;
    ApiRoot = BinanceApiRoot "https://api.binance.com" ;
}
printfn "%A" apiContext

let tsDbConnectionString: string = 
    Sql.host (getSecret "binance-db-host")
    |> Sql.database (getSecret "binance-db-name")
    |> Sql.username (getSecret "binance-db-user")
    |> Sql.password (getSecret "binance-db-password")
    |> Sql.requireSslMode
    |> Sql.trustServerCertificate true
    |> Sql.formatConnectionString

let tsDbConnection = new NpgsqlConnection(tsDbConnectionString)
tsDbConnection.Open()


let Handle (_: string) =
    klinesRequest apiContext "ETHBTC" DAY_1 None None
    |> function
    | Success a ->
        a
        |> Array.map klineEntryFromDTO
        |> Array.map candlestickFromKlineEntry
        |> classifyCandlestickPatterns
    | Failure msg -> failwith $"{msg}"

[<EntryPoint>]
let main argv =
    Handle ""
    0