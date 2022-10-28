module Function

open Secrets
open BinanceApi
open AssetBalances
open Npgsql.FSharp
open Npgsql
open FSharp.Json
open TimeSeriesDb
open CurrencyPair
open AdjacencyMap
open DomainTypes

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

let getNonEmptyAccountBalances (apiContext: BinanceApiContext): Balance list =
    binanceAccountBalances apiContext
    |> List.filter (fun balance -> totalOfBalance balance <> 0.0m<equity>)

let Handle (_: string) =
    let klines = klinesRequest apiContext "ETHBTC" MINUTE_1 None None
    printfn "%A" klines

    let adjacencyMap =
        tradingCurrencyPairs apiContext
        |> createAdjacencyMap

    getNonEmptyAccountBalances apiContext
    |> List.map(balanceToDbDTO)
    |> (insertBalancesIntoTsDb tsDbConnection)

[<EntryPoint>]
let main argv =
    Handle ""
    0
