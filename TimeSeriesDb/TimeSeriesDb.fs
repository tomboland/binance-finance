module TimeSeriesDb

open System
open Npgsql
open Npgsql.FSharp
open DomainTypes

type BalanceDbDTO =
    { asset: string
      free: decimal
      locked: decimal }

let balanceToDbDTO (balance: Balance) =
    let (Asset asset) = balance.asset

    { asset = asset
    ; free = balance.free / 1.0m<equity>
    ; locked = balance.locked / 1.0m<equity> }

let balanceInsertionQuery (balance: BalanceDbDTO) (time: DateTime) =
    "insert into binance_balances values (@time, @asset, @free, @locked)",
    [ [ "@time", Sql.timestamptz time
        "@asset", Sql.string balance.asset
        "@free", Sql.decimal balance.free
        "@locked", Sql.decimal balance.locked ] ]

let insertBalancesIntoTsDb (connection: NpgsqlConnection) (balances: BalanceDbDTO list) : unit =
    let now = DateTime.Now
    let queries =
        balances
        |> List.map (fun balance -> (balanceInsertionQuery balance now))
    connection
    |> Sql.existingConnection
    |> Sql.executeTransaction queries
    |> ignore
