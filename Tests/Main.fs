module BinanceFinanceTests

open Expecto
open Expecto.Flip
open DomainTypes
open AssetBalances
open AdjacencyMap


let assestBalancesTests =
    testList "AssetBalances Tests" [
        test "Total of 0 balances is 0" {
            let balance = { asset = Asset "BTC" ; free = 0.0m<equity> ; locked = 0.0m<equity> }
            totalOfBalance balance 
            |> Expect.equal "0 free and 0 locked should be 0 total" 0.0m<equity>
        }
    ]

let adjacencyMapTests =
    testList "AdjacencyMap Tests" [
        let BTCETH = Symbol "BTCETH"
        let BTCOTH = Symbol "BTCOTH"
        let ETHOTH = Symbol "ETHOTH"
        let BTC = Base "BTC"
        let ETH = Base "ETH"
        let OTH = Quote "OTH"

        test "Minimal CurrencyPair list" {
            let currencyPairs = [
                { symbol = BTCETH
                ; baseAsset = BTC
                ; quoteAsset = ETH
                ; rate = 2.0m<rate> }
            ]
            let expectedAdjacencyMap =
                Map.ofList [
                    BTC, Map.ofList [ETH, (BTCETH, 2.0m<rate>)];
                    ETH, Map.ofList [BTC, (BTCETH, 0.5m<rate>)]
                ]
            createAdjacencyMap currencyPairs
            |> Expect.equal "map should match expected" expectedAdjacencyMap
        }

        let currencyPairsThroughOTH = [
            { symbol = BTCOTH
            ; baseAsset = BTC
            ; quoteAsset = OTH
            ; rate = 2.0m<rate> };
            { symbol = ETHOTH
            ; baseAsset = ETH
            ; quoteAsset = OTH
            ; rate = 4.0m<rate> }
        ]
        test "Unlinked CurrencyPair list" {
            let expectedAdjacencyMap =
                Map.ofList [
                    BTC, Map.ofList [OTH, (BTCOTH, 2.0m<rate>)];
                    OTH, Map.ofList [
                        BTC, (BTCOTH, 0.5m<rate>) ;
                        ETH, (ETHOTH, 0.25m<rate>)
                    ];
                    ETH, Map.ofList [OTH, (ETHOTH, 4.0m<rate>)]
                ]
            createAdjacencyMap currencyPairsThroughOTH
            |> Expect.equal "map should match expected" expectedAdjacencyMap
        }

        test "Find ETH exchange rate with BTC through OTH" {
            ()
        }
    ]

[<EntryPoint>]
let main argv =
    runTestsWithCLIArgs [] [||] adjacencyMapTests
