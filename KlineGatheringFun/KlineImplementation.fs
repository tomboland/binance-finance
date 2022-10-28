module KlineImplementation

open System
open DomainTypes
open BinanceApi
open CandlestickClassifier
open HttpFs.Logging

let dtFromEpochMs (epochMs: int64): DateTime =
    DateTimeOffset.FromUnixTimeMilliseconds(epochMs).UtcDateTime

let klineEntryFromDTO (k: KlineEntryDTO): KlineEntry =
    let ( openTime, openPrice, highPrice, lowPrice, closePrice
        , volume, closeTime, quoteAssetVolume, numberOfTrades
        , takerBuyBaseAssetVolume, takerBuyQuoteAssetVolume , _) = k
    { openTime = dtFromEpochMs openTime
    ; openRate = (decimal openPrice) * 1m<rate>
    ; closeRate = (decimal closePrice) * 1m<rate>
    ; lowRate = (decimal lowPrice) * 1m<rate>
    ; highRate = (decimal highPrice) * 1m<rate>
    ; volume = (decimal volume) * 1m<volume>
    ; closeTime = dtFromEpochMs closeTime
    ; quoteAssetVolume = (decimal quoteAssetVolume) * 1m<volume>
    ; numberOfTrades = numberOfTrades
    ; takerBuyBaseAssetVolume = (decimal takerBuyBaseAssetVolume) * 1m<volume>
    ; takerBuyQuoteAssetVolume = (decimal takerBuyQuoteAssetVolume) * 1m<volume>
    }

let candlestickFromKlineEntry (k: KlineEntry): Candlestick =
    { openTime = k.openTime
    ; openPrice = k.openRate
    ; closePrice = k.closeRate
    ; lowPrice = k.lowRate
    ; highPrice = k.highRate }

let classifyCandlestickPatterns (cs: Candlestick array) =
    for i in 0 .. cs.Length - 1 do
        match (cs, i) with
        | MarubozuBlack -> printfn "MarubozuBlack: %A" i
        | MarubozuClosingBlack -> printfn "MarubozuCLosingBlack: %A" i
        | MarubozuWhite -> printfn "MarubozuWhite: %A" i
        | MarubozuOpeningWhite -> printfn "MarubozuOpeningWhite: %A" i
        | ShootingStarOneCandle -> printfn "ShootingStarOneCandle: %A" i
        | DojiDragonFly -> printfn "DojiDragonFly: %A" i
        | DojiGravestone -> printfn "DojiGravestone: %A" i
        | Hammer -> printfn "Hammer: %A" i
        | HangingMan -> printfn "HangingMan: %A" i
        | BeltHoldBearish -> printfn "BeltHoldBearish: %A" i
        | BeltHoldBullish -> printfn "BeltHoldBullish: %A" i
        | _ ->  ()