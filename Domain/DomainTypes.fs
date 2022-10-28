module DomainTypes

open System

[<Measure>] type rate
[<Measure>] type volume
[<Measure>] type equity

type Symbol = Symbol of string
type Asset = Asset of string

type Balance = {
    asset: Asset
    free: decimal<equity>
    locked: decimal<equity>
}

type TradingAsset =
    | Base of string
    | Quote of string
    member this.value =
        match this with
        | Base v -> v
        | Quote v -> v


type CurrencyPair = {
    symbol: Symbol
    baseAsset: TradingAsset
    quoteAsset: TradingAsset
    rate: decimal<rate>
}

[<Struct>]
type Candlestick = {
    openTime: DateTime
    openPrice: decimal<rate>
    closePrice: decimal<rate>
    lowPrice: decimal<rate>
    highPrice: decimal<rate>
}

[<Struct>]
type KlineEntry = {
    openTime: DateTime
    openRate: decimal<rate>
    closeRate: decimal<rate>
    lowRate: decimal<rate>
    highRate: decimal<rate>
    volume: decimal<volume>
    closeTime: DateTime
    quoteAssetVolume: decimal<volume>
    numberOfTrades: int
    takerBuyBaseAssetVolume: decimal<volume>
    takerBuyQuoteAssetVolume: decimal<volume>
}