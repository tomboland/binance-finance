module CurrencyPair

open DomainTypes
open BinanceApi


type ExchangeSymbol = {
    symbol: Symbol
    status: SymbolStatus
    baseAsset: TradingAsset
    quoteAsset: TradingAsset
}

type Ticker = {
    symbol: Symbol
    rate: decimal<rate>
}


let exchangeSymbolFromDTO (symbol: ExchangeInfoSymbolDTO) =
    { symbol = Symbol symbol.symbol
    ; status = tradingStatusFromDTO symbol.status
    ; baseAsset = Base symbol.baseAsset
    ; quoteAsset = Quote symbol.quoteAsset }

let binanceExchangeSymbols (apiContext: BinanceApiContext) =
    exchangeInfoRequest apiContext
    |> function
    | Success exchangeInfo ->
        exchangeInfo.symbols
        |> List.map (exchangeSymbolFromDTO)
    | Failure err -> failwith $"{err}"

let tickerFromDTO (ticker: TickerPriceDTO) =
    { symbol = Symbol ticker.symbol
    ; rate = (decimal ticker.price) * 1.0m<rate> }

let binanceTickers (apiContext: BinanceApiContext) =
    tickerRequest apiContext
    |> function
    | Success tickers ->
        List.map (tickerFromDTO) tickers
    | Failure err -> failwith $"{err}"

let tickersBySymbol (tickers: Ticker list) =
    tickers
    |> List.map (fun ticker -> ticker.symbol, ticker)
    |> Map.ofList

let exchangeSymbolsBySymbol (exchangeSymbols: ExchangeSymbol list) =
    exchangeSymbols
    |> List.map (fun symbol -> symbol.symbol, symbol)
    |> Map.ofList

let currencyPairsFromExchangeSymbolTicker (exchangeSymbols: ExchangeSymbol list) (tickers: Ticker list) =
    let tickersBySymbol = tickersBySymbol tickers
    exchangeSymbols
    |> List.filter (fun symbol -> tickersBySymbol.ContainsKey symbol.symbol)
    |> List.map (fun symbol ->
        { symbol = symbol.symbol
        ; baseAsset = symbol.baseAsset
        ; quoteAsset = symbol.quoteAsset
        ; rate = tickersBySymbol.[symbol.symbol].rate })

let tradingCurrencyPairs (apiContext: BinanceApiContext) =
    let exchangeSymbols =
        binanceExchangeSymbols apiContext
        |> List.filter (fun symbol -> symbol.status = TRADING)
    let tickers = binanceTickers apiContext
    currencyPairsFromExchangeSymbolTicker exchangeSymbols tickers
