module BinanceApi
open System
open System.Text
open System.Security.Cryptography
open HttpFs.Client
open System.Net
open FSharp.Json
open Hopac

type BinanceApiKey = BinanceApiKey of string
type BinanceApiSecret = BinanceApiSecret of string
type BinanceApiRoot = BinanceApiRoot of string

type BinanceApiContext = {
    ApiKey: BinanceApiKey;
    ApiSecret: BinanceApiSecret;
    ApiRoot: BinanceApiRoot;
}

type ApiResponseError = 
    | Unknown = -1000
    | Disconnected = -1001
    | Unauthorized = -1002
    | TooManyRequests = -1003
    | UnexpectedResp = -1006
    | Timeout = -1007
    | InvalidMessage = -1013
    | UnknownOrderComposition = -1014
    | TooManyOrders = -1015
    | ServiceShuttingDown = -1016
    | UnsupportedOperation = -1020
    | InvalidTimestamp = -1021
    | InvalidSignature = -1022
    | IllegalChars = -1100
    | TooManyParameters = -1101
    | MandatoryParamEmptyOrMalformed = -1102
    | UnknownParam = -1103
    | UnreadParameters = -1004
    | ParamEmpty = -1105
    | ParamNotRequired = -1006
    | NoDepth = -1112
    | TifNotRequired = -1114
    | InvaliddTif = -1115
    | InvalidOrderType = -1116
    | InvalidSide = -1117
    | EmptyNewClOrdId = -1118
    | EmptyOrgClOrdId = -1119
    | BadInterval = -1120
    | BadSymbol = -1121
    | InvalidListenKey = -1125
    | MoreThanXxHours = -1127
    | OptionalParamsBadCombo = -1128
    | InvalidParameter = -1130
    | BadApiId = -2008
    | DuplicateApiKeyDesc = -2009
    | CancelAllFail = -2012
    | NoSuchOrder = -2013
    | BadApiKeyFmt = -2014
    | RejectedMbxKey = -2015

type ApiResponseErrorMsg = ApiResponseErrorMsg of string

type ApiResponseFailure = { code : int ; msg : string }

type ApiResult<'a> = 
    | Success of 'a
    | Failure of ApiResponseFailure

type SymbolStatus =
    | PRE_TRADING
    | TRADING
    | POST_TRADING
    | END_OF_DAY
    | HALT
    | AUCTION_MATCH
    | BREAK
    | UNKNOWN

let tradingStatusFromDTO (statusDTO: string) =
    match statusDTO with
    | "PRE_TRADING" -> PRE_TRADING
    | "TRADING" -> TRADING
    | "POST_TRADING" -> POST_TRADING
    | "END_OF_DAY" -> END_OF_DAY
    | "HALT" -> HALT
    | "AUCTION_MATCH" -> AUCTION_MATCH
    | "BREAK" -> BREAK
    | _ -> UNKNOWN

type OrderStatus =
    | NEW
    | PARTIALLY_FILLED
    | FILLED
    | CANCELLED
    | PENDING_CANCEL
    | REJECTED
    | EXPIRED

type OCOStatus =
    | RESPONSE
    | EXEC_STARTED
    | ALL_DONE

type OCOOrderStatus =
    | EXECUTING
    | ALL_DONE
    | REJECT

type ContingencyType =
    | OCO

type OrderType =
    | LIMIT
    | MARKET
    | STOP_LOSS
    | STOP_LOSS_LIMIT
    | TAKE_PROFIT
    | TAKE_PROFIT_LIMIT
    | LIMIT_MAKER

type OrderResponse =
    | ACK
    | RESULT
    | FULL

type OrderSide =
    | BUY
    | SELL

type TimeInForce =
    | GTC
    | IOC
    | FOK

type KlineInterval =
    | MINUTE_1
    | MINUTE_3
    | MINUTE_5
    | MINUTE_15
    | MINUTE_30
    | HOUR_1
    | HOUR_2
    | HOUR_4
    | HOUR_6
    | HOUR_8
    | HOUR_12
    | DAY_1
    | DAY_3
    | WEEK_1
    | MONTH_1  
    member this.value =
        match this with
        | MINUTE_1 -> "1m"
        | MINUTE_3 -> "3m"
        | MINUTE_5 -> "5m"
        | MINUTE_15 -> "15m"
        | MINUTE_30 -> "30m"
        | HOUR_1 -> "1h"
        | HOUR_2 -> "2h"
        | HOUR_4 -> "4h"
        | HOUR_6 -> "6h"
        | HOUR_8 -> "8h"
        | HOUR_12 -> "12h"
        | DAY_1 -> "1d"
        | DAY_3 -> "3d"
        | WEEK_1 -> "1w"
        | MONTH_1 -> "1M"

type RateLimitType =
    | REQUEST_WEIGHT
    | ORDERS
    | RAW_REQUESTS

type RateLimitInterval =
    | SECOND
    | MINUTE
    | DAY

[<Measure>] type RateLimitIntervalNum
[<Measure>] type RateLimitLimit

type RequestWeight = {
    rateLimitType: RateLimitType
    interval: RateLimitInterval
    intervalNum: int<RateLimitIntervalNum>
    limit: int<RateLimitLimit>
}

type ExchangeInfoSymbolFilterDTO = {
    filterType: string
    minPrice: string option
    maxPrice: string option
    tickSize: string option
    multiplierUp: string option
    multiplierDown: string option
    avgPriceMins: int option
}

type ExchangeInfoSymbolDTO = {
    symbol: string
    status: string
    baseAsset: string
    baseAssetPrecision: int
    quoteAsset: string
    quoteAssetPrecision: int
    baseCommissionPrecision: int
    quoteCommissionPrecision: int
    orderTypes: string list
    icebergAllowed: bool
    ocoAllowed: bool
    quoteOrderQtyMarketAllowed: bool
    isSpotTradingAllowed: bool
    isMarginTradingAllowed: bool
    filters: ExchangeInfoSymbolFilterDTO list
    permissions: string list
}

type ExchangeInfoRateLimitsDTO = {
    rateLimitType: string
    interval: string
    intervalNum: int
    limit: int
}

type ExchangeInfoDTO = {
    timezone: string
    serverTime: int64
    rateLimits: ExchangeInfoRateLimitsDTO list
    symbols: ExchangeInfoSymbolDTO list
}
type TickerPriceDTO = {
    symbol: string
    price: string
}

type AccountType = 
| MARGIN = 1
| SPOT = 2

type BalanceDTO = {
    asset: string
    free: string
    locked: string
}

type AccountInformationDTO = {
    makerCommission: int
    takerCommission: int
    buyerCommission: int
    sellerCommission: int
    canTrade: bool
    canWithdraw: bool
    canDeposit: bool
    updateTime: int64
    accountType: AccountType
    balances: BalanceDTO list
    permissions: string list option
}

type TickerPricesDTO = TickerPriceDTO list


type KlinesRequestWindow = {
    startTime: int64 option
    endTime: int64 option
}

type KlineEntryDTO = 
    ( int64  // opening time
    * string // opening rate
    * string // highest rate
    * string // lowest rate
    * string // closing rate
    * string // volume
    * int64  // closing time
    * string // quote asset volume
    * int    // number of trades
    * string // taker buy base asset volume
    * string // taker buy quote asset volume
    * string // IGNORE
    )


type BinanceApiEndpoint = BinanceApiEndpoint of string


let ExchangeInfoEndpoint = BinanceApiEndpoint "api/v1/exchangeInfo"
let TickerPriceEndpoint = BinanceApiEndpoint "api/v3/ticker/price"
let KlinesEndpoint = BinanceApiEndpoint "api/v3/klines"
let AccountInformationEndpoint = BinanceApiEndpoint "api/v3/account"


let unixTime () = DateTimeOffset(DateTime.Now).ToUnixTimeMilliseconds()


let responseToCodeBody (response : Response) : int * string =
    use ms = new IO.MemoryStream()
    response.body.CopyTo(ms)
    let responseBody = ms.ToArray() |> Encoding.ASCII.GetString
    (response.statusCode, responseBody)


let handleResponse (successFunc: (string -> 'a)) (response: Response) =
    let (code, body) = responseToCodeBody response
    match enum code with
    | HttpStatusCode.OK -> Success <| successFunc body
    | _ ->                 Failure <| Json.deserialize<ApiResponseFailure> body


let inspect x =
    printfn "%A" x
    x

let makeBinanceRequest (apiKey: BinanceApiKey) (req: Request): Response =
    let (BinanceApiKey uApiKey) = apiKey
    req
    |> Request.setHeader (Custom ("X-MBX-APIKEY", uApiKey))
    |> getResponse
    |> run


let bytesToHexString (bytes : byte[]) : string =
    bytes
    |> Seq.map (fun c -> c.ToString("X2"))
    |> Seq.reduce (+)


let signRequest (apiSecret: BinanceApiSecret) (queryString: string) =
    let (BinanceApiSecret uApiSecret) = apiSecret
    let secretBytes = Encoding.ASCII.GetBytes uApiSecret
    let secretQuery = Encoding.ASCII.GetBytes queryString
    use hmac = new HMACSHA256(secretBytes)
    hmac.ComputeHash(secretQuery)
    |> bytesToHexString


let makeBinanceUrl (apiRoot: BinanceApiRoot) (endpoint: BinanceApiEndpoint) (queryString: string) =
    let (BinanceApiRoot uApiRoot) = apiRoot
    let (BinanceApiEndpoint uEndpoint) = endpoint
    $"{uApiRoot}/{uEndpoint}?{queryString}"


let makeBinanceUrlNoQS (apiRoot: BinanceApiRoot) (endpoint: BinanceApiEndpoint) =
    let (BinanceApiRoot uApiRoot) = apiRoot
    let (BinanceApiEndpoint uEndpoint) = endpoint
    $"{uApiRoot}/{uEndpoint}"


let accountInformationRequest (context: BinanceApiContext) =
    let now = string (unixTime ())
    let signature = signRequest context.ApiSecret $"timestamp={now}"
    let queryString = $"timestamp={now}&signature={signature}"
    Request.createUrl Get <| makeBinanceUrl context.ApiRoot AccountInformationEndpoint queryString
    |> makeBinanceRequest context.ApiKey
    |> handleResponse Json.deserialize<AccountInformationDTO>


let exchangeInfoRequest (context: BinanceApiContext) =
    Request.createUrl Get <| makeBinanceUrl context.ApiRoot ExchangeInfoEndpoint ""
    |> makeBinanceRequest context.ApiKey
    |> handleResponse Json.deserialize<ExchangeInfoDTO>


let tickerRequest (context: BinanceApiContext) =
    Request.createUrl Get <| makeBinanceUrl context.ApiRoot TickerPriceEndpoint ""
    |> makeBinanceRequest context.ApiKey
    |> handleResponse Json.deserialize<TickerPricesDTO>


let addOptionalLimitToRequest (request: Request) (requestLimit: int option) = 
    match requestLimit with
    | Some limit -> request |> Request.queryStringItem "limit" (string limit)
    | None -> request


let addOptionalWindowToRequest (request: Request) (requestWindow: KlinesRequestWindow option): Request =
    match requestWindow with
    | Some window ->
        request
        |> Request.queryStringItem "startTime" (string window.startTime)
        |> Request.queryStringItem "endTime" (string window.endTime)
    | None -> request


let klinesRequest (context: BinanceApiContext) (symbol: string) (interval: KlineInterval) (requestWindow: KlinesRequestWindow option) (limit: int option) =
    Request.createUrl Get (makeBinanceUrlNoQS context.ApiRoot KlinesEndpoint)
    |> Request.queryStringItem "symbol" symbol
    |> Request.queryStringItem "interval" interval.value
    |> makeBinanceRequest context.ApiKey
    |> handleResponse Json.deserialize<array<KlineEntryDTO>>

