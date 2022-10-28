module CandlestickClassifier

open System
open DomainTypes


let openPrice c = c.openPrice
let closePrice c = c.closePrice
let highPrice c = c.highPrice
let lowPrice c = c.lowPrice
let bodyHeight c = c.closePrice - c.openPrice
let bodyTop c = max c.openPrice c.closePrice
let bodyBottom c = min c.openPrice c.closePrice
let upperShadowHeight c = c.highPrice - bodyTop c
let lowerShadowHeight c = bodyBottom c - c.lowPrice
let shadowHeight c = upperShadowHeight c - lowerShadowHeight c

type CandlestickPattern = {
    significant: Candlestick array
    trend: Candlestick array
}

let getCandlestickPattern (numTrend: int) (numSignificant: int) (timeSeries: Candlestick array) (index: int) =
    let significantStart = index + numTrend 
    if index + numTrend + numSignificant < timeSeries.Length then
        Some {
            significant = timeSeries.[significantStart..(significantStart + numSignificant)]
            ; trend = timeSeries.[index..(significantStart - 1)] 
        }
    else
        None

let averagePrice (cs: Candlestick array) =
    (cs |> Array.map closePrice |> Array.sum) / (decimal <| Array.length cs)


let upwardTrend cs =
    cs
    |> Array.windowed 5
    |> Array.pairwise
    |> Array.forall (fun (c1, c2) -> averagePrice c1 > averagePrice c2)

let (|UpwardTrend|_|) (cs: Candlestick array) =
    if upwardTrend cs then Some UpwardTrend else None

let downwardTrend cs =
    cs
    |> Array.windowed 5
    |> Array.pairwise
    |> Array.forall (fun (c1, c2) -> averagePrice c1 < averagePrice c2)

let (|DownwardTrend|_|) (cs: Candlestick array) =
    if downwardTrend cs then Some DownwardTrend else None

let percentageGreater (x: decimal<rate>) (y: decimal<rate>) =
    (abs (x - y) / y) * 100m

let percentageLess (x: decimal<rate>) (y: decimal<rate>) =
    (abs (y - x) / x) * 100m

let percentageDifference (x: decimal<rate>) (y: decimal<rate>) =
    (abs (x - y) / max x y) * 100m

let percentageUp (x: decimal<rate>) (y: decimal<rate>) =
    (abs (y - x) / y) * 100m

let percentageDown (x: decimal<rate>) (y: decimal<rate>) = 
    (abs (y - x) / y) * 100m

let slightlyGreater (x: decimal<rate>) (y: decimal<rate>) =
    0.3m <= percentageGreater x y && percentageGreater x y < 1m

let moderatelyGreater (x: decimal<rate>) (y: decimal<rate>) =
    1m <= percentageGreater x y && percentageGreater x y < 2.5m

let largelyGreater (x: decimal<rate>) (y: decimal<rate>) =
    2.5m <= percentageGreater x y && percentageGreater x y < 5m

let extremelyGreater (x: decimal<rate>) (y: decimal<rate>) =
    percentageGreater x y >= 5m

let slightlyLess (x: decimal<rate>) (y: decimal<rate>) =
    0.3m <= percentageLess x y && percentageLess x y < 1m

let moderatelyLess (x: decimal<rate>) (y: decimal<rate>) =
    1m <= percentageLess x y && percentageLess x y < 2.5m

let largelyLess (x: decimal<rate>) (y: decimal<rate>) =
    2.5m <= percentageLess x y && percentageLess x y < 5m

let extremelyLess (x: decimal<rate>) (y: decimal<rate>) =
    percentageLess x y >= 5m

let extremelyNear (x: decimal<rate>) (y: decimal<rate>) =
    percentageDifference x y <= 0.3m

let moderatelyNear (x: decimal<rate>) (y: decimal<rate>) =
    0.3m <= percentageDifference x y && percentageDifference x y < 1m

let near (x: decimal<rate>) (y: decimal<rate>) =
    0m <= percentageDifference x y && percentageDifference x y < 1m

let nearUp (x: decimal<rate>) (y: decimal<rate>) =
    0.1m <= percentageUp x y && percentageUp x y < 0.3m

let nearDown (x: decimal<rate>) (y: decimal<rate>) =
    0.1m <= percentageDown x y && percentageDown x y < 0.3m

let doji c =
    extremelyNear c.openPrice c.closePrice

let (|Doji|_|) c =
    if doji c then Some Doji else None

let smallBody c =
    slightlyLess (bodyBottom c) (bodyTop c)

let (|SmallBody|_|) c =
    if smallBody c then Some SmallBody else None

let normalBody c =
    moderatelyLess (bodyBottom c) (bodyTop c)

let (|NormalBody|_|) c =
    if normalBody c then Some NormalBody else None

let longBody c =
    largelyLess (bodyBottom c) (bodyTop c)

let (|LongBody|_|) c =
    if longBody c then Some LongBody else None

let extremelyLongBody c =
    extremelyLess (bodyBottom c) (bodyTop c)

let (|ExtremelyLongBody|_|) c =
    if extremelyLongBody c then Some ExtremelyLongBody else None

let noUpperShadow c =
    extremelyNear c.highPrice (bodyTop c)

let (|NoUpperShadow|_|) c =
    if noUpperShadow c then Some NoUpperShadow else None

let smallUpperShadow c =
    slightlyGreater c.highPrice (bodyTop c)

let (|SmallUpperShadow|_|) c =
    if smallUpperShadow c then Some SmallUpperShadow else None

let normalUpperShadow c =
    moderatelyGreater c.highPrice (bodyTop c)

let (|NormalUpperShadow|_|) c =
    if normalUpperShadow c then Some NormalUpperShadow else None
    
let longUpperShadow c =
    largelyGreater c.highPrice (bodyTop c)
    
let (|LongUpperShadow|_|) c =
    if largelyGreater c.highPrice (bodyTop c) then Some LongUpperShadow else None

let extremelyLongUpperShadow c =
    extremelyGreater c.highPrice (bodyTop c)

let (|ExtremelyLongUpperShadow|_|) c =
    if extremelyLongUpperShadow c then Some ExtremelyLongUpperShadow else None

let noLowerShadow c =
    extremelyNear c.lowPrice (bodyBottom c)

let (|NoLowerShadow|_|) c =
    if noLowerShadow c then Some NoLowerShadow else None

let smallLowerShadow c =
    slightlyLess c.lowPrice (bodyBottom c)

let (|SmallLowerShadow|_|) c =
    if smallLowerShadow c then Some SmallLowerShadow else None

let normalLowerShadow c =
    moderatelyLess c.lowPrice (bodyBottom c)

let (|NormalLowerShadow|_|) c =
    if normalLowerShadow c then Some NormalLowerShadow else None

let longLowerShadow c =
    largelyLess c.lowPrice (bodyBottom c)

let (|LongLowerShadow|_|) c =
    if longLowerShadow c then Some LongLowerShadow else None

let extremelyLongLowerShadow c =
    extremelyLess c.lowPrice (bodyBottom c)

let (|ExtremelyLongLowerShadow|_|) c =
    if extremelyLongLowerShadow c then Some ExtremelyLongLowerShadow else None

let blackBody c =
    c.openPrice > c.closePrice

let (|BlackBody|_|) c =
    if blackBody c then Some BlackBody else None

let whiteBody c =
    c.openPrice < c.closePrice

let (|WhiteBody|_|) c =
    if whiteBody c then Some WhiteBody else None

let smallBlackBody c =
    smallBody c && blackBody c

let (|SmallBlackBody|_|) c =
    if smallBlackBody c then Some SmallBlackBody else None

let smallWhiteBody c =
    smallBody c && whiteBody c

let (|SmallWhiteBody|_|) c =
    if smallWhiteBody c then Some SmallWhiteBody else None

let normalBlackBody c =
    normalBody c && blackBody c

let (|NormalBlackBody|_|) c =
    if normalBlackBody c then Some NormalBlackBody else None

let normalWhiteBody c =
    normalBody c && whiteBody c

let (|NormalWhiteBody|_|) c =
    if normalWhiteBody c then Some NormalWhiteBody else None

let longBlackBody c =
    longBody c && blackBody c

let (|LongBlackBody|_|) c =
    if longBlackBody c then Some LongBlackBody else None

let longWhiteBody c =
    longBody c && whiteBody c

let (|LongWhiteBody|_|) c =
    if longWhiteBody c then Some LongWhiteBody else None

let extremelyLongBlackBody c =
    extremelyLongBody c && blackBody c

let (|ExtremelyLongBlackBody|_|) c =
    if extremelyLongBlackBody c then Some ExtremelyLongBlackBody else None

let extremelyLongWhiteBody c =
    extremelyLongBody c && whiteBody c

let (|ExtremelyLongWhiteBody|_|) c =
    if extremelyLongWhiteBody c then Some ExtremelyLongWhiteBody else None

let downShadowGap (c1, c2) =
    c1.lowPrice > c2.highPrice

let (|DownShadowGap|_|) (c1, c2) =
    if downShadowGap (c1, c2) then Some DownShadowGap else None

let upShadowGap (c1, c2) =
    c1.highPrice < c2.lowPrice

let (|UpShadowGap|_|) (c1, c2) =
    if upShadowGap (c1, c2) then Some UpShadowGap else None

let downBodyGap (c1, c2) =
    bodyBottom c1 > bodyTop c2

let (|DownBodyGap|_|) (c1, c2) =
    if downBodyGap (c1, c2) then Some DownBodyGap else None

let upBodyGap (c1, c2) =
    bodyTop c1 < bodyBottom c2

let (|UpBodyGap|_|) (c1, c2) =
    if upBodyGap (c1, c2) then Some UpBodyGap else None

let marubozuBlack cs index =
    match getCandlestickPattern 0 1 cs index with
    | Some { significant = s } ->
        noUpperShadow s.[0] && longBlackBody s.[0] && noLowerShadow s.[0]
    | None -> false

let (|MarubozuBlack|_|) (cs, index) =
    if marubozuBlack cs index then Some MarubozuBlack else None

let marubozuWhite cs index =
    match getCandlestickPattern 0 1 cs index with
    | Some { significant = s } ->
        noUpperShadow s.[0] && longWhiteBody s.[0] && noLowerShadow s.[0]
    | None -> false

let (|MarubozuWhite|_|) (cs, index) =
    if marubozuWhite cs index then Some MarubozuWhite else None

let beltHoldBullish cs index =
    match getCandlestickPattern 8 1 cs index with
    | Some { trend = t ; significant = s } ->
        downwardTrend t && longWhiteBody s.[0] && noLowerShadow s.[0]
        && moderatelyNear s.[0].closePrice s.[0].highPrice
    | None -> false

let (|BeltHoldBullish|_|) (cs, index) =
    if beltHoldBullish cs index then Some BeltHoldBullish else None

let marubozuClosingBlack cs index =
    match getCandlestickPattern 0 1 cs index with
    | Some { significant = s } -> 
        longBlackBody s.[0] && not <| noUpperShadow s.[0] && noLowerShadow s.[0]
    | None -> false

let (|MarubozuClosingBlack|_|) (cs, index) =
    if marubozuClosingBlack cs index then Some MarubozuClosingBlack else None

let marubozuOpeningWhite cs index =
    match getCandlestickPattern 0 1 cs index with
    | Some { significant = s } -> 
        longWhiteBody s.[0] && not <| noUpperShadow s.[0] && noLowerShadow s.[0]
    | None -> false 

let (|MarubozuOpeningWhite|_|) (cs, index) =
    if marubozuOpeningWhite cs index then Some MarubozuOpeningWhite else None

let shootingStarOneCandle cs index =
    match getCandlestickPattern 0 1 cs index with
    | Some { trend = t ; significant = s } ->
        upwardTrend t && longUpperShadow s.[0]
        && upperShadowHeight s.[0] > 2m * bodyHeight s.[0]
        && smallBody s.[0] && noLowerShadow s.[0]
    | None -> false

let (|ShootingStarOneCandle|_|) (cs, index) =
    if shootingStarOneCandle cs index then Some ShootingStarOneCandle else None

let dojiGravestone cs index =
    match getCandlestickPattern 0 1 cs index with
    | Some { significant = s} ->
        doji s.[0] && noLowerShadow s.[0] && longUpperShadow s.[0]
    | None -> false

let (|DojiGravestone|_|) (cs, index) =
    if dojiGravestone cs index then Some DojiGravestone else None

let beltHoldBearish cs index =
    match getCandlestickPattern 8 1 cs index with
    | Some { trend = t ; significant = s } ->
        upwardTrend t && noUpperShadow s.[0] && smallLowerShadow s.[0] && longBlackBody s.[0]
    | None -> false

let (|BeltHoldBearish|_|) (cs, index) =
    if beltHoldBearish cs index then Some BeltHoldBearish else None

let dojiDragonFly cs index =
    match getCandlestickPattern 0 1 cs index with
    | Some { significant = s } ->
        doji s.[0] && smallUpperShadow s.[0] && longLowerShadow s.[0]
    | None -> false

let (|DojiDragonFly|_|) (cs, index) =
    if dojiDragonFly cs index then Some DojiDragonFly else None

let hammer cs index =
    match getCandlestickPattern 8 1 cs index with
    | Some { trend = t ; significant = s } ->
        downwardTrend t && smallBody s.[0] && not <| noLowerShadow s.[0]
        && 2m * bodyHeight s.[0] < lowerShadowHeight s.[0] 
        && lowerShadowHeight s.[0] < 3m * bodyHeight s.[0]
        && (smallUpperShadow s.[0] || noUpperShadow s.[0])
    | None -> false

let (|Hammer|_|) (cs, index) =
    if hammer cs index then Some Hammer else None

let hangingMan cs index =
    match getCandlestickPattern 8 1 cs index with
    | Some { trend = t ; significant = s } ->
        upwardTrend t && noUpperShadow s.[0] && longLowerShadow s.[0] && smallBody s.[0]
    | None -> false

let (|HangingMan|_|) (cs, index) =
    if hangingMan cs index then Some HangingMan else None

