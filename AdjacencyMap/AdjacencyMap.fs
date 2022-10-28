module AdjacencyMap

open DomainTypes


type SymbolAdjacencyMap = Map<TradingAsset, Map<TradingAsset, (Symbol * decimal<rate>)>>


let reverseCurrencyPairs (currencyPairs: CurrencyPair list) =
    currencyPairs
    |> List.map (fun pair ->
        { symbol = pair.symbol
        ; baseAsset = pair.quoteAsset
        ; quoteAsset = pair.baseAsset
        ; rate = (1.0m<rate> / decimal pair.rate) }
    )
    

let createAdjacencyMap (currencyPairs: CurrencyPair list): SymbolAdjacencyMap =
    let mutable (adjacencyMap: SymbolAdjacencyMap) = Map.empty
    for { symbol = symbol
        ; baseAsset = baseAsset
        ; quoteAsset = quoteAsset
        ; rate = rate 
        } in currencyPairs @ (reverseCurrencyPairs currencyPairs) do
        if not <| adjacencyMap.ContainsKey baseAsset then 
            adjacencyMap <- adjacencyMap.Add (baseAsset, Map.empty.Add (quoteAsset, (symbol, rate)))
        else
            let existingValue = adjacencyMap.[baseAsset]
            adjacencyMap <- adjacencyMap.Add (baseAsset, existingValue.Add (quoteAsset, (symbol, rate)))
    adjacencyMap


//let findAssetRateAgainstOther (adjacencyMap: SymbolAdjacencyMap) (targetAsset: TradingAsset) (sourceAsset: TradingAsset ) =
//    let sourceAdjacency = adjacencyMap.[sourceAsset]
//    let rec loop acc (currentAdjacency: Map<TradingAsset, (Symbol * decimal<rate>)>)  =
//        if currentAdjacency.ContainsKey(targetAsset) then
//            acc :: [snd currentAdjacency.[targetAsset]]
//        else
//            []
//
//    loop (0.0m * 1.0m<rate>) sourceAdjacency

