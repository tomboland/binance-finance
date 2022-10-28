module AssetBalances
open BinanceApi

[<Measure>] type equity

type Asset = Asset of string

type Balance = {
    asset: Asset
    free: decimal<equity>
    locked: decimal<equity>
}

let balanceFromDTO (balance: BalanceDTO) =
    { asset = Asset balance.asset
    ; free = (decimal balance.free) * 1.0m<equity>
    ; locked =(decimal balance.locked) * 1.0m<equity> }

let binanceAccountBalances (apiContext: BinanceApiContext) =
    accountInformationRequest apiContext
    |> function
    | Success accountInformation ->
        accountInformation.balances
        |> List.map balanceFromDTO
    | Failure err -> failwith $"{err}"


let totalOfBalance (balance: Balance): decimal<equity> =
    balance.free + balance.locked