module Here4.Dispatch exposing (..)


type Dispatch effectMsg ctrlMsg nodeMsg
    = Self nodeMsg
    | Ctrl ctrlMsg
    | Effect effectMsg


type DispatchHub key effectMsg ctrlMsg nodeMsg globalMsg navMsg hubMsg
    = Hub hubMsg
    | Send key (Dispatch effectMsg ctrlMsg nodeMsg)
    | Forward key ctrlMsg
    | HubEff effectMsg
    | GlobalEffect globalMsg
    | NavEffect navMsg
