module Dispatch exposing (..)


type Dispatch effectMsg ctrlMsg nodeMsg
    = Self nodeMsg
    | Ctrl ctrlMsg
    | Effect effectMsg


type DispatchHub key effectMsg ctrlMsg nodeMsg globalMsg hubMsg
    = Hub hubMsg
    | Send key (Dispatch effectMsg ctrlMsg nodeMsg)
    | Forward key ctrlMsg
    | HubEff effectMsg
    | GlobalEffect globalMsg
