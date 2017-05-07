module Dispatch exposing (..)

type Dispatch ctrlMsg nodeMsg
    = Self nodeMsg
    | Ctrl ctrlMsg

type DispatchHub key ctrlMsg nodeMsg hubMsg
    = Hub hubMsg
    | Send key (Dispatch ctrlMsg nodeMsg)
    | Forward ctrlMsg
