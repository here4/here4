module Dispatch exposing (..)

type Dispatch downMsg selfMsg
    = Down downMsg
    | Self selfMsg

type DispatchHub key ctrlMsg nodeMsg hubMsg
    = Hub hubMsg
    | Send key (Dispatch ctrlMsg nodeMsg)
    | Forward ctrlMsg
