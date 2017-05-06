module Dispatch exposing (..)

type Dispatch downMsg selfMsg
    = Down downMsg
    | Self selfMsg
