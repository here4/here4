module Object.Types exposing
    ( Load(..)
    )


import Appearance exposing (Appearance)

type Load result
    = Loading result
    | Ready Appearance


