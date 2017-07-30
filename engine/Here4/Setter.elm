module Here4.Setter
    exposing
        ( Update
        , applyUpdates
        )


type alias Update t =
    t -> t


applyUpdates : List (Update t) -> t -> t
applyUpdates updates attributes =
    List.foldl (\f attr -> f attr) attributes updates
