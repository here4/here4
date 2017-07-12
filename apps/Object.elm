module Object exposing
    ( ObjectAttributes(..)
    , ObjectResult(..)
    , ObjectMsg(..)
    , objectInit
    , objectUpdate
    )

import Appearance exposing (Appearance)
import Object.ReflectiveObj exposing (..)
import Object.TexturedObj exposing (..)
import Object.Types exposing (Load(..))
import Tuple

type ObjectAttributes
    = Appearance Appearance
    | TexturedObj TexturedObjAttributes
    | ReflectiveObj ReflectiveObjAttributes

type ObjectResult
    = TexturedObjResult TexturedObjResult
    | ReflectiveObjResult ReflectiveObjResult

type ObjectMsg
    = TexturedObjMsg TexturedObjMsg
    | ReflectiveObjMsg ReflectiveObjMsg


wrap :
    (result -> ObjectResult)
    -> (msg -> ObjectMsg)
    -> (Load result, Cmd msg)
    -> (Load ObjectResult, Cmd ObjectMsg)
wrap t m (model, msg) =
    case model of
        Loading model_ ->
            ( Loading (t model_), Cmd.map m msg )
        Ready appear ->
            ( Ready appear, Cmd.none )


objectInit : ObjectAttributes -> (Load ObjectResult, Cmd ObjectMsg)
objectInit attributes =
    case attributes of
        Appearance appear ->
            ( Ready appear, Cmd.none )
        TexturedObj obj ->
            texturedObjInit obj
            |> wrap TexturedObjResult TexturedObjMsg
        ReflectiveObj obj ->
            reflectiveObjInit obj
            |> wrap ReflectiveObjResult ReflectiveObjMsg


objectUpdate : ObjectMsg -> Load ObjectResult -> (Load ObjectResult, Cmd ObjectMsg)
objectUpdate msg model =
    case model of
        Ready appear ->
            ( Ready appear, Cmd.none )

        Loading partial ->
            case (msg, partial) of
                (TexturedObjMsg msg_, TexturedObjResult model_) ->
                    texturedObjUpdate msg_ (Loading model_)
                    |> wrap TexturedObjResult TexturedObjMsg
                (ReflectiveObjMsg msg_, ReflectiveObjResult model_) ->
                    reflectiveObjUpdate msg_ (Loading model_)
                    |> wrap ReflectiveObjResult ReflectiveObjMsg
                _ ->
                    ( model, Cmd.none )
                

