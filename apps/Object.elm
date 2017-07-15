module Object exposing
    ( ObjectAttributes(..)
    , ObjectResult(..)
    , ObjectMsg(..)
    , objectInit
    , objectUpdate
    , texturedObj
    , texturedObjWith
    )

import Appearance exposing (Appearance)
import Object.FlatTexture exposing (..)
import Object.ReflectiveObj exposing (..)
import Object.TexturedObj as TexturedObj exposing (..)
import Object.Types exposing (Load(..))
import Setter exposing (..)
import Tuple

type ObjectAttributes
    = Invisible ()
    | Appearance Appearance
    | FlatTexture FlatTextureAttributes
    | TexturedObj TexturedObjAttributes
    | ReflectiveObj ReflectiveObjAttributes

type ObjectResult
    = InvisibleResult ()
    | FlatTextureResult FlatTextureResult
    | TexturedObjResult TexturedObjResult
    | ReflectiveObjResult ReflectiveObjResult

type ObjectMsg
    = FlatTextureMsg FlatTextureMsg
    | TexturedObjMsg TexturedObjMsg
    | ReflectiveObjMsg ReflectiveObjMsg


texturedObj : String -> String -> String -> ObjectAttributes
texturedObj meshPath diffuseTexturePath normalTexturePath =
    TexturedObj.texturedObj meshPath diffuseTexturePath normalTexturePath
    |> TexturedObj

texturedObjWith : String -> String -> String
    -> List (Update TexturedObjAttributes)
    -> ObjectAttributes
texturedObjWith meshPath diffuseTexturePath normalTexturePath updates =
    TexturedObj.texturedObj meshPath diffuseTexturePath normalTexturePath
    |> applyUpdates updates
    |> TexturedObj

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
        Invisible () ->
            ( Loading (InvisibleResult ()), Cmd.none )
        Appearance appear ->
            ( Ready appear, Cmd.none )
        FlatTexture obj ->
            flatTextureInit obj
            |> wrap FlatTextureResult FlatTextureMsg
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
                (FlatTextureMsg msg_, FlatTextureResult model_) ->
                    flatTextureUpdate msg_ (Loading model_)
                    |> wrap FlatTextureResult FlatTextureMsg
                (TexturedObjMsg msg_, TexturedObjResult model_) ->
                    texturedObjUpdate msg_ (Loading model_)
                    |> wrap TexturedObjResult TexturedObjMsg
                (ReflectiveObjMsg msg_, ReflectiveObjResult model_) ->
                    reflectiveObjUpdate msg_ (Loading model_)
                    |> wrap ReflectiveObjResult ReflectiveObjMsg
                _ ->
                    ( model, Cmd.none )
                

