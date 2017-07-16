module Object
    exposing
        ( objectInit
        , objectUpdate
        )

import Math.Vector3 exposing (Vec3)
import Object.Attributes exposing (ObjectAttributes(..), ObjectResult(..), ObjectMsg(..))
import Object.FlatTexture exposing (flatTextureInit, flatTextureUpdate)
import Object.ReflectiveObj exposing (reflectiveObjInit, reflectiveObjUpdate)
import Object.TexturedObj exposing (texturedObjInit, texturedObjUpdate)
import Object.Types exposing (Load(..))
import Setter exposing (..)
import Tuple



wrap :
    (result -> ObjectResult)
    -> (msg -> ObjectMsg)
    -> ( Load result, Cmd msg )
    -> ( Load ObjectResult, Cmd ObjectMsg )
wrap t m ( model, msg ) =
    case model of
        Loading model_ ->
            ( Loading (t model_), Cmd.map m msg )

        Ready appear dimensions ->
            ( Ready appear dimensions, Cmd.none )


objectInit : ObjectAttributes -> ( Load ObjectResult, Cmd ObjectMsg )
objectInit attributes =
    case attributes of
        Invisible () ->
            ( Loading (InvisibleResult ()), Cmd.none )

        Appearance appear dimensions ->
            ( Ready appear dimensions, Cmd.none )

        FlatTexture obj ->
            flatTextureInit obj
                |> wrap FlatTextureResult FlatTextureMsg

        TexturedObj obj ->
            texturedObjInit obj
                |> wrap TexturedObjResult TexturedObjMsg

        ReflectiveObj obj ->
            reflectiveObjInit obj
                |> wrap ReflectiveObjResult ReflectiveObjMsg


objectUpdate : ObjectMsg -> Load ObjectResult -> ( Load ObjectResult, Cmd ObjectMsg )
objectUpdate msg model =
    case model of
        Ready appear dimensions ->
            ( Ready appear dimensions, Cmd.none )

        Loading partial ->
            case ( msg, partial ) of
                ( FlatTextureMsg msg_, FlatTextureResult model_ ) ->
                    flatTextureUpdate msg_ (Loading model_)
                        |> wrap FlatTextureResult FlatTextureMsg

                ( TexturedObjMsg msg_, TexturedObjResult model_ ) ->
                    texturedObjUpdate msg_ (Loading model_)
                        |> wrap TexturedObjResult TexturedObjMsg

                ( ReflectiveObjMsg msg_, ReflectiveObjResult model_ ) ->
                    reflectiveObjUpdate msg_ (Loading model_)
                        |> wrap ReflectiveObjResult ReflectiveObjMsg

                _ ->
                    ( model, Cmd.none )
