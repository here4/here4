module World exposing (WorldModel, create)

import Html exposing (Html)
import Bag exposing (Bag)
import Time exposing (Time)
import Space
import Control exposing (WorldMsg)
import Maybe.Extra exposing (isJust)
import Dispatch exposing (..)
import Dynamic exposing (Dynamic)
import Model exposing (Args)
import Body exposing (Body)
import Camera exposing (Framing, Shot)
import App exposing (..)
import Ground exposing (Ground)

type alias WorldModel a =
    { worldModel : a
    , maybeGround : Maybe Ground
    , apps : Bag App
    , participants : Bag Participant
    }


type alias Participant =
    { rideKey : Maybe Bag.Key
    -- , focusKey : Bag.Key
    }


create :
    ( model, Cmd msg )
    -> (msg -> model -> ( model, Cmd msg ))
    -> { apps : List ( App, Cmd AppMsg ) }
    -> Program Args (Model.Model (WorldModel model) (WorldMsg msg)) (Model.Msg (WorldMsg msg))
create hubInit hubUpdate details =
    Space.programWithFlags
        { init = worldInit hubInit details
        , view = worldView
        , update = worldUpdate hubUpdate
        , label = worldLabel
        , overlay = worldOverlay
        , keyLimit = worldKeyLimit
        , animate = worldAnimate
        , join = worldJoin
        , leave = worldLeave
        , changeRide = worldChangeRide
        , framing = worldFraming
        , focus = worldFocus
        , ground = worldGround
        }


worldApps : List ( App, Cmd AppMsg ) -> ( Bag App, Cmd (WorldMsg a) )
worldApps appsList =
    let
        f ( newApps, newCmdMsg ) ( oldBag, oldCmdMsgs ) =
            let
                ( key, newBag ) =
                    Bag.insert newApps oldBag
            in
                ( newBag, oldCmdMsgs ++ [ Cmd.map (Send key) newCmdMsg ] )

        ( appsBag, unbatched ) =
            List.foldl f ( Bag.empty, [] ) appsList
    in
        ( appsBag, Cmd.batch unbatched )


worldInit :
    ( model, Cmd msg )
    -> { apps : List ( App, Cmd AppMsg ) }
    -> ( WorldModel model, Cmd (WorldMsg msg) )
worldInit hubInit details =
    let
        ( hubModel, hubCmd ) =
            hubInit

        ( appsBag, appCmds ) =
            worldApps details.apps
    in
        ( { worldModel = hubModel
          , maybeGround = Nothing
          , apps = appsBag
          , participants = Bag.empty
          }
        , Cmd.batch
            [ Cmd.map Hub hubCmd
            , appCmds
            ]
        )


worldView : WorldModel model -> Maybe Model.World
worldView model =
    case model.maybeGround of
        Nothing ->
            Nothing

        Just ground ->
            Just (makeWorld ground model)


makeWorld : Ground -> WorldModel a -> Model.World
makeWorld ground model =
    let
        worldBodies =
            List.concatMap bodies (Bag.items model.apps)
    in
        { bodies = worldBodies, ground = ground }


worldUpdate :
    (msg -> model -> ( model, Cmd msg ))
    -> WorldMsg msg
    -> WorldModel model
    -> ( WorldModel model, Cmd (WorldMsg msg) )
worldUpdate hubUpdate msg model =
    case msg of
        Hub hubMsg ->
            let
                ( hubModel, hubCmd ) =
                    hubUpdate hubMsg model.worldModel
            in
                ( { model | worldModel = hubModel }, Cmd.map Hub hubCmd )

        HubEff (Control.UpdateGround ground) ->
            ( { model | maybeGround = Just ground }, Cmd.none )

        Send key appMsg ->
            case Bag.get key model.apps of
                Nothing ->
                    ( model, Cmd.none )

                Just t ->
                    let
                        ( appModel, appCmdMsg ) =
                            App.update appMsg t

                        foo x =
                            case x of
                                Effect e ->
                                    HubEff e

                                m ->
                                    Send key m
                    in
                        ( { model | apps = Bag.replace key appModel model.apps }
                        , Cmd.map foo appCmdMsg
                        )

        Forward key fwdMsg ->
            let
                mApp =
                    Bag.get key model.participants
                    |> Maybe.andThen .rideKey
                    |> Maybe.andThen (\k -> Bag.get k model.apps)
            in
                case mApp of
                    Just t ->
                        let
                            ( appModel, appCmdMsg ) =
                                App.update (Ctrl fwdMsg) t
                        in
                            ( { model | apps = Bag.replace key appModel model.apps }
                            , Cmd.map (Send key) appCmdMsg
                            )

                    Nothing ->
                        ( model, Cmd.none )



worldKeyLimit : WorldModel a -> Int
worldKeyLimit model =
    Bag.size model.apps


worldAnimate : Ground -> Time -> WorldModel a -> WorldModel a
worldAnimate ground dt model =
    { model | apps = Bag.map (App.animate ground dt) model.apps }


worldJoin : WorldModel a -> (WorldModel a, Bag.Key)
worldJoin model =
    let
        freshParticipant = { rideKey = Nothing }

        ( key, newParticipants ) =
            Bag.insert freshParticipant model.participants

    in
        ( { model | participants = newParticipants }, key )


worldLeave : Bag.Key -> WorldModel a -> WorldModel a
worldLeave key model =
    let
        newParticipants = Bag.remove key model.participants
    in
        { model | participants = newParticipants }
    

worldChangeRide : Bag.Key -> WorldModel a -> WorldModel a
worldChangeRide partiKey model =
    let
        keyLimit =
            worldKeyLimit model

        hasFraming key =
            isJust (worldFraming (Just key) model)

        nextKey key =
            (key + 1) % keyLimit

        findCameraHelp origKey key =
            if hasFraming key then
                key
            else
                let
                    next =
                        nextKey key
                in
                    if next == origKey then
                        origKey
                    else
                        findCameraHelp origKey next

        findCamera key =
            findCameraHelp key key

        rideKey =
            Bag.get partiKey model.participants
            |> Maybe.andThen .rideKey

        key =
            findCamera (Maybe.withDefault 0 rideKey)

        newKey =
            Just (findCamera (nextKey key))
    in
        { model | participants = Bag.replace partiKey { rideKey = newKey } model.participants }

worldLabel : Maybe Bag.Key -> WorldModel a -> String
worldLabel mkey model =
    let
        none =
            "<>"

        mApp =
            Maybe.andThen (\k -> Bag.get k model.participants) mkey
            |> Maybe.andThen .rideKey
            |> Maybe.andThen (\k -> Bag.get k model.apps)
    in
        case mApp of
            Just app ->
                App.label app

            Nothing ->
                none

worldOverlay : Maybe Bag.Key -> WorldModel a -> Html (WorldMsg msg)
worldOverlay mPartiKey model =
    let
        none =
            Html.text "Welcome to DreamBuggy"

{-
        rideKey =
            Maybe.andThen (\k -> Bag.get k model.participants) mPartiKey
            |> Maybe.andThen .rideKey
-}
    in
        case mPartiKey of
            Just partiKey ->
                case Bag.get partiKey model.participants of
                    Just parti ->
                        case parti.rideKey of
                            Just key ->
                                case Bag.get key model.apps of
                                    Just app ->
                                        Html.map (Send key) (App.overlay app)

                                    Nothing ->
                                        Html.text "App not found"

                            Nothing ->
                                Html.text ("No ride for participant " ++ toString partiKey)

                    Nothing ->
                        Html.text "Participant not found"

            Nothing ->
                Html.text "No participant"


worldFraming : Maybe Bag.Key -> WorldModel a -> Maybe Framing
worldFraming mkey model =
    case mkey of
        Just key ->
            case Bag.get key model.apps of
                Just app ->
                    App.framing app

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


worldFocus : Bag.Key -> WorldModel a -> Maybe Focus
worldFocus key model =
    case Bag.get key model.apps of
        Just app ->
            App.focus app

        _ ->
            Nothing


worldGround : WorldModel model -> Maybe Ground
worldGround model =
    model.maybeGround
