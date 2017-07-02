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
    , parties : Bag Party
    }


type alias Party =
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
          , parties = Bag.empty
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
                mRideKey =
                    Bag.get key model.parties
                    |> Maybe.andThen .rideKey
            in
                case mRideKey of
                    Just rideKey ->
                        case Bag.get rideKey model.apps of
                            Just t ->
                                let
                                    ( appModel, appCmdMsg ) =
                                        App.update (Ctrl fwdMsg) t
                                in
                                    ( { model | apps = Bag.replace rideKey appModel model.apps }
                                    , Cmd.map (Send rideKey) appCmdMsg
                                    )

                            Nothing ->
                                ( model, Cmd.none )
                    Nothing ->
                        ( model, Cmd.none )


worldAnimate : Ground -> Time -> WorldModel a -> WorldModel a
worldAnimate ground dt model =
    { model | apps = Bag.map (App.animate ground dt) model.apps }


worldJoin : WorldModel a -> (WorldModel a, Bag.Key)
worldJoin model =
    let
        freshParty = { rideKey = Nothing }

        ( key, newPartys ) =
            Bag.insert freshParty model.parties

    in
        ( { model | parties = newPartys }, key )


worldLeave : Bag.Key -> WorldModel a -> WorldModel a
worldLeave key model =
    let
        newPartys = Bag.remove key model.parties
    in
        { model | parties = newPartys }
    

worldChangeRide : Bag.Key -> WorldModel a -> WorldModel a
worldChangeRide partyKey model =
    let
        keyLimit =
            Bag.size model.apps

        hasFraming key =
            isJust (Maybe.andThen App.framing (Bag.get key model.apps))

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
            Bag.get partyKey model.parties
            |> Maybe.andThen .rideKey

        key =
            findCamera (Maybe.withDefault 0 rideKey)

        newKey =
            Just (findCamera (nextKey key))
    in
        { model | parties = Bag.replace partyKey { rideKey = newKey } model.parties }

worldLabel : Maybe Bag.Key -> WorldModel a -> String
worldLabel mkey model =
    let
        none =
            "<>"

        mApp =
            Maybe.andThen (\k -> Bag.get k model.parties) mkey
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
            Maybe.andThen (\k -> Bag.get k model.parties) mPartiKey
            |> Maybe.andThen .rideKey
-}
    in
        case mPartiKey of
            Just partyKey ->
                case Bag.get partyKey model.parties of
                    Just party ->
                        case party.rideKey of
                            Just key ->
                                case Bag.get key model.apps of
                                    Just app ->
                                        Html.map (Send key) (App.overlay app)

                                    Nothing ->
                                        Html.text "App not found"

                            Nothing ->
                                Html.text ("No ride for party " ++ toString partyKey)

                    Nothing ->
                        Html.text "Party not found"

            Nothing ->
                Html.text "No party"


worldFraming : Maybe Bag.Key -> WorldModel a -> Maybe Framing
worldFraming mkey model =
    let
        mApp =
            Maybe.andThen (\k -> Bag.get k model.parties) mkey
            |> Maybe.andThen .rideKey
            |> Maybe.andThen (\k -> Bag.get k model.apps)
    in
        Maybe.andThen App.framing mApp


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
