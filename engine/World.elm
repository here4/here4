module World exposing (Attributes, Multiverse, create)

import Html exposing (Html)
import Bag exposing (Bag)
import Time exposing (Time)
import Space
import Control exposing (..)
import Maybe.Extra exposing (isJust)
import Dispatch exposing (..)
import Dynamic exposing (Dynamic)
import Model exposing (Args, GlobalMsg, WorldKey(..), AppKey(..), PartyKey(..))
import Location exposing (..)
import Body exposing (Body)
import Camera exposing (Framing, Shot)
import App exposing (..)
import Ground exposing (Ground)
import Math.Vector3 as V3 exposing (vec3)
import Orientation exposing (Orientation)
import Task


type alias Attributes =
    { id : String
    , label : String
    , apps : List ( App, Cmd AppMsg )
    , defaultSelf : ( App, Cmd AppMsg )
    }


type alias Multiverse a =
    { worldModel : a
    , worlds : Bag Stuff
    }


type alias Stuff =
    { id : String
    , label : String
    , maybeGround : Maybe Ground
    , apps : Bag App
    , parties : Bag Party
    , defaultSelf : ( App, Cmd AppMsg )
    }


type alias Party =
    { rideKey : Maybe AppKey
    , self : App -- App to be when not riding

    -- , focusKey : Bag.Key
    }


create :
    ( model, Cmd msg )
    -> (msg -> model -> ( model, Cmd msg ))
    -> List Attributes
    -> Program Args (Model.Model (Multiverse model) (WorldMsg msg)) (Model.Msg (WorldMsg msg))
create hubInit hubUpdate attributes =
    Space.programWithFlags
        { init = worldInit hubInit attributes
        , view = worldView
        , update = worldUpdate hubUpdate
        , worldId = worldId
        , worldLabel = worldLabel
        , partyLabel = worldPartyLabel
        , overlay = worldOverlay
        , animate = worldAnimate
        , join = worldJoin
        , leave = worldLeave
        , changeRide = worldChangeRide
        , framing = worldFraming
        , focus = worldFocus
        , ground = worldGround
        }


worldApp : WorldKey AppKey -> Multiverse a -> Maybe App
worldApp (WorldKey worldKey (AppKey appKey)) model =
    Bag.get worldKey model.worlds
        |> Maybe.map .apps
        |> Maybe.andThen (Bag.get appKey)


worldParty : WorldKey PartyKey -> Multiverse a -> Maybe Party
worldParty (WorldKey worldKey (PartyKey partyKey)) model =
    Bag.get worldKey model.worlds
        |> Maybe.map .parties
        |> Maybe.andThen (Bag.get partyKey)


worldApps : WorldKey () -> List ( App, Cmd AppMsg ) -> ( Bag App, List (Cmd (WorldMsg msg)) )
worldApps (WorldKey worldKey ()) appsList =
    let
        f ( newApps, newCmdMsg ) ( oldBag, oldCmdMsgs ) =
            let
                ( key, newBag ) =
                    Bag.insert newApps oldBag
            in
                ( newBag, oldCmdMsgs ++ [ Cmd.map (Send (ToApp (WorldKey worldKey (AppKey key)))) newCmdMsg ] )

        ( appsBag, unbatched ) =
            List.foldl f ( Bag.empty, [] ) appsList

        worldCmds =
            List.map (Cmd.map (toWorldMsg (WorldKey worldKey ()))) unbatched
    in
        ( appsBag, worldCmds )


oneWorldInit :
    Attributes
    -> ( Bag Stuff, List (Cmd (WorldMsg msg)) )
    -> ( Bag Stuff, List (Cmd (WorldMsg msg)) )
oneWorldInit attributes ( oldWorlds, oldCmds ) =
    let
        emptyStuff =
            { id = attributes.id
            , label = attributes.label
            , maybeGround = Nothing
            , apps = Bag.empty
            , parties = Bag.empty
            , defaultSelf = attributes.defaultSelf
            }

        ( worldKey, oneWorlds ) =
            Bag.insert emptyStuff oldWorlds

        ( appsBag, appCmds ) =
            worldApps (WorldKey worldKey ()) attributes.apps

        updateApps stuff =
            { stuff | apps = appsBag }

        newWorlds =
            Bag.update worldKey (Maybe.map updateApps) oneWorlds
    in
        ( newWorlds, oldCmds ++ appCmds )


worldInit :
    ( model, Cmd msg )
    -> List Attributes
    -> ( Multiverse model, Cmd (WorldMsg msg) )
worldInit hubInit attributes =
    let
        ( hubModel, hubCmd ) =
            hubInit

        ( worldsBag, worldsCmds ) =
            List.foldl oneWorldInit ( Bag.empty, [] ) attributes
    in
        ( { worldModel = hubModel
          , worlds = worldsBag
          }
        , Cmd.batch (Cmd.map Hub hubCmd :: worldsCmds)
        )


worldView : WorldKey () -> Multiverse model -> Maybe Model.World
worldView (WorldKey worldKey ()) model =
    let
        mStuff =
            Bag.get worldKey model.worlds
    in
        case Maybe.andThen .maybeGround mStuff of
            Nothing ->
                Nothing

            Just ground ->
                Maybe.map (makeWorld ground) mStuff


makeWorld : Ground -> Stuff -> Model.World
makeWorld ground stuff =
    let
        partyBodies party =
            case party.rideKey of
                Just _ ->
                    []

                Nothing ->
                    bodies party.self

        worldBodies =
            List.concatMap bodies (Bag.items stuff.apps)
                ++ List.concatMap partyBodies (Bag.items stuff.parties)
    in
        { bodies = worldBodies, ground = ground }


toWorldEffect : WorldKey () -> EffectMsg () -> EffectMsg (WorldKey ())
toWorldEffect worldKey e =
    case e of
        UpdateGround () ground ->
            UpdateGround worldKey ground

        RelocateParty () partyKey location ->
            RelocateParty worldKey partyKey location


toWorldMsg :
    WorldKey ()
    -> DispatchHub Route (EffectMsg ()) Msg Dynamic GlobalMsg a
    -> DispatchHub Route (EffectMsg (WorldKey ())) Msg Dynamic GlobalMsg a
toWorldMsg worldKey msg =
    let
        toWorldDispatch d =
            case d of
                Self nodeMsg ->
                    Self nodeMsg

                Ctrl ctrlMsg ->
                    Ctrl ctrlMsg

                Effect e ->
                    Effect (toWorldEffect worldKey e)
    in
        case msg of
            Hub hubMsg ->
                Hub hubMsg

            Send key dispatch ->
                Send key (toWorldDispatch dispatch)

            Forward key ctrlMsg ->
                Forward key ctrlMsg

            HubEff e ->
                HubEff (toWorldEffect worldKey e)

            GlobalEffect globalMsg ->
                GlobalEffect globalMsg


toAppMsg : Dispatch (EffectMsg (WorldKey ())) Msg Dynamic -> AppMsg
toAppMsg dispatch =
    let
        toAppEffect e =
            case e of
                UpdateGround _ ground ->
                    UpdateGround () ground

                RelocateParty _ partyKey location ->
                    RelocateParty () partyKey location
    in
        case dispatch of
            Self nodeMsg ->
                Self nodeMsg

            Ctrl ctrlMsg ->
                Ctrl ctrlMsg

            Effect e ->
                Effect (toAppEffect e)


relativeAppPosition : WorldKey PartyKey -> Relative -> Multiverse model -> ( Maybe AppKey, Maybe AppPosition )
relativeAppPosition (WorldKey worldKey (PartyKey partyKey)) relative model =
    let
        -- lookup : AppId -> (AppKey, App)
        lookup appId =
            Bag.get worldKey model.worlds
                |> Maybe.map .apps
                |> Maybe.andThen (Bag.find (\app -> App.id app == appId))

        mTarget appId =
            lookup appId
                |> Maybe.map Tuple.second
                |> Maybe.andThen (App.framing (PartyKey partyKey))
                |> Maybe.map .target

        mAppKey appId =
            lookup appId
                |> Maybe.map (Tuple.first >> AppKey)

        near d target =
            let
                position =
                    V3.add target.position (V3.scale d (Model.direction target))

                displacement =
                    V3.sub target.position position

                orientation =
                    Orientation.fromTo V3.k displacement
            in
                { position = position
                , orientation = orientation
                }
    in
        case relative of
            At position o ->
                case o of
                    FacingNorth ->
                        ( Nothing
                        , Just
                            { position = position
                            , orientation = Orientation.initial
                            }
                        )

                    WithOrientation orientation ->
                        ( Nothing
                        , Just
                            { position = position
                            , orientation = orientation
                            }
                        )

            Facing appId ->
                ( Nothing, Maybe.map (near 7) (mTarget appId) )

            Behind appId ->
                ( Nothing, Maybe.map (near -7) (mTarget appId) )

            Become appId ->
                ( mAppKey appId, Nothing )


relativeRelocate : WorldKey PartyKey -> Relative -> Multiverse model -> ( Multiverse model, Cmd (WorldMsg msg) )
relativeRelocate worldPartyKey relative model =
    let
        (WorldKey worldKey (PartyKey partyKey)) = worldPartyKey

        updateRide party =
            case relativeAppPosition worldPartyKey relative model of
                ( Just rideKey, _ ) ->
                    ( { party | rideKey = Just rideKey }
                    , Cmd.map (Send (ToApp (WorldKey worldKey rideKey)))
                        (Task.succeed (PartyKey partyKey) |> Task.perform (Ctrl << Enter))
                    )

                ( _, Just appPosition ) ->
                    ( { party
                        | rideKey = Nothing
                        , self = App.reposition (Just appPosition) party.self
                      }
                    , Cmd.none
                    )

                _ ->
                    ( party, Cmd.none )

        mNewPartyCmds =
            Maybe.map updateRide <| worldParty (WorldKey worldKey (PartyKey partyKey)) model

        mNewParty =
            Maybe.map Tuple.first mNewPartyCmds

        newCmds =
            Maybe.map Tuple.second mNewPartyCmds
                |> Maybe.withDefault Cmd.none

        updateParties stuff =
            { stuff | parties = Bag.update partyKey (always mNewParty) stuff.parties }
    in
        ( { model | worlds = Bag.update worldKey (Maybe.map updateParties) model.worlds }
        , newCmds
        )


remoteRelocate : WorldKey PartyKey -> WorldId -> Relative -> Multiverse model -> ( Multiverse model, Cmd (WorldMsg msg) )
remoteRelocate oldWorldPartyKey remoteWorldId relative model =
    let
        -- WorldKey worldKey (PartyKey partyKey) = oldWorldPartyKey
        mRemote =
            Bag.find (\world -> world.id == remoteWorldId) model.worlds
    in
        case mRemote of
            Just ( remoteWorldKey, remoteWorld ) ->
                let
                    -- leave this world
                    leftThisWorld =
                        worldLeave oldWorldPartyKey model

                    -- enter next world
                    ( mNewWorldPartyKey, joinedNewWorld, joinCmd ) =
                        worldJoin (WorldKey remoteWorldKey ()) leftThisWorld
                in
                    case mNewWorldPartyKey of
                        Just newWorldPartyKey ->
                            let
                                -- relocate relative
                                ( newModel, relativeCmd ) =
                                    relativeRelocate newWorldPartyKey relative joinedNewWorld

                                playerUpdateMsg =
                                    Model.PlayerUpdate oldWorldPartyKey newWorldPartyKey

                                playerUpdateCmd =
                                    Task.succeed playerUpdateMsg |> Task.perform GlobalEffect
                            in
                                ( newModel
                                , Cmd.batch
                                    [ playerUpdateCmd
                                    , relativeCmd
                                    , joinCmd
                                    ]
                                )

                        Nothing ->
                            ( model, Cmd.none )

            Nothing ->
                ( model, Cmd.none )


relocate : WorldKey PartyKey -> Location -> Multiverse model -> ( Multiverse model, Cmd (WorldMsg msg) )
relocate worldPartyKey location model =
    case location of
        Local relative ->
            relativeRelocate worldPartyKey relative model

        Remote remote relative ->
            remoteRelocate worldPartyKey remote relative model


worldUpdate :
    (msg -> model -> ( model, Cmd msg ))
    -> WorldMsg msg
    -> Multiverse model
    -> ( Multiverse model, Cmd (WorldMsg msg) )
worldUpdate hubUpdate msg model =
    case msg of
        Hub hubMsg ->
            let
                ( hubModel, hubCmd ) =
                    hubUpdate hubMsg model.worldModel
            in
                ( { model | worldModel = hubModel }, Cmd.map Hub hubCmd )

        HubEff (Control.UpdateGround (WorldKey worldKey ()) ground) ->
            let
                updateGround stuff =
                    { stuff | maybeGround = Just ground }
            in
                ( { model | worlds = Bag.update worldKey (Maybe.map updateGround) model.worlds }
                , Cmd.none
                )

        HubEff (Control.RelocateParty (WorldKey worldKey ()) (PartyKey partyKey) location) ->
            relocate (WorldKey worldKey (PartyKey partyKey)) location model

        Send key appMsg ->
            let
                ( mApp, worldKey, updateModel ) =
                    case key of
                        ToApp (WorldKey worldKey (AppKey appKey)) ->
                            ( worldApp (WorldKey worldKey (AppKey appKey)) model
                            , worldKey
                            , \newApp ->
                                let
                                    updateApps stuff =
                                        { stuff | apps = Bag.replace appKey newApp stuff.apps }
                                in
                                    { model | worlds = Bag.update worldKey (Maybe.map updateApps) model.worlds }
                            )

                        ToParty (WorldKey worldKey (PartyKey partyKey)) ->
                            let
                                u newSelf party =
                                    { party | self = newSelf }
                            in
                                ( Maybe.map .self <| worldParty (WorldKey worldKey (PartyKey partyKey)) model
                                , worldKey
                                , \newSelf ->
                                    let
                                        updateParties stuff =
                                            { stuff | parties = Bag.update partyKey (Maybe.map (u newSelf)) stuff.parties }
                                    in
                                        { model | worlds = Bag.update worldKey (Maybe.map updateParties) model.worlds }
                                )

                response x =
                    case x of
                        Effect e ->
                            HubEff (toWorldEffect (WorldKey worldKey ()) e)

                        m ->
                            toWorldMsg (WorldKey worldKey ()) (Send key m)
            in
                case mApp of
                    Nothing ->
                        ( model, Cmd.none )

                    Just app ->
                        let
                            ( appModel, appCmdMsg ) =
                                App.update (toAppMsg appMsg) app
                        in
                            ( updateModel appModel
                            , Cmd.map response appCmdMsg
                            )

        Forward (ToParty (WorldKey worldKey (PartyKey partyKey))) fwdMsg ->
            case worldParty (WorldKey worldKey (PartyKey partyKey)) model of
                Just party ->
                    case party.rideKey of
                        Just (AppKey rideKey) ->
                            case worldApp (WorldKey worldKey (AppKey rideKey)) model of
                                Just t ->
                                    let
                                        ( appModel, appCmdMsg ) =
                                            App.update (Ctrl fwdMsg) t

                                        updateApps stuff =
                                            { stuff | apps = Bag.replace rideKey appModel stuff.apps }

                                        newModel =
                                            { model | worlds = Bag.update worldKey (Maybe.map updateApps) model.worlds }
                                    in
                                        ( newModel
                                        , Cmd.map (Send (ToApp (WorldKey worldKey (AppKey rideKey))) >> toWorldMsg (WorldKey worldKey ())) appCmdMsg
                                        )

                                Nothing ->
                                    ( model, Cmd.none )

                        Nothing ->
                            let
                                ( appModel, appCmdMsg ) =
                                    App.update (Ctrl fwdMsg) party.self

                                u newSelf party =
                                    { party | self = newSelf }

                                updateParties stuff =
                                    { stuff | parties = Bag.update partyKey (Maybe.map (u appModel)) stuff.parties }

                                newModel =
                                    { model | worlds = Bag.update worldKey (Maybe.map updateParties) model.worlds }
                            in
                                ( newModel
                                , Cmd.map (Send (ToParty (WorldKey worldKey (PartyKey partyKey))) >> toWorldMsg (WorldKey worldKey ())) appCmdMsg
                                )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


worldAnimate : WorldKey () -> Ground -> Time -> Multiverse a -> Multiverse a
worldAnimate (WorldKey worldKey ()) ground dt model =
    let
        updateApps stuff =
            { stuff | apps = Bag.map (App.animate ground dt) stuff.apps }
    in
        { model | worlds = Bag.update worldKey (Maybe.map updateApps) model.worlds }


worldJoin : WorldKey () -> Multiverse model -> ( Maybe (WorldKey PartyKey), Multiverse model, Cmd (WorldMsg msg) )
worldJoin (WorldKey worldKey ()) model =
    let
        -- freshParty : Stuff -> (Party, Cmd msg)
        freshParty stuff =
            let
                ( defaultSelfApp, defaultSelfCmd ) =
                    stuff.defaultSelf
            in
                ( { rideKey = Nothing
                  , self = defaultSelfApp
                  }
                , defaultSelfCmd
                )

        -- thisFreshParty : Maybe (Party, Cmd msg)
        thisFreshParty =
            Bag.get worldKey model.worlds
                |> Maybe.map freshParty

        -- insertParty : Party -> Bag Party -> ( WorldKey PartyKey, Bag Party )
        insertParty newParty parties =
            let
                ( partyKey, newParties ) =
                    Bag.insert newParty parties

                worldPartyKey =
                    WorldKey worldKey (PartyKey partyKey)
            in
                ( worldPartyKey, newParties )

        -- updateParties :
        --     (Bag Party -> (WorldKey PartyKey, Bag Party, Cmd msg))
        --     -> Stuff -> (WorldKey PartyKey, Stuff, Cmd msg)
        updateParties f stuff =
            let
                ( worldPartyKey, newParties ) =
                    f stuff.parties
            in
                ( worldPartyKey, { stuff | parties = newParties } )
    in
        case Bag.get worldKey model.worlds of
            Just stuff ->
                let
                    ( newParty, selfCmd ) =
                        freshParty stuff

                    ( worldPartyKey, newStuff ) =
                        updateParties (insertParty newParty) stuff

                    newModel =
                        { model | worlds = Bag.replace worldKey newStuff model.worlds }
                in
                    ( Just worldPartyKey
                    , newModel
                    , Cmd.map (Send (ToParty worldPartyKey) >> toWorldMsg (WorldKey worldKey ())) selfCmd
                    )

            Nothing ->
                ( Nothing, model, Cmd.none )


worldLeave : WorldKey PartyKey -> Multiverse a -> Multiverse a
worldLeave (WorldKey worldKey (PartyKey partyKey)) model =
    let
        updateParties f stuff =
            { stuff | parties = f stuff.parties }
    in
        { model | worlds = Bag.update worldKey (Maybe.map (updateParties (Bag.remove partyKey))) model.worlds }


worldChangeRide : WorldKey PartyKey -> Multiverse model -> ( Multiverse model, Cmd (WorldMsg msg) )
worldChangeRide (WorldKey worldKey (PartyKey partyKey)) model =
    let
        updateRide party =
            case ( party.rideKey, App.framing (PartyKey partyKey) party.self ) of
                ( Just (AppKey rideKey), _ ) ->
                    let
                        positioning x =
                            { position = x.position, orientation = x.orientation }

                        ridePos =
                            Maybe.andThen (App.framing (PartyKey partyKey)) (worldApp (WorldKey worldKey (AppKey rideKey)) model)
                                |> Maybe.map (.pov >> positioning)

                        cmd =
                            Cmd.map (Send (ToApp ((WorldKey worldKey (AppKey rideKey)))))
                                (Task.succeed (PartyKey partyKey) |> Task.perform (Ctrl << Leave))
                    in
                        ( { party
                            | rideKey = Nothing
                            , self = App.reposition ridePos party.self
                          }
                        , cmd
                        )

                ( Nothing, Just myFraming ) ->
                    let
                        myPos =
                            myFraming.pov.position

                        myDir =
                            Model.direction myFraming.pov

                        secondPosition ( k, app ) =
                            case App.framing (PartyKey partyKey) app of
                                Just framing ->
                                    Just ( k, framing.target.position )

                                Nothing ->
                                    Nothing

                        relativePosition appPos =
                            V3.sub appPos myPos

                        inFrontOf relPos =
                            V3.dot relPos myDir > 0

                        mClosestKey =
                            Bag.get worldKey model.worlds
                                |> Maybe.map .apps
                                |> Maybe.withDefault Bag.empty
                                |> Bag.toList
                                |> List.filterMap secondPosition
                                |> List.map (Tuple.mapSecond relativePosition)
                                |> List.filter (Tuple.second >> inFrontOf)
                                |> List.map (Tuple.mapSecond V3.lengthSquared)
                                |> List.filter (Tuple.second >> (\d -> d < 10 * 10))
                                |> List.sortBy Tuple.second
                                |> List.head
                                |> Maybe.map Tuple.first
                                |> Maybe.map AppKey

                        cmd =
                            case mClosestKey of
                                Just (AppKey rideKey) ->
                                    Cmd.map (Send (ToApp (WorldKey worldKey (AppKey rideKey))))
                                        (Task.succeed (PartyKey partyKey) |> Task.perform (Ctrl << Enter))

                                Nothing ->
                                    Cmd.none
                    in
                        ( { party | rideKey = mClosestKey }
                        , cmd
                        )

                _ ->
                    ( party, Cmd.none )
    in
        let
            mNewPartyCmds =
                Maybe.map updateRide <| worldParty (WorldKey worldKey (PartyKey partyKey)) model

            mNewParty =
                Maybe.map Tuple.first mNewPartyCmds

            newCmds =
                Maybe.map Tuple.second mNewPartyCmds
                    |> Maybe.withDefault Cmd.none

            updateParties stuff =
                { stuff | parties = Bag.update partyKey (always mNewParty) stuff.parties }
        in
            ( { model | worlds = Bag.update worldKey (Maybe.map updateParties) model.worlds }
            , newCmds
            )


worldId : WorldKey () -> Multiverse a -> Maybe String
worldId (WorldKey worldKey ()) model =
    Bag.get worldKey model.worlds
        |> Maybe.map .id


worldLabel : WorldKey () -> Multiverse a -> Maybe String
worldLabel (WorldKey worldKey ()) model =
    Bag.get worldKey model.worlds
        |> Maybe.map .label


worldPartyLabel : WorldKey PartyKey -> Multiverse a -> String
worldPartyLabel worldPartyKey model =
    let
        (WorldKey worldKey (PartyKey _)) =
            worldPartyKey

        none =
            "<>"
    in
        case worldParty worldPartyKey model of
            Just party ->
                case party.rideKey of
                    Just (AppKey appKey) ->
                        case worldApp (WorldKey worldKey (AppKey appKey)) model of
                            Just app ->
                                App.label app

                            Nothing ->
                                "Ride not found"

                    Nothing ->
                        App.label party.self

            Nothing ->
                "Party not found"


worldOverlay : WorldKey PartyKey -> Multiverse a -> Html (WorldMsg msg)
worldOverlay worldPartyKey model =
    let
        (WorldKey worldKey (PartyKey _)) =
            worldPartyKey

        none =
            Html.text "Welcome to DreamBuggy"
    in
        case worldParty worldPartyKey model of
            Just party ->
                case party.rideKey of
                    Just (AppKey appKey) ->
                        let
                            worldAppKey =
                                WorldKey worldKey (AppKey appKey)
                        in
                            case worldApp worldAppKey model of
                                Just app ->
                                    Html.map (Send (ToApp worldAppKey) >> toWorldMsg (WorldKey worldKey ())) (App.overlay app)

                                Nothing ->
                                    Html.text "App not found"

                    Nothing ->
                        Html.map (Send (ToParty worldPartyKey) >> toWorldMsg (WorldKey worldKey ())) (App.overlay party.self)

            Nothing ->
                Html.text "Party not found"


worldFraming : WorldKey PartyKey -> Multiverse a -> Maybe Framing
worldFraming worldPartyKey model =
    let
        (WorldKey worldKey partyKey) =
            worldPartyKey
    in
        case worldParty worldPartyKey model of
            Just party ->
                case party.rideKey of
                    Just (AppKey appKey) ->
                        case worldApp (WorldKey worldKey (AppKey appKey)) model of
                            Just app ->
                                App.framing partyKey app

                            Nothing ->
                                Nothing

                    Nothing ->
                        App.framing partyKey party.self

            Nothing ->
                Nothing


worldFocus : WorldKey AppKey -> Multiverse a -> Maybe Focus
worldFocus appKey model =
    case worldApp appKey model of
        Just app ->
            App.focus app

        _ ->
            Nothing


worldGround : WorldKey () -> Multiverse model -> Maybe Ground
worldGround (WorldKey worldKey ()) model =
    Bag.get worldKey model.worlds
        |> Maybe.andThen .maybeGround
