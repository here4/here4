module Here4.World exposing (Attributes, Multiverse, create)

import Color exposing (Color)
import Dynamic exposing (Dynamic)
import Here4.App.Types exposing (..)
import Here4.App.Internal as App exposing (..)
import Here4.Bag as Bag exposing (Bag)
import Here4.Barrier exposing (Barrier, joinBarriers)
import Here4.Body exposing (Body)
import Here4.Camera.Types exposing (Framing, Shot)
import Here4.Control exposing (..)
import Here4.Dispatch exposing (..)
import Here4.Ground exposing (Ground)
import Here4.Location exposing (..)
import Here4.Model as Model exposing (GlobalMsg, NavigatorMsg, WorldKey(..), AppKey(..), PartyKey(..))
import Here4.Navigator.Control exposing (NavMsg)
import Here4.Orientation as Orientation exposing (Orientation)
import Here4.Space as Space
import Html exposing (Html)
import List.Extra as List
import Math.Vector3 as V3 exposing (vec3)
import Maybe.Extra as Maybe exposing (isJust)
import Task
import Time exposing (Time)


type alias Attributes =
    { id : String
    , label : String
    , backgroundColor : Color
    , apps : List ( App, Cmd AppMsg )
    , defaultSelf : ( App, Cmd AppMsg )
    }


type alias Multiverse state =
    { state : state
    , worlds : Bag World
    }


type alias World =
    { id : String
    , label : String
    , backgroundColor : Color
    , maybeGround : Maybe Ground
    , barriers : List Barrier
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
    (flags -> ( model, Cmd (NavMsg msg) ))
    -> (NavMsg msg -> model -> ( model, Cmd (NavMsg msg) ))
    -> (Multiverse model -> Sub (NavMsg msg))
    -> List Attributes
    -> Program flags (Model.Model (Multiverse model) (WorldMsg (NavMsg msg))) (Model.Msg (WorldMsg (NavMsg msg)))
create hubInit hubUpdate navSubscriptions attributes =
    Space.programWithFlags navSubscriptions
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


worldAddApps : WorldKey () -> List ( App, Cmd AppMsg ) -> Bag App -> ( Bag App, List (Cmd (WorldMsg msg)) )
worldAddApps (WorldKey worldKey ()) appsList apps0 =
    let
        response appKey x =
            case x of
                Effect e ->
                    HubEff (toWorldEffect (WorldKey worldKey ()) (Just appKey) e)

                m ->
                    let
                        key =
                            ToApp (WorldKey worldKey appKey)
                    in
                        toWorldMsg (WorldKey worldKey ()) (Just appKey) (Send key m)

        f ( newApp, newCmdMsg ) ( oldBag, oldCmdMsgs ) =
            let
                ( appKey, newBag ) =
                    Bag.insert newApp oldBag
            in
                ( newBag, Cmd.map (response (AppKey appKey)) newCmdMsg :: oldCmdMsgs )

        ( appsBag, worldCmds ) =
            List.foldl f ( apps0, [] ) appsList
    in
        ( appsBag, worldCmds )


oneWorldInit :
    Attributes
    -> ( Bag World, List (Cmd (WorldMsg msg)) )
    -> ( Bag World, List (Cmd (WorldMsg msg)) )
oneWorldInit attributes ( oldWorlds, oldCmds ) =
    let
        emptyWorld =
            { id = attributes.id
            , label = attributes.label
            , backgroundColor = attributes.backgroundColor
            , maybeGround = Nothing
            , barriers = []
            , apps = Bag.empty
            , parties = Bag.empty
            , defaultSelf = attributes.defaultSelf
            }

        ( worldKey, oneWorlds ) =
            Bag.insert emptyWorld oldWorlds

        ( appsBag, appCmds ) =
            worldAddApps (WorldKey worldKey ()) attributes.apps Bag.empty

        updateApps world =
            { world | apps = appsBag }

        newWorlds =
            Bag.update worldKey (Maybe.map updateApps) oneWorlds
    in
        ( newWorlds, oldCmds ++ appCmds )


worldInit :
    (flags -> ( model, Cmd msg ))
    -> List Attributes
    -> flags
    -> ( Multiverse model, Cmd (WorldMsg msg) )
worldInit hubInit attributes flags =
    let
        ( hubModel, hubCmd ) =
            hubInit flags

        ( worldsBag, worldsCmds ) =
            List.foldl oneWorldInit ( Bag.empty, [] ) attributes
    in
        ( { state = hubModel
          , worlds = worldsBag
          }
        , Cmd.batch (Cmd.map Hub hubCmd :: worldsCmds)
        )


worldView : WorldKey () -> Multiverse model -> Maybe Model.World
worldView (WorldKey worldKey ()) model =
    let
        mWorld =
            Bag.get worldKey model.worlds
    in
        case Maybe.andThen .maybeGround mWorld of
            Nothing ->
                Nothing

            Just ground ->
                Maybe.map (makeWorld ground) mWorld


makeWorld : Ground -> World -> Model.World
makeWorld ground world =
    let
        partyBodies party =
            case party.rideKey of
                Just _ ->
                    always []

                Nothing ->
                    bodies party.self

        worldBodies pos =
            List.concatMap (\app -> (bodies app) pos) (Bag.items world.apps)
                ++ List.concatMap (\app -> (partyBodies app) pos) (Bag.items world.parties)
    in
        { backgroundColor = world.backgroundColor
        , bodies = worldBodies
        , ground = ground
        }


toWorldEffect : WorldKey () -> Maybe AppKey -> EffectMsg () () -> EffectMsg (WorldKey ()) (Maybe AppKey)
toWorldEffect worldKey appKey e =
    case e of
        UpdateGround () ground ->
            UpdateGround worldKey ground

        AddBarrier () barrier ->
            AddBarrier worldKey barrier

        RelocateParty () partyKey location ->
            RelocateParty worldKey partyKey location

        AddApp () app ->
            AddApp worldKey app

        RemoveApp () () ->
            RemoveApp worldKey appKey


toWorldMsg :
    WorldKey ()
    -> Maybe AppKey
    -> DispatchHub Route (EffectMsg () ()) Msg Dynamic GlobalMsg NavigatorMsg a
    -> DispatchHub Route (EffectMsg (WorldKey ()) (Maybe AppKey)) Msg Dynamic GlobalMsg NavigatorMsg a
toWorldMsg worldKey appKey msg =
    let
        toWorldDispatch d =
            case d of
                Self nodeMsg ->
                    Self nodeMsg

                Ctrl ctrlMsg ->
                    Ctrl ctrlMsg

                Effect e ->
                    Effect (toWorldEffect worldKey appKey e)
    in
        case msg of
            Hub hubMsg ->
                Hub hubMsg

            Send key dispatch ->
                Send key (toWorldDispatch dispatch)

            Forward key ctrlMsg ->
                Forward key ctrlMsg

            HubEff e ->
                HubEff (toWorldEffect worldKey appKey e)

            GlobalEffect globalMsg ->
                GlobalEffect globalMsg

            NavEffect navMsg ->
                NavEffect navMsg


toAppMsg : Dispatch (EffectMsg (WorldKey ()) appKey) Msg Dynamic -> AppMsg
toAppMsg dispatch =
    let
        toAppEffect e =
            case e of
                UpdateGround _ ground ->
                    UpdateGround () ground

                AddBarrier _ barrier ->
                    AddBarrier () barrier

                RelocateParty _ partyKey location ->
                    RelocateParty () partyKey location

                AddApp _ app ->
                    AddApp () app

                RemoveApp _ _ ->
                    RemoveApp () ()
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
        (WorldKey worldKey (PartyKey partyKey)) =
            worldPartyKey

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

        updateParties world =
            { world | parties = Bag.update partyKey (always mNewParty) world.parties }
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
    (NavMsg msg -> model -> ( model, Cmd (NavMsg msg) ))
    -> WorldMsg (NavMsg msg)
    -> Multiverse model
    -> ( Multiverse model, Cmd (WorldMsg (NavMsg msg)) )
worldUpdate hubUpdate msg model =
    case msg of
        Hub hubMsg ->
            let
                ( hubModel, hubCmd ) =
                    hubUpdate hubMsg model.state

                response m =
                    case m of
                        Effect e ->
                            NavEffect e

                        _ ->
                            Hub m
            in
                ( { model | state = hubModel }, Cmd.map response hubCmd )

        HubEff (UpdateGround (WorldKey worldKey ()) ground) ->
            let
                updateGround world =
                    { world | maybeGround = Just ground }
            in
                ( { model | worlds = Bag.update worldKey (Maybe.map updateGround) model.worlds }
                , Cmd.none
                )

        HubEff (AddBarrier (WorldKey worldKey ()) barrier) ->
            let
                updateBarriers world =
                    { world | barriers = barrier :: world.barriers }
            in
                ( { model | worlds = Bag.update worldKey (Maybe.map updateBarriers) model.worlds }
                , Cmd.none
                )

        HubEff (RelocateParty (WorldKey worldKey ()) (PartyKey partyKey) location) ->
            relocate (WorldKey worldKey (PartyKey partyKey)) location model


        HubEff (AddApp (WorldKey worldKey ()) app) ->
            let
                addApp world =
                    let
                        ( newApps, newCmdMsgs ) =
                            worldAddApps (WorldKey worldKey ()) [app] world.apps
                    in
                        ( { world | apps = newApps }
                        , Cmd.batch newCmdMsgs
                        )

                replaceWorld w =
                    Bag.replace worldKey w model.worlds

                updateModel ws =
                    { model | worlds = ws }

            in
                Bag.get worldKey model.worlds
                |> Maybe.map
                    ( addApp >> Tuple.mapFirst (replaceWorld >> updateModel))
                |> Maybe.withDefault (model, Cmd.none)


        HubEff (RemoveApp (WorldKey worldKey ()) (Just (AppKey appKey))) ->
            let
                updateApps f world =
                    { world | apps = f world.apps }
                newModel =
                    { model | worlds = Bag.update worldKey (Maybe.map (updateApps (Bag.remove appKey))) model.worlds }
            in
                ( newModel, Cmd.none )


        Send key appMsg ->
            let
                ( mApp, worldKey, updateModel ) =
                    case key of
                        ToApp (WorldKey worldKey (AppKey appKey)) ->
                            ( worldApp (WorldKey worldKey (AppKey appKey)) model
                            , worldKey
                            , \newApp ->
                                let
                                    updateApps world =
                                        { world | apps = Bag.replace appKey newApp world.apps }
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
                                        updateParties world =
                                            { world | parties = Bag.update partyKey (Maybe.map (u newSelf)) world.parties }
                                    in
                                        { model | worlds = Bag.update worldKey (Maybe.map updateParties) model.worlds }
                                )

                response x =
                    case x of
                        Effect e ->
                            HubEff (toWorldEffect (WorldKey worldKey ()) Nothing e)

                        m ->
                            toWorldMsg (WorldKey worldKey ()) Nothing (Send key m)
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

                                        updateApps world =
                                            { world | apps = Bag.replace rideKey appModel world.apps }

                                        newModel =
                                            { model | worlds = Bag.update worldKey (Maybe.map updateApps) model.worlds }
                                    in
                                        ( newModel
                                        , Cmd.map (Send (ToApp (WorldKey worldKey (AppKey rideKey))) >> toWorldMsg (WorldKey worldKey ()) Nothing) appCmdMsg
                                        )

                                Nothing ->
                                    ( model, Cmd.none )

                        Nothing ->
                            let
                                ( appModel, appCmdMsg ) =
                                    App.update (Ctrl fwdMsg) party.self

                                u newSelf party =
                                    { party | self = newSelf }

                                updateParties world =
                                    { world | parties = Bag.update partyKey (Maybe.map (u appModel)) world.parties }

                                newModel =
                                    { model | worlds = Bag.update worldKey (Maybe.map updateParties) model.worlds }
                            in
                                ( newModel
                                , Cmd.map (Send (ToParty (WorldKey worldKey (PartyKey partyKey))) >> toWorldMsg (WorldKey worldKey ()) Nothing) appCmdMsg
                                )

                Nothing ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


worldAnimate : WorldKey () -> Ground -> Time -> Multiverse model
    -> ( Multiverse model, Cmd (WorldMsg (NavMsg msg)) )
worldAnimate (WorldKey worldKey ()) ground dt model =
    let
        response appKey x =
            case x of
                Effect e ->
                    HubEff (toWorldEffect (WorldKey worldKey ()) (Just appKey) e)

                m ->
                    let
                        key =
                            ToApp (WorldKey worldKey appKey)
                    in
                        toWorldMsg (WorldKey worldKey ()) (Just appKey) (Send key m)

        animate app ( apps, appCmds ) =
            let
                ( newApp, appCmd ) =
                    App.animate ground dt app

                ( appKey, newBag ) =
                    Bag.insert newApp apps
            in
                ( newBag, Cmd.map (response (AppKey appKey)) appCmd :: appCmds )
       
        updateApps world =
            let
                ( newApps, newAppCmds ) =
                    Bag.foldl animate ( Bag.empty, [] ) world.apps
            in
                ( { world | apps = newApps }, Cmd.batch newAppCmds )

        replaceWorld w =
            Bag.replace worldKey w model.worlds

        updateModel ws =
            { model | worlds = ws }
    in
        Bag.get worldKey model.worlds
        |> Maybe.map
            ( updateApps >> Tuple.mapFirst (replaceWorld >> updateModel))
        |> Maybe.withDefault (model, Cmd.none)


worldJoin : WorldKey () -> Multiverse model -> ( Maybe (WorldKey PartyKey), Multiverse model, Cmd (WorldMsg msg) )
worldJoin (WorldKey worldKey ()) model =
    let
        -- freshParty : World -> (Party, Cmd msg)
        freshParty world =
            let
                ( defaultSelfApp, defaultSelfCmd ) =
                    world.defaultSelf
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
        --     -> World -> (WorldKey PartyKey, World, Cmd msg)
        updateParties f world =
            let
                ( worldPartyKey, newParties ) =
                    f world.parties
            in
                ( worldPartyKey, { world | parties = newParties } )
    in
        case Bag.get worldKey model.worlds of
            Just world ->
                let
                    ( newParty, selfCmd ) =
                        freshParty world

                    ( worldPartyKey, newWorld ) =
                        updateParties (insertParty newParty) world

                    newModel =
                        { model | worlds = Bag.replace worldKey newWorld model.worlds }
                in
                    ( Just worldPartyKey
                    , newModel
                    , Cmd.map (Send (ToParty worldPartyKey) >> toWorldMsg (WorldKey worldKey ()) Nothing) selfCmd
                    )

            Nothing ->
                ( Nothing, model, Cmd.none )


worldLeave : WorldKey PartyKey -> Multiverse a -> Multiverse a
worldLeave (WorldKey worldKey (PartyKey partyKey)) model =
    let
        updateParties f world =
            { world | parties = f world.parties }
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

            updateParties world =
                { world | parties = Bag.update partyKey (always mNewParty) world.parties }
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
                                    Html.map (Send (ToApp worldAppKey) >> toWorldMsg (WorldKey worldKey ()) Nothing) (App.overlay app)

                                Nothing ->
                                    Html.text "App not found"

                    Nothing ->
                        Html.map (Send (ToParty worldPartyKey) >> toWorldMsg (WorldKey worldKey ()) Nothing) (App.overlay party.self)

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
    let
        groundWithBarriers : World -> Maybe Ground
        groundWithBarriers world =
            let
                withBarriers ground =
                    { ground | barrier = joinBarriers (ground.barrier :: world.barriers)
                    }
            in
                Maybe.map withBarriers world.maybeGround
    in
        Bag.get worldKey model.worlds
        |> Maybe.andThen groundWithBarriers
