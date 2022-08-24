module Lunarlander exposing (..)

import Browser
import Browser.Events exposing (onAnimationFrame, onAnimationFrameDelta, onKeyDown, onKeyUp)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode
import Round
import Time


main =
    Browser.element { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions : GameModel -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrame Tock
        , onAnimationFrameDelta Tick
        , onKeyDown (keyDecoder toKeyDown)
        , onKeyUp (keyDecoder toKeyUp)
        ]


keyDecoder : (String -> Msg) -> Decode.Decoder Msg
keyDecoder toDir =
    Decode.map toDir (Decode.field "key" Decode.string)


toKeyDown : String -> Msg
toKeyDown key =
    KeyDown key


toKeyUp : String -> Msg
toKeyUp key =
    KeyUp key


type Msg
    = Tick Float
    | Tock Time.Posix
    | KeyDown String
    | KeyUp String


type alias Spaceship =
    { x : Float -- m
    , y : Float -- m
    , verticalSpeed : Float -- m/s
    , horizontalSpeed : Float -- m/s
    , height : Float
    , thrust : Float -- m/s
    , tilt : Float -- degrees
    , tiltSpeed : Float -- degrees/s
    , fuel : Float -- kg
    }


type GameStatus
    = Playing
    | Lost


type alias Keylog =
    { up : Bool, down : Bool, left : Bool, right : Bool }


type alias LandingArea =
    { x : Float
    , y : Float
    , width : Float
    , score: Int
    }


type alias GameModel =
    { height : Float -- meters
    , width : Float -- meters
    , spaceship : Spaceship
    , status : GameStatus
    , viewportHeight : Float -- pixels
    , viewportWidth : Float -- pixels
    , g : Float
    , delta : Float
    , keylog : Keylog
    , landingAreas : List LandingArea
    }


init : () -> ( GameModel, Cmd Msg )
init _ =
    ( GameModel 200 200 (Spaceship 150 150 0 0 10 2 0 90 100) Playing 500 500 1.62 16 (Keylog False False False False) [ LandingArea 50 10 30 10], Cmd.none )


view : GameModel -> Html Msg
view model =
    div
        [ class "view"
        , style "width" (String.fromFloat model.viewportWidth)
        , style "height" (String.fromFloat model.viewportHeight)
        ]
        ([ drawSpaceship (transposeSpaceship model)
         , drawInfoBox model.spaceship
        ] ++ drawLandingAreas model)

drawInfoBox : Spaceship -> Html Msg
drawInfoBox spaceship =
    div [ class "info" ]
        [ p [] [ text ("Vertical Speed: " ++ Round.round 2 spaceship.verticalSpeed ++ " m/s") ]
        , p [] [ text ("Horizontal Speed: " ++ Round.round 2 spaceship.horizontalSpeed ++ " m/s") ]
        , p [] [ text ("Altitude: " ++ Round.round 2 spaceship.y ++ " m") ]
        , p [] [ text ("tilt: " ++ Round.round 2 spaceship.tilt ++ " °") ]
        , p [] [ text ("fuel: " ++ Round.round 2 spaceship.fuel ++ " kg") ]
        ]

drawLandingAreas : GameModel -> List (Html Msg)
drawLandingAreas model =
    model.landingAreas
    |> List.map (transposeLandingArea model)
    |> List.map drawLandingArea


drawLandingArea : LandingArea -> Html Msg
drawLandingArea landingArea =
    div [ class "landing-area"
        , style "width" (Round.round 0 landingArea.width)
        , style "top" (Round.round 0 landingArea.y)
        , style "left" (Round.round 0 landingArea.x)
        ] []


transposeLandingArea : GameModel -> LandingArea -> LandingArea
transposeLandingArea model landingArea =
    {landingArea | y = (model.height - landingArea.y) * (model.viewportHeight / model.height)
                 , x = (model.width - landingArea.x) * (model.viewportWidth / model.width)}


transposeSpaceship : GameModel -> Spaceship
transposeSpaceship model =
    let
        spaceship =
            model.spaceship
    in
    { spaceship | y = (model.height - spaceship.y) * (model.viewportHeight / model.height)
                , x = (model.width - spaceship.x) * (model.viewportWidth / model.width)}


drawSpaceship : Spaceship -> Html Msg
drawSpaceship spaceship =
    div
        [ class "spaceship"
        , style "top" (Round.round 2 spaceship.y)
        , style "left" (Round.round 2 spaceship.x)
        , style "width" (String.fromFloat spaceship.height)
        , style "height" (String.fromFloat spaceship.height)
        , style "transform" (rotate spaceship.tilt)
        ]
        []


rotate : Float -> String
rotate degree =
    "rotate(" ++ Round.round 2 degree ++ "deg)"


update : Msg -> GameModel -> ( GameModel, Cmd Msg )
update msg model =
    case model.status of
        Playing ->
            case msg of
                Tick delta ->
                    if model.spaceship.y < 0 || model.spaceship.fuel < 0 then
                        ( { model | status = Lost }, Cmd.none )

                    else
                        ( { model
                            | spaceship = updateSpaceship model.spaceship model (delta / 1000)
                            , delta = delta
                          }
                        , Cmd.none
                        )

                KeyDown key ->
                    ( { model | keylog = keyDown key model.keylog }, Cmd.none )

                KeyUp key ->
                    ( { model | keylog = keyUp key model.keylog }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Lost ->
            ( model, Cmd.none )


keyDown : String -> Keylog -> Keylog
keyDown key keylog =
    case key of
        "ArrowUp" ->
            { keylog | up = True }

        "ArrowDown" ->
            { keylog | down = True }

        "ArrowLeft" ->
            { keylog | left = True }

        "ArrowRight" ->
            { keylog | right = True }

        _ ->
            keylog


keyUp : String -> Keylog -> Keylog
keyUp key keylog =
    case key of
        "ArrowUp" ->
            { keylog | up = False }

        "ArrowDown" ->
            { keylog | down = False }

        "ArrowLeft" ->
            { keylog | left = False }

        "ArrowRight" ->
            { keylog | right = False }

        _ ->
            keylog


tilt : Keylog -> Float -> Spaceship -> Spaceship
tilt keylog delta spaceship =
    if keylog.left then
        { spaceship | tilt = spaceship.tilt - (spaceship.tiltSpeed * delta) }

    else if keylog.right then
        { spaceship | tilt = spaceship.tilt + (spaceship.tiltSpeed * delta) }

    else
        spaceship


thrustSpaceship : Keylog -> Float -> Spaceship -> Spaceship
thrustSpaceship keylog delta spaceship =
    if keylog.up then
        { spaceship
            | verticalSpeed = spaceship.verticalSpeed - (spaceship.thrust * delta * Basics.sin (degrees (spaceship.tilt + 90)))
            , horizontalSpeed = spaceship.horizontalSpeed - (spaceship.thrust * delta * Basics.cos (degrees (spaceship.tilt + 90)))
            , fuel = spaceship.fuel - 0.1
        }

    else
        spaceship


updateSpaceship : Spaceship -> GameModel -> Float -> Spaceship
updateSpaceship spaceship model delta =
    spaceship
        |> tilt model.keylog 0.1
        |> thrustSpaceship model.keylog 0.1
        |> applyGravityOnSpaceship model.g delta
        |> moveSpaceship delta


applyGravityOnSpaceship : Float -> Float -> Spaceship -> Spaceship
applyGravityOnSpaceship g delta spaceship =
    { spaceship | verticalSpeed = spaceship.verticalSpeed + (g * delta) }


moveSpaceship : Float -> Spaceship -> Spaceship
moveSpaceship delta spaceship =
    { spaceship
        | y = spaceship.y - (spaceship.verticalSpeed * delta)
        , x = spaceship.x - (spaceship.horizontalSpeed * delta)
    }
