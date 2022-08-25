module Lunarlander exposing (..)

import Browser
import Browser.Dom exposing (Viewport)
import Browser.Events exposing (onAnimationFrame, onAnimationFrameDelta, onKeyDown, onKeyUp, onResize)
import Canvas exposing (Renderable, Shape, rect, shapes)
import Canvas.Settings exposing (fill)
import Canvas.Settings.Advanced exposing (Transform, scale, transform, translate)
import Canvas.Texture as Texture exposing (Texture)
import Color exposing (Color)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode
import Round
import Task exposing (Task)
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
        , onResize (\w h -> WindowResize (Debug.log "resize" ( w, h )))
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
    | WindowResize ( Int, Int )
    | TextureLoaded (Maybe Texture)


type GameStatus
    = Playing
    | Lost


type alias Keylog =
    { up : Bool, down : Bool, left : Bool, right : Bool }


type alias LandingArea =
    { x : Float
    , y : Float
    , width : Float
    , score : Int
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


type alias Spaceship =
    { x : Float -- m
    , y : Float -- m
    , verticalSpeed : Float -- m/s
    , horizontalSpeed : Float -- m/s
    , height : Float -- m
    , width : Float -- m
    , thrust : Float -- m/s
    , tilt : Float -- degrees
    , tiltSpeed : Float -- degrees/s
    , fuel : Float -- kg
    , texture : Maybe Texture
    }


init : () -> ( GameModel, Cmd Msg )
init _ =
    ( GameModel 200 200 (Spaceship 100 190 0 0 10 10 2 0 90 100 Maybe.Nothing) Playing 500 500 1.62 16 (Keylog False False False False) [ LandingArea 50 10 30 10 ]
    , Task.attempt handleGetViewport Browser.Dom.getViewport
    )


handleGetViewport : Result x Viewport -> Msg
handleGetViewport x =
    case x of
        Err err ->
            WindowResize ( 500, 500 )

        Ok viewport ->
            WindowResize ( round viewport.viewport.width, round viewport.viewport.height )


textures : List (Texture.Source Msg)
textures =
    [ Texture.loadFromImageUrl "./assets/lunar-module.svg" TextureLoaded ]


view : GameModel -> Html Msg
view model =
    div []
        [ Canvas.toHtmlWith { width = round model.viewportWidth, height = round model.viewportHeight, textures = textures }
            [ style "display" "block" ]
            (renderBackground model ++ [ renderSpaceship (transposeSpaceship model) model ])
        , drawInfoBox model.spaceship
        ]


renderBackground : GameModel -> List Renderable
renderBackground model =
    [ blackBackground model
    , stars model 0 0
    , stars model 600 0
    , stars model 0 600
     ]


blackBackground model =
    shapes [ fill Color.black ] [ Canvas.rect ( 0, 0 ) model.viewportWidth model.viewportHeight ]

stars: GameModel -> Float -> Float -> Renderable
stars model offsetX offsetY =
    let
        y = offsetX - (model.height - model.spaceship.y) * (model.viewportHeight / model.height)
        x = offsetY - (model.width - model.spaceship.x) * (model.viewportWidth / model.width) + 150 * (model.viewportWidth / model.width)
    in
    shapes [ transform [translate x y], fill Color.white ]
        [ Canvas.circle ( 100, 100 ) 1
        , Canvas.circle ( 200, 200 ) 1
        , Canvas.circle ( 300, 300 ) 1
        , Canvas.circle ( 400, 400 ) 1
        , Canvas.circle ( 500, 500 ) 1
        , Canvas.circle ( 600, 600 ) 1
        ]


drawInfoBox : Spaceship -> Html Msg
drawInfoBox spaceship =
    div [ class "info" ]
        [ p [] [ text ("Vertical Speed: " ++ Round.round 2 spaceship.verticalSpeed ++ " m/s") ]
        , p [] [ text ("Horizontal Speed: " ++ Round.round 2 spaceship.horizontalSpeed ++ " m/s") ]
        , p [] [ text ("Altitude: " ++ Round.round 2 spaceship.y ++ " m") ]
        , p [] [ text ("tilt: " ++ Round.round 2 spaceship.tilt ++ " °") ]
        , p [] [ text ("fuel: " ++ Round.round 2 spaceship.fuel ++ " kg") ]
        ]


transposeSpaceship : GameModel -> Spaceship
transposeSpaceship model =
    let
        spaceship =
            model.spaceship
    in
    { spaceship
        | y = ((model.height - spaceship.y) * (model.viewportHeight / model.height))
                |> Basics.max 100
                |> Basics.min (model.viewportHeight - 100)

        , x = (model.width - spaceship.x) * (model.viewportWidth / model.width)
                |> Basics.max 100
                |> Basics.min (model.viewportWidth - 100)

        , width = spaceship.width * (model.viewportWidth / model.width)
        , height = spaceship.height * (model.viewportHeight / model.height)
    }


renderSpaceship : Spaceship -> GameModel -> Renderable
renderSpaceship spaceship model =
    case spaceship.texture of
        Just texture ->
            let
                scaleRatio =
                    0.05

                t =
                    Texture.sprite { x = 0, y = 0, width = model.viewportHeight, height = model.viewportHeight } texture

                dim =
                    Texture.dimensions t
            in
            Canvas.texture
                [ transform
                    [ translate spaceship.x spaceship.y
                    , scale scaleRatio scaleRatio
                    , translate (dim.width / 2) (dim.height / 2)
                    , Canvas.Settings.Advanced.rotate (degrees spaceship.tilt)
                    , translate -(dim.width / 2) -(dim.height / 2)
                    ]
                ]
                ( 0, 0 )
                t

        Nothing ->
            shapes
                [ transform
                    [ translate spaceship.x spaceship.y
                    , translate (spaceship.width / 2) (spaceship.height / 2)
                    , Canvas.Settings.Advanced.rotate (degrees spaceship.tilt)
                    , translate -(spaceship.width / 2) -(spaceship.height / 2)
                    ]
                , Canvas.Settings.stroke (Color.rgb 1 1 1)
                ]
                [ rect ( 0, 0 ) spaceship.width spaceship.height ]


update : Msg -> GameModel -> ( GameModel, Cmd Msg )
update msg ({ spaceship } as model) =
    case model.status of
        Playing ->
            case msg of
                Tick delta ->
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

                TextureLoaded texture ->
                    case texture of
                        Just t ->
                            ( { model | spaceship = { spaceship | texture = Just (Texture.sprite { x = 0, y = 0, width = model.viewportWidth, height = model.viewportHeight } t) } }, Cmd.none )

                        Nothing ->
                            ( { model | spaceship = { spaceship | texture = texture } }, Cmd.none )

                Tock _ ->
                    ( model, Cmd.none )

                WindowResize ( width, height ) ->
                    ( { model | viewportHeight = toFloat height, viewportWidth = toFloat width }, Cmd.none )

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
