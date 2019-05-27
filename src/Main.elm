module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy)
import Task
import Time
import Random


-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL
---- TODO: Maybe keep track of path but also use a dictionary of dictionaries which look up by x then by y and lazy build the path nodes by each x coordinate's dictionary


type alias Model =
    { step : Int
    , point : Point
    , path : List Point
    , distanceSeries : DistanceSeries
    }


type alias Point =
    { x : Int
    , y : Int
    }


type alias DistanceSeries =
    List ( Int, Float )


origin : Point
origin =
    Point 0 0


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0 origin [ origin ] [ ( 0, 0 ) ], Cmd.none )



-- UPDATE


type Msg
    = Tick Time.Posix
    | NextMove Move


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( model
            , Random.generate NextMove nextMove
            )

        NextMove move ->
            let
                newPoint =
                    nextPoint model.point move
            in
                ( { point = newPoint
                  , path = model.path ++ [ newPoint ]
                  , distanceSeries = nextDistanceSeries model
                  , step = model.step + 1
                  }
                , Cmd.none
                )


nextDistanceSeries : Model -> DistanceSeries
nextDistanceSeries model =
    if model.step < 1000 then
        model.distanceSeries ++ [ ( model.step, distanceTo model.point ) ]
    else
        model.distanceSeries


distanceTo : Point -> Float
distanceTo point =
    roundedDistance (sqrt (toFloat (point.x ^ 2 + point.y ^ 2)))


roundedDistance : Float -> Float
roundedDistance distance =
    (toFloat (round (distance * 100))) / 100


type Move
    = Up
    | Down
    | Left
    | Right


nextMove : Random.Generator Move
nextMove =
    Random.uniform Up [ Down, Left, Right ]


nextPoint : Point -> Move -> Point
nextPoint point move =
    case move of
        Up ->
            { point | y = point.y + 1 }

        Down ->
            { point | y = point.y - 1 }

        Left ->
            { point | x = point.x - 1 }

        Right ->
            { point | x = point.x + 1 }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    tickAction model.step


tickAction : Int -> Sub Msg
tickAction step =
    if step < 10000 then
        Time.every 1 Tick
    else
        Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text "x: "
        , text (String.fromInt model.point.x)
        , text ", y: "
        , text (String.fromInt model.point.y)
        , div [ class "walk-field" ] (walkField model)
        , lazy distanceSeriesView model.distanceSeries
        ]


distanceSeriesView : DistanceSeries -> Html Msg
distanceSeriesView distanceSeries =
    div [ class "distance-series" ]
        (List.foldr distanceView [] distanceSeries)



-- walker
-- bottom style: [y position] - [walker height]
-- left style: [x position]


walkField : Model -> List (Html Msg)
walkField model =
    let
        point =
            model.point
    in
        [ div [ class "x-axis" ] []
        , div [ class "y-axis" ] []
        , div [ class "walker-container" ]
            [ div
                [ class "walker"
                , style "left" (String.fromInt (3 * point.x) ++ "px")
                , style "bottom" (String.fromInt ((3 * point.y) - 5) ++ "px")
                ]
                []
            , div [ class "walker-path" ] (List.map walkerPoint model.path)
            ]
        ]


distanceView : ( Int, Float ) -> List (Html Msg) -> List (Html Msg)
distanceView ( step, distance ) list =
    list
        ++ [ span
                [ class "dot"
                , style "bottom" ((String.fromFloat (3 * distance)) ++ "px")
                , style "left" ((String.fromInt (1 * step)) ++ "px")
                ]
                []
           ]


walkerPoint : Point -> Html Msg
walkerPoint point =
    span
        [ class "dot"
        , style "bottom" ((String.fromInt (3 * point.y)) ++ "px")
        , style "left" ((String.fromInt (3 * point.x)) ++ "px")
        ]
        []
