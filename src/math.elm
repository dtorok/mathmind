module App exposing (..)

import Tuple

import Html exposing (Html, div, text, program, node)
import Html.Attributes exposing (attribute)

import Exercise


-----
-- MODEL
type Model =
    ExModel (List Exercise.Model)

convCmd : (modelA -> modelB) -> (cmdA -> cmdB) -> (modelA, Cmd cmdA) -> (modelB, Cmd cmdB)
convCmd fModel fCmd (m, cmdA) =
  (fModel m, Cmd.map fCmd cmdA)

batchCmd : (Int -> msgA -> msgB) -> List (Cmd msgA) -> Cmd msgB
batchCmd fMsg cmds = cmds
  |> List.indexedMap (\ i cmd -> Cmd.map (fMsg i) cmd)
  |> Cmd.batch

init : ( Model, Cmd Msg )
init =
  List.repeat 3 Exercise.init
    |> List.unzip
    |> Tuple.mapFirst ExModel
    |> Tuple.mapSecond (batchCmd ExMsg)


-----
-- MESSAGES
type Msg
    = NoOp
    | ExMsg Int Exercise.Msg


-----
-- VIEW
view : Model -> Html Msg
view model =
  let content =
    case model of
      ExModel exModels -> List.indexedMap viewExercise exModels
  in
    div []
    [ stylesheet
    , div [] content ]

viewExercise : Int -> Exercise.Model -> Html Msg
viewExercise index model =
  let html = Exercise.view model
  in Html.map (ExMsg index) html

stylesheet : Html msg
stylesheet =
    let
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      "css.css"
            ]
    in
        node "link" attrs []


-----
-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (msg, model) of
        (ExMsg index exMsg, ExModel exModels) ->
          exModels
            |> List.indexedMap (\ i exModel ->
              if i == index then
                Exercise.update exMsg exModel
              else
                (exModel, Cmd.none)
              )
            |> List.unzip
            |> Tuple.mapFirst ExModel
            |> Tuple.mapSecond (batchCmd ExMsg)
        (_, _) ->
            ( model, Cmd.none )


-----
-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-----
-- MAIN
main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
