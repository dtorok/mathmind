module App exposing (..)

import Tuple
import Task
import Dom

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
  List.repeat 3 1
    |> List.indexedMap (\i _ -> index2id i)
    |> List.map Exercise.init
    |> List.unzip
    |> Tuple.mapFirst ExModel
    |> Tuple.mapSecond (batchCmd ExMsg)


-----
-- MESSAGES
type Msg
    = NoOp
    | ExMsg Int Exercise.Msg
    | NextExercise Int


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
            |> List.indexedMap (indexedExerciseUpdate index exMsg)
            |> List.unzip
            |> Tuple.mapFirst ExModel
            |> Tuple.mapSecond Cmd.batch
        (NextExercise index, ExModel exModels) ->
          Debug.log ("NextExercise" ++ (toString index)) (model, Cmd.none)
        (_, _) ->
            ( model, Cmd.none )

indexedExerciseUpdate : Int -> Exercise.Msg -> Int -> Exercise.Model -> (Exercise.Model, Cmd Msg)
indexedExerciseUpdate index exMsg i exModel =
  if i == index then
    case Exercise.update exMsg exModel of
      (m, c) ->
        (m,
        if Exercise.isCorrect m
          then Task.attempt (always NoOp) (Dom.focus (index2id (i + 1)))
          else Cmd.map (ExMsg i) c
        )
  else
    (exModel, Cmd.none)


-----
-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-----
-- HELPERS
index2id : Int -> String
index2id index = "input" ++ (toString index)


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
