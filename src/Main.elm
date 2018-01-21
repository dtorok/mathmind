module Main exposing (..)

import Html exposing (Html, div, text, program, node, img, table, tr, td)

import ListGame
import ImageGame

type Game
  = ListGame
  | ImageGame

type Model
  = ListGameModel ListGame.Model
  | ImageGameModel ImageGame.Model

type Msg
  = ListGameMsg ListGame.Msg
  | ImageGameMsg ImageGame.Msg

mapTuple : (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapTuple f1 f2 (a1, a2) =
  (f1 a1, f2 a2)

init : Game -> (Model, Cmd Msg)
init game =
  case game of
    ListGame ->
      mapTuple ListGameModel (Cmd.map ListGameMsg) ListGame.init
    ImageGame ->
      mapTuple ImageGameModel (Cmd.map ImageGameMsg) ImageGame.initGame

view : Model -> Html Msg
view model =
  case model of
    ListGameModel model ->
      Html.map ListGameMsg <| (ListGame.view model)
    ImageGameModel model ->
      Html.map ImageGameMsg <| (ImageGame.view model)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model) of
    (ListGameMsg msg, ListGameModel model) ->
      mapTuple ListGameModel (Cmd.map ListGameMsg) (ListGame.update msg model)
    (ImageGameMsg msg, ImageGameModel model) ->
      mapTuple ImageGameModel (Cmd.map ImageGameMsg) (ImageGame.update msg model)
    _ ->
      (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    ListGameModel model ->
      Sub.none
    ImageGameModel model ->
      Sub.map ImageGameMsg <| ImageGame.subscriptions model

main : Program Never Model Msg
main =
    program
        { init = init ImageGame
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
