module App exposing (..)
import Task
import Dom
import Html exposing (Html, div, text, program, node, img, table, tr, td)
import Html.Attributes exposing (attribute, class, src, width, height)
import Html.Events exposing (onClick)

import Exercise


-----
-- DATABASE
images : List Image
images = []


-----
-- MODELS
type alias Coord = (Int, Int)
type alias Board = List (List Cell)
type Cell = Visible | Hidden
type alias Image =
  { url: String
  , width: Int
  , height: Int
  }

type alias Model =
  { imgUrl: String
  , width: Int
  , height: Int
  , rows: Int
  , cols: Int
  , chosen: Maybe Coord
  , exModel: Maybe Exercise.Model
  , board: Board
  }

init : String -> Int -> Int -> (Model, Cmd Msg)
init imgUrl width height =
  let
    rows = 4
    cols = 5
    model =
      { imgUrl = imgUrl
      , width = width
      , height = height
      , rows = rows
      , cols = cols
      , chosen = Nothing
      , exModel = Nothing
      , board = List.repeat rows <| List.repeat cols Hidden
      }
  in
    (model, Cmd.none)

makeVisible : Board -> Maybe Coord -> Board
makeVisible board maybeCoord =
  case maybeCoord of
    Nothing -> board
    Just (r, c) ->
      (flip List.indexedMap) board
        (\br row ->
          (flip List.indexedMap) row
            (\bc cell ->
              if (r == br && c == bc) then
                Visible
              else
                cell
            )
        )

-----
-- MESSAGES
type Msg
  = NoOp
  | Choose Coord
  | ExMsg Exercise.Msg


-----
-- VIEW
view : Model -> Html Msg
view model =
  div [class "puzzle"]
    [ stylesheet
    , viewImage model
    , viewTable model
    , viewExercise model
    ]

viewImage : Model -> Html Msg
viewImage model =
  img [ src model.imgUrl ] []

viewTable : Model -> Html Msg
viewTable model =
  let
    getClass cell = case cell of
      Visible -> "visible"
      Hidden -> "hidden"

    getChosenClass chosen r c = case chosen of
      Nothing -> ""
      Just (cr, cc) -> if (r == cr && c == cc) then "chosen" else ""

  in
    table [width model.width, height model.height] <|
      (flip List.indexedMap) model.board
        (\r row ->
          tr [] <|
            (flip List.indexedMap) row
              (\c cell ->
                td [class (getClass cell), class (getChosenClass model.chosen r c), onClick (Choose (r, c))] []
              )
        )

viewExercise : Model -> Html Msg
viewExercise model =
  case model.exModel of
    Nothing -> text ""
    Just exModel ->
      let exHtml = Exercise.view exModel
      in Html.map ExMsg exHtml


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
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp -> (model, Cmd.none)
    Choose coord ->
      let
        domId = "exercise"
        (exModel, exCmd) = Exercise.init domId
        model_ = { model
                | chosen = Just coord
                , exModel = Just exModel }
        cmdFocus = Task.attempt (always NoOp) (Dom.focus domId)
        cmdExCmd = Cmd.map ExMsg exCmd
      in
        (model_, Cmd.batch [cmdExCmd, cmdFocus])
    ExMsg exMsg ->
      case model.exModel of
        Nothing -> (model, Cmd.none)
        Just exModel ->
          let
            (exModel_, exCmd) = Exercise.update exMsg exModel
            model_ = { model
                     | exModel = Just exModel_}
          in
            if Exercise.isCorrect exModel_ then
              updateExerciseCorrect model
            else
              ( model_, Cmd.map ExMsg exCmd )

updateExerciseCorrect : Model -> (Model, Cmd Msg)
updateExerciseCorrect model =
  let
    model_ = { model
            | exModel = Nothing
            , chosen = Nothing
            , board = makeVisible model.board model.chosen }
  in
    (model_, Cmd.none)

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
        { init = init "https://i.ytimg.com/vi/GCqECajz920/maxresdefault.jpg" 1280 720
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
