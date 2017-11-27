module Exercise exposing (..)

import Random exposing (pair, int)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onInput)


-----
-- MODEL
type Operator = Add
type Result = None | Correct | Wrong

type alias Model = {
  num1: Int,
  num2: Int,
  op: Operator,
  result: Result
}

createModel : Int -> Operator -> Int -> Model
createModel num1 op num2 =
  { num1 = num1
  , num2 = num2
  , op = op
  , result = None }


-----
-- INIT
initWithData : Int -> Operator -> Int -> (Model, Cmd Msg)
initWithData num1 op num2 =
  (createModel num1 op num2, Cmd.none)

init : (Model, Cmd Msg)
init =
  let
    model = createModel 0 Add 0
    gen = Random.generate Init <| pair (int 0  9) (int 0 9)
  in
    ( model, gen )

-----
-- MESSAGES
type Msg = Noop | Init (Int, Int) | Input String


-----
-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop ->
      (model, Cmd.none)
    Init (n1, n2) ->
      let model = createModel n1 Add n2
      in (model, Cmd.none)
    Input value ->
      (model, Cmd.none)


-----
-- VIEW
view : Model -> Html Msg
view model =
  div [ class "exercise" ]
  [ viewExercise model
  , input [ onInput Input ] [] ]

viewExercise : Model -> Html msg
viewExercise model = text <| (toString model.num1) ++ " " ++ (op2str model.op) ++ " " ++ (toString model.num2) ++ " = "


-----
-- HELPERS
op2str : Operator -> String
op2str op =
  case op of
    Add -> "+"