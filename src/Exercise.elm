module Exercise exposing (..)

import Random exposing (pair, int)

import Html exposing (Html, div, input, text, form)
import Html.Attributes exposing (class)
import Html.Events exposing (onInput, onSubmit)


-----
-- MODEL
type Operator = Add
type Evaluation = None | Correct | Wrong

type alias Model = {
  num1: Int,
  num2: Int,
  result: Int,
  op: Operator,
  input : String,
  evaluation: Evaluation
}

createModel : Int -> Operator -> Int -> Model
createModel num1 op num2 =
  { num1 = num1
  , num2 = num2
  , result = execute num1 op num2
  , op = op
  , input = ""
  , evaluation = None }


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
type Msg = Noop | Init (Int, Int) | Input String | Submit


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
      ( { model | input = value }, Cmd.none)
    Submit ->
      let
        evaluation =
          if model.input == (toString model.result) then
            Correct
          else
            Wrong
      in
        ( { model | evaluation = evaluation }, Cmd.none )


-----
-- VIEW
view : Model -> Html Msg
view model =
  div [ class "exercise" ]
    [ form [onSubmit Submit]
      [
        viewExercise model
      , input [ onInput Input ] []
      , viewEvaluation model
      ]
    ]

viewExercise : Model -> Html msg
viewExercise model = text <| (toString model.num1) ++ " " ++ (op2str model.op) ++ " " ++ (toString model.num2) ++ " = "

viewEvaluation : Model -> Html msg
viewEvaluation model =
  let content = case model.evaluation of
    Correct -> "OK"
    Wrong -> "Wrong"
    None -> ""
  in
    div [ class "evaluation" ] [text <| " " ++ content]


-----
-- HELPERS
op2str : Operator -> String
op2str op =
  case op of
    Add -> "+"

execute : Int -> Operator -> Int -> Int
execute num1 op num2 =
  case op of
    Add -> num1 + num2
