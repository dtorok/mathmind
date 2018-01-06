module Exercise exposing (..)

import Random exposing (pair, int, andThen, Generator, map)

import Html exposing (Html, div, input, text, form)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onInput, onSubmit)

import Database


-----
-- MODEL
type Operator = Add | Sub
type Evaluation = None | Correct | Wrong
type ExerciseType = Addition | Subtraction


exerciseTypes : List ExerciseType
exerciseTypes =
  [ Addition
  , Subtraction
  ]

type alias Model =
  { id: String
  , exerciseType: ExerciseType
  , num1: Int
  , num2: Int
  , result: Int
  , op: Operator
  , input : String
  , evaluation: Evaluation
}


-----
-- MESSAGES
type Msg = Noop | InitModel Model | Input String | Submit


-----
-- MODEL CREATION
createModel : String -> ExerciseType -> Int -> Operator -> Int -> Model
createModel id t num1 op num2 =
  { id = id
  , exerciseType = t
  , num1 = num1
  , num2 = num2
  , result = execute num1 op num2
  , op = op
  , input = ""
  , evaluation = None }


-----
-- INIT
initWithData : String -> ExerciseType -> Int -> Operator -> Int -> (Model, Cmd Msg)
initWithData id t num1 op num2 =
  (createModel id t num1 op num2, Cmd.none)

init : String -> (Model, Cmd Msg)
init id =
  let
    model = createModel id Addition 0 Add 0
    gen = Random.generate InitModel
      (  int 0 ((List.length exerciseTypes) - 1)
      |> map index2type
      |> andThen (generateModelByType id)
      )
  in
    ( model, gen )

generateModelByType : String -> ExerciseType -> Generator Model
generateModelByType id t =
  let
    genData = generateByType t
    data2model : (Int, Operator, Int) -> Model
    data2model (n1, op, n2) =
      createModel id t n1 op n2
  in
    Random.map data2model genData

generateByType : ExerciseType -> Generator (Int, Operator, Int)
generateByType t =
  let op = type2op t
      result (n1, n2) = (n1, op, n2)
  in case t of
    Addition ->
      pair (int 3 9) (int 0 9)
        |> map result
    Subtraction ->
      pair (int 2 9) (int 2 9)
        |> map (\(a, b) -> (a + b, b))
        |> map result


-----
-- API
isCorrect : Model -> Bool
isCorrect model = model.evaluation == Correct


-----
-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Noop ->
      (model, Cmd.none)
    InitModel model ->
      (model, Cmd.none)
    Input value ->
      ( { model | input = value }, Cmd.none)
    Submit ->
      let
        correct = model.input == (toString model.result)
        evaluation =
          if correct then Correct
          else Wrong
        cmd =
          if correct then Database.dbExerciseSolved (exerciseType2str model.exerciseType)
          else Cmd.none
      in
        ( { model | evaluation = evaluation }, cmd )


-----
-- VIEW
view : Model -> Html Msg
view model =
  div [ class "exercise" ]
    [ form [onSubmit Submit]
      [
        viewExercise model
      , input [ onInput Input, id model.id ] []
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
    Sub -> "-"

exerciseType2str : ExerciseType -> String
exerciseType2str t =
  case t of
    Addition -> "addition"
    Subtraction -> "subtraction"

type2op : ExerciseType -> Operator
type2op t =
  case t of
    Addition -> Add
    Subtraction -> Sub

orderTuple : (Int, Int) -> (Int, Int)
orderTuple (a, b) = (max a b, min a b)

index2type : Int -> ExerciseType
index2type i
  =  List.drop i exerciseTypes
  |> List.head
  |> Maybe.withDefault Addition

execute : Int -> Operator -> Int -> Int
execute num1 op num2 =
  case op of
    Add -> num1 + num2
    Sub -> num1 - num2
