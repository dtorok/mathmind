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
type Component = Num1 | Num2 | Result
type ExerciseType
  = Addition
  | Subtraction
  | DoubleAddition
  | DoubleSubtraction
  | AdditionTo10
  | SubtractionFrom10
  | Addition20
  | Subtraction20


exerciseTypes : List ExerciseType
exerciseTypes =
  [ AdditionTo10
  , SubtractionFrom10
  , DoubleSubtraction
  , Addition20
  , Addition20
  , Subtraction20
  , Subtraction20
  ]

type alias Model =
  { id: String
  , exerciseType: ExerciseType
  , num1: Int
  , num2: Int
  , result: Int
  , op: Operator
  , input : String
  , blank : Component
  , evaluation: Evaluation
}


-----
-- MESSAGES
type Msg = Noop | InitModel Model | Input String | Submit


-----
-- MODEL CREATION
createModel : String -> ExerciseType -> Int -> Operator -> Int -> Component -> Model
createModel id t num1 op num2 blank =
  { id = id
  , exerciseType = t
  , num1 = num1
  , num2 = num2
  , result = execute num1 op num2
  , op = op
  , input = ""
  , blank = blank
  , evaluation = None }


-----
-- INIT
initWithData : String -> ExerciseType -> Int -> Operator -> Int -> Component -> (Model, Cmd Msg)
initWithData id t num1 op num2 blank =
  (createModel id t num1 op num2 blank, Cmd.none)

init : String -> (Model, Cmd Msg)
init id = initWithTypeOptions id exerciseTypes

initWithTypeOptions : String -> List ExerciseType -> (Model, Cmd Msg)
initWithTypeOptions id typeOptions =
  let
    model = createModel id Addition 0 Add 0 Result
    gen = Random.generate InitModel (
      int 0 ((List.length typeOptions) - 1)
      -- int 5 5
      |> map index2type
      |> andThen (generateModelByType id)
      )
  in
    ( model, gen )


generateModelByType : String -> ExerciseType -> Generator Model
generateModelByType id t =
  let
    genData = generateByType t
    blank = type2blank t
    data2model : (Int, Operator, Int) -> Model
    data2model (n1, op, n2) =
      createModel id t n1 op n2 blank
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
    Addition20 ->
      pair (int 10 20) (int 3 20)
        |> map result
    Subtraction20 ->
      pair (int 1 20) (int 5 10)
        |> map (\(a, b) -> (a + b, b))
        |> map result
    DoubleAddition ->
      (int 3 10)
        |> map (\a -> (a, a))
        |> map result
    DoubleSubtraction ->
      (int 3 10)
        |> map (\a -> (a * 2, a))
        |> map result
    AdditionTo10 ->
      (int 1 9)
        |> map (\a -> (a, 10 - a))
        |> map result
    SubtractionFrom10 ->
      (int 1 9)
        |> map (\a -> (10, a))
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
        correct = model.input == toString (component2value model model.blank)
        evaluation =
          if correct then Correct
          else Wrong
        cmd =
          if correct then Database.dbAddScore (exerciseType2str model.exerciseType)
          else Cmd.none
      in
        ( { model | evaluation = evaluation }, cmd )


-----
-- VIEW
view : Model -> Html Msg
view model =
  div [ class "exercise" ]
    [ form [onSubmit Submit]
      [ viewComponent model Num1
      , viewOperator model
      , viewComponent model Num2
      , text " = "
      , viewComponent model Result
      , viewEvaluation model
      ]
    ]

viewComponent : Model -> Component -> Html Msg
viewComponent model component =
  let
    value = component2value model component
    isBlank = component == model.blank
  in
    if isBlank then
      input [ onInput Input, id model.id ] []
    else
      text (toString value)

viewOperator : Model -> Html Msg
viewOperator model =
  text (" " ++ (op2str model.op) ++ " ")

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
    Addition20 -> "addition20"
    Subtraction20 -> "subtraction20"
    DoubleAddition -> "double_addition"
    DoubleSubtraction -> "double_subtraction"
    AdditionTo10 -> "addition_to_10"
    SubtractionFrom10 -> "subtraction_from_10"

type2op : ExerciseType -> Operator
type2op t =
  case t of
    Addition -> Add
    Subtraction -> Sub
    Addition20 -> Add
    Subtraction20 -> Sub
    DoubleAddition -> Add
    DoubleSubtraction -> Sub
    AdditionTo10 -> Add
    SubtractionFrom10 -> Sub

type2blank : ExerciseType -> Component
type2blank t =
  case t of
    Addition -> Result
    Subtraction -> Result
    Addition20 -> Result
    Subtraction20 -> Result
    DoubleAddition -> Result
    DoubleSubtraction -> Result
    AdditionTo10 -> Num2
    SubtractionFrom10 -> Num2

component2value : Model -> Component -> Int
component2value model component =
  case component of
    Num1 -> model.num1
    Num2 -> model.num2
    Result -> model.result

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
