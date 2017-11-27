module App exposing (..)

import Html exposing (Html, div, text, program, node)
import Html.Attributes exposing (attribute)

import Exercise


-- MODEL


type Model =
    ExModel Exercise.Model


convCmd : (modelA -> modelB) -> (cmdA -> cmdB) -> (modelA, Cmd cmdA) -> (modelB, Cmd cmdB)
convCmd fModel fCmd (m, cmdA) =
  (fModel m, Cmd.map fCmd cmdA)

init : ( Model, Cmd Msg )
init = convCmd ExModel ExMsg <|
    Exercise.init


-----
-- MESSAGES
type Msg
    = NoOp
    | ExMsg Exercise.Msg


-----
-- VIEW
view : Model -> Html Msg
view model =
  let content =
    case model of
      ExModel exModel -> viewExercise exModel
  in
    div []
    [ stylesheet
    , content ]

viewExercise : Exercise.Model -> Html Msg
viewExercise model =
  let html = Exercise.view model
  in Html.map ExMsg html

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

-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (msg, model) of
        (ExMsg exMsg, ExModel exModel) ->
          convCmd ExModel ExMsg <| Exercise.update exMsg exModel
        (_, _) ->
            ( model, Cmd.none )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }