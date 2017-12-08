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
images =
  [ initImage "http://cdn.cheatcc.com/Screenshots/guide/title_card.jpg" 1280 720 4 4
  , initImage "https://i.ytimg.com/vi/IdptgB2a7xA/maxresdefault.jpg" 1280 720 4 6
  , initImage "https://static01.nyt.com/images/2016/04/23/movies/video-agent-fox/video-agent-fox-videoSixteenByNine1050.jpg" 1050 591 3 4
  , initImage "http://pre07.deviantart.net/1881/th/pre/f/2017/142/1/f/sahara_ajar_gary_by_giuseppedirosso-dba3ys1.jpg" 1385 577 3 3
  , initImage "http://assets1.ignimgs.com/2017/08/08/00---intro-1502235223391_1280w.jpg" 1280 720 4 6
  , initImage "http://images5.fanpop.com/image/photos/29500000/Po-vs-Shen-kung-fu-panda-2-29569311-1078-741.png" 1078 741 4 4
  , initImage "http://3.bp.blogspot.com/-OCAFi2IC8-Y/VqxdMPD8MGI/AAAAAAAADK8/6L8FhecuRVQ/s1600/%25E8%259E%25A2%25E5%25B9%2595%25E5%25BF%25AB%25E7%2585%25A7%2B2016-01-30%2B%25E4%25B8%258B%25E5%258D%25882.50.04.png" 1316 650 3 5
  , initImage "http://www.boomerang.asia/dynamic/show/00000000/472/601e449754dfa1e05f84e34cce2ca3a6.jpg" 1230 603 4 7
  , initImage "https://i.ytimg.com/vi/r6fx5C_YcKM/maxresdefault.jpg" 1280 720 5 5
  , initImage "http://m.cdn.blog.hu/sm/smokingbarrels/image/vuk.jpg" 1280 720 4 3
  ]

  -- [ initImage "https://i.ytimg.com/vi/GCqECajz920/maxresdefault.jpg" 1280 720 5 6
  -- , initImage "https://assets.halfbrick.com/wp-content/uploads/New-Characters.jpg" 1024 768 3 3
  -- , initImage "http://images.contentful.com/7h71s48744nc/1ovlLkLeOgocakYMIIWKek/cf1a6efcb54cceeb4f65f12f237d9f75/arthur-and-the-invisibles.jpg" 1000 735 4 3
  -- , initImage "http://download.gamezone.com/uploads/image/data/869699/dragon2.jpg" 1024 564 3 4
  -- , initImage "https://vignette.wikia.nocookie.net/minecraft-mob/images/1/19/Minecraft_mob_lineup_wallpaper_by_younggeorge-d5rngge.png/revision/latest?cb=20150919083150" 900 563 3 5
  -- , initImage "https://sickr.files.wordpress.com/2016/11/steamworld_dig.jpg?w=1200" 1200 800 5 6
  -- , initImage "https://www.awn.com/sites/default/files/image/featured/1026353-trick-3d-heralds-holiday-season-elf-s-story.jpg" 1280 720 3 4
  -- , initImage "http://www.dan-dare.org/FreeFun/Images/CartoonsMoviesTV/KungFuPandaWallpaper1024.jpg" 1024 768 5 5
  -- , initImage "http://www.poisonmushroom.org/content/uploads/2014/11/Club_Nintendo_Characters_Poster.jpg" 1006 587 4 6
  -- , initImage "https://cdn57.androidauthority.net/wp-content/uploads/2016/11/star-wars-galaxy-of-heroes-840x400.jpg" 840 400 3 5
  -- ]

-----
-- MODELS
type alias Coord = (Int, Int)
type alias Board = List (List Cell)
type Cell = Visible | Hidden
type alias Image =
  { url: String
  , width: Int
  , height: Int
  , rows: Int
  , cols: Int
  }

type alias Model =
  { image: Image
  , chosen: Maybe Coord
  , exModel: Maybe Exercise.Model
  , board: Board
  }

init : Int -> (Model, Cmd Msg)
init index =
  let
    img = itemAt images index
    model =
      case img of
        Nothing -> Debug.crash "invalid image index..."
        Just image ->
          { image = image
          , chosen = Nothing
          , exModel = Nothing
          , board = List.repeat image.rows <| List.repeat image.cols Hidden
          }
  in
    (model, Cmd.none)

initImage : String -> Int -> Int -> Int -> Int -> Image
initImage url width height rows cols =
  { url = url
  , width = width
  , height = height
  , rows = rows
  , cols = cols
  }

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
  | ChooseImage Int
  | ChooseCell Coord
  | ExMsg Exercise.Msg


-----
-- VIEW
view : Model -> Html Msg
view model =
  div [class "puzzle"]
    [ stylesheet
    , viewImages model
    , viewImage model
    , viewTable model
    , viewExercise model
    ]

viewImages : Model -> Html Msg
viewImages model =
  let getContent i img = toString i
  in
    div [ class "images" ] (
      images |> List.indexedMap (
        \i img -> div [class "thumbnail", onClick (ChooseImage i)] [ text (getContent i img) ]
      )
    )

viewImage : Model -> Html Msg
viewImage model =
  img [ src model.image.url ] []

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
    table [width model.image.width, height model.image.height] <|
      (flip List.indexedMap) model.board
        (\r row ->
          tr [] <|
            (flip List.indexedMap) row
              (\c cell ->
                td [class (getClass cell), class (getChosenClass model.chosen r c), onClick (ChooseCell (r, c))] []
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
    ChooseCell coord ->
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
    ChooseImage index ->
      init index

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
-- HELPERS
itemAt : List a -> Int -> Maybe a
itemAt list index =
  case list of
    [] -> Nothing
    x::xs ->
      if index == 0 then
        Just x
      else
        itemAt xs (index - 1)


-----
-- MAIN
main : Program Never Model Msg
main =
    program
        { init = init 0
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
