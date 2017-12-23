module ExercisePuzzle exposing (..)
import Task
import Dom
import Html exposing (Html, div, text, program, node, img, table, tr, td)
import Html.Attributes exposing (attribute, class, src, width, height)
import Html.Events exposing (onClick)

import PuzzleExercise exposing (Puzzle)
-- import Exercise
import Database


-----
-- DATABASE
images : List PuzzleExercise.Image
images =
  [ PuzzleExercise.initImage "shifu" "https://i.ytimg.com/vi/pXExMuZw9eM/maxresdefault.jpg" 1280 720 1 1 -- 3 3
  , PuzzleExercise.initImage "calvin" "https://vignette.wikia.nocookie.net/candh/images/2/2b/Calvin.jpg/revision/latest" 1024 768 3 4
  , PuzzleExercise.initImage "poo-before-fight" "https://vignette3.wikia.nocookie.net/kungfupanda/images/0/0a/PoAdversary.jpg/revision/latest" 1920 816 3 6
  , PuzzleExercise.initImage "cars-crowd" "https://wallpapercave.com/wp/k6XGIO9.jpg" 1024 768 5 5
  , PuzzleExercise.initImage "shaun-candy-bus" "https://i.ytimg.com/vi/WBXEE2StIwY/maxresdefault.jpg" 1280 720 3 4
  , PuzzleExercise.initImage "reksio" "http://bi.gazeta.pl/im/30/b3/bd/z12432176IER,Reksio.jpg" 1280 864 3 3
  , PuzzleExercise.initImage "cars-wasabi" "https://pbs.twimg.com/media/DNK_RxWX0AEjkBg.jpg" 1000 418 3 3
  , PuzzleExercise.initImage "spaceman-finn" "https://i1.wp.com/www.tor.com/wp-content/uploads/2016/01/calvin-hobbes-tfa-spaceman-finn.jpg?resize=625%2C625&type=vertical" 625 625 3 3
  , PuzzleExercise.initImage "poo-animgif" "https://quintype-01.imgix.net/thequint%2F2016-04%2Ff9b095be-760d-422a-8652-da8896835a79%2Ftumblr_lum55wl03h1qiol6o1.gif?q=35&auto=format&w=1200" 1200 511 4 6
  , PuzzleExercise.initImage "kylo_and_dart" "https://static.independent.co.uk/s3fs-public/thumbnails/image/2016/01/07/16/kylo-darth-calvin-hobbes.jpg" 1172 806 5 5
  ]

  -- [ initImage "0" "http://cdn.cheatcc.com/Screenshots/guide/title_card.jpg" 1280 720 4 4
  -- , initImage "1" "https://i.ytimg.com/vi/IdptgB2a7xA/maxresdefault.jpg" 1280 720 4 6
  -- , initImage "2" "https://static01.nyt.com/images/2016/04/23/movies/video-agent-fox/video-agent-fox-videoSixteenByNine1050.jpg" 1050 591 3 4
  -- , initImage "3" "http://pre07.deviantart.net/1881/th/pre/f/2017/142/1/f/sahara_ajar_gary_by_giuseppedirosso-dba3ys1.jpg" 1385 577 3 3
  -- , initImage "4" "http://assets1.ignimgs.com/2017/08/08/00---intro-1502235223391_1280w.jpg" 1280 720 4 6
  -- , initImage "5" "http://images5.fanpop.com/image/photos/29500000/Po-vs-Shen-kung-fu-panda-2-29569311-1078-741.png" 1078 741 4 4
  -- , initImage "6" "http://3.bp.blogspot.com/-OCAFi2IC8-Y/VqxdMPD8MGI/AAAAAAAADK8/6L8FhecuRVQ/s1600/%25E8%259E%25A2%25E5%25B9%2595%25E5%25BF%25AB%25E7%2585%25A7%2B2016-01-30%2B%25E4%25B8%258B%25E5%258D%25882.50.04.png" 1316 650 3 5
  -- , initImage "7" "http://www.boomerang.asia/dynamic/show/00000000/472/601e449754dfa1e05f84e34cce2ca3a6.jpg" 1230 603 4 7
  -- , initImage "8" "https://i.ytimg.com/vi/r6fx5C_YcKM/maxresdefault.jpg" 1280 720 5 5
  -- , initImage "9" "http://m.cdn.blog.hu/sm/smokingbarrels/image/vuk.jpg" 1280 720 4 3
  -- ]

  -- [ initImage "10" "https://i.ytimg.com/vi/GCqECajz920/maxresdefault.jpg" 1280 720 5 6
  -- , initImage "11" "https://assets.halfbrick.com/wp-content/uploads/New-Characters.jpg" 1024 768 3 3
  -- , initImage "12" "http://images.contentful.com/7h71s48744nc/1ovlLkLeOgocakYMIIWKek/cf1a6efcb54cceeb4f65f12f237d9f75/arthur-and-the-invisibles.jpg" 1000 735 4 3
  -- , initImage "13" "http://download.gamezone.com/uploads/image/data/869699/dragon2.jpg" 1024 564 3 4
  -- , initImage "14" "https://vignette.wikia.nocookie.net/minecraft-mob/images/1/19/Minecraft_mob_lineup_wallpaper_by_younggeorge-d5rngge.png/revision/latest?cb=20150919083150" 900 563 3 5
  -- , initImage "15" "https://sickr.files.wordpress.com/2016/11/steamworld_dig.jpg?w=1200" 1200 800 5 6
  -- , initImage "16" "https://www.awn.com/sites/default/files/image/featured/1026353-trick-3d-heralds-holiday-season-elf-s-story.jpg" 1280 720 3 4
  -- , initImage "17" "http://www.dan-dare.org/FreeFun/Images/CartoonsMoviesTV/KungFuPandaWallpaper1024.jpg" 1024 768 5 5
  -- , initImage "18" "http://www.poisonmushroom.org/content/uploads/2014/11/Club_Nintendo_Characters_Poster.jpg" 1006 587 4 6
  -- , initImage "19" "https://cdn57.androidauthority.net/wp-content/uploads/2016/11/star-wars-galaxy-of-heroes-840x400.jpg" 840 400 3 5
  -- ]

-----
-- MODELS
type alias Model =
  { imageIndex: Int
  , doneImages: List String
  , puzzle: Puzzle
  , fireworks: Maybe String
  }

initGame : (Model, Cmd Msg)
initGame =
  let (model, cmd) = init 0
  in (model, Database.getDoneImages "")

init : Int -> (Model, Cmd Msg)
init index =
  let
    img = itemAt images index
    model =
      case img of
        Nothing -> Debug.crash "invalid image index..."
        Just image ->
          { imageIndex = index
          , doneImages = []
          , fireworks = Nothing
          , puzzle = PuzzleExercise.initPuzzle image False
          }
  in
    (model, Cmd.none)

changePuzzle : Model -> Int -> (Model, Cmd Msg)
changePuzzle model index =
  let img = itemAt images index
      model_ = case img of
        Nothing -> Debug.crash "invalid image index..."
        Just image ->
          { model
          | imageIndex = index
          , puzzle = PuzzleExercise.initPuzzle image (isImageDone model image) }
  in
    (model_, Cmd.none)

isImageDone : Model -> PuzzleExercise.Image -> Bool
isImageDone model img = List.any ((==) img.imageId) model.doneImages

-----
-- MESSAGES
type Msg
  = NoOp
  | DoneImages (List String)
  | ChooseImage Int
  | PuzzleMsg PuzzleExercise.Msg


-----
-- VIEW
view : Model -> Html Msg
view model =
  div [class "puzzle"]
    [ stylesheet
    -- , viewImageUrl model
    , viewImages model
    , PuzzleExercise.viewImageAndTable model.puzzle |> Html.map PuzzleMsg
    , PuzzleExercise.viewExercise model.puzzle |> Html.map PuzzleMsg
    , viewFireworks model
    ]

viewImages : Model -> Html Msg
viewImages model =
  let getContent i img = toString i
      getSelectedClass i =
        if i == model.imageIndex then "selected " else ""
      getDoneClass img =
        if isImageDone model img then "done " else ""

      getThumbClass i img = class <| "thumbnail " ++ (getSelectedClass i) ++ (getDoneClass img)
  in
    div [ class "images" ] (
      images |> List.indexedMap (
        \i img -> div [getThumbClass i img, onClick (ChooseImage i)] [ text (getContent i img) ]
      )
    )

viewImageUrl : Model -> Html Msg
viewImageUrl model = div [] [ text model.puzzle.image.url ]

viewFireworks : Model -> Html Msg
viewFireworks model =
  case model.fireworks of
    Nothing -> text ""
    Just res ->
      div [class "fireworks"] [
        img [src ("resources/" ++ res)] []
      ]

stylesheet : Html msg
stylesheet =
    let
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      "resources/css.css"
            ]
    in
        node "link" attrs []


-----
-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp -> (model, Cmd.none)
    DoneImages doneImages ->
      (updateDoneImages model doneImages, Cmd.none)
    PuzzleMsg puzMsg ->
      let
          puzModel = model.puzzle
          (puzModel_, puzCmd_) = PuzzleExercise.update puzMsg model.puzzle
      in
        ( {model | puzzle = puzModel_}
        , Cmd.map PuzzleMsg puzCmd_
        ) |> andThen updateFireworks
          |> andThen updateDoneImage
    ChooseImage index ->
      changePuzzle model index

updateDoneImages : Model -> List String -> Model
updateDoneImages model doneImages = { model | doneImages = doneImages }

updateFireworks : Model -> (Model, Cmd Msg)
updateFireworks model =
  let model_ =
    if PuzzleExercise.isDone model.puzzle then
      { model | fireworks = Just "fireworks1.gif" }
    else
      model
  in
    (model_, Cmd.none)

updateDoneImage : Model -> (Model, Cmd Msg)
updateDoneImage model =
  let cmd_ =
    if PuzzleExercise.isDone model.puzzle then
      Database.setImageDone model.puzzle.image.imageId
    else
      Cmd.none
  in
    (model, cmd_)


-----
-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
      [ Database.doneImages DoneImages ]


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

andThen : (Model -> (Model, Cmd a)) -> (Model, Cmd a) -> (Model, Cmd a)
andThen f (model, cmd) =
  let (model_, cmd_) = f model
  in (model_, Cmd.batch [cmd, cmd_])


-----
-- MAIN
-- main = images |> List.map (\i -> i.rows * i.cols) |> List.sum |> toString |> text
main : Program Never Model Msg
main =
    program
        { init = initGame
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
