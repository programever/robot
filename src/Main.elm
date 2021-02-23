module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Css
    exposing
        ( alignItems
        , backgroundImage
        , backgroundPosition
        , backgroundSize
        , border3
        , center
        , color
        , column
        , contain
        , displayFlex
        , flexDirection
        , fontSize
        , height
        , hex
        , marginTop
        , padding
        , px
        , row
        , solid
        , url
        , width
        )
import Html.Styled exposing (Html, div, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Json.Decode as JD



---- CONST ----


gridsConfig : { rows : Int, cols : Int }
gridsConfig =
    { rows = 5, cols = 5 }



---- TYPE ----


type Msg
    = MoveTo Point
    | Noop


type Direction
    = North
    | East
    | South
    | West


type alias Grids =
    List (List Cell)


type Cell
    = Normal Point
    | Active Point


type alias Point =
    ( Int, Int )



---- SUBSCRIPTION ----


subscriptions : Model -> Sub Msg
subscriptions { grids } =
    let
        ( x, y ) =
            getActiveCell grids
                |> Maybe.map getCellPoint
                |> Maybe.withDefault ( 0, 0 )

        keyDecoder =
            JD.map (toDirection >> toMsg) (JD.field "keyCode" JD.int)

        toDirection keyCode =
            case keyCode of
                38 ->
                    Just North

                39 ->
                    Just East

                40 ->
                    Just South

                37 ->
                    Just West

                _ ->
                    Nothing

        toMsg direction =
            case direction of
                Nothing ->
                    Noop

                Just direction_ ->
                    case direction_ of
                        North ->
                            MoveTo ( x, y - 1 )

                        East ->
                            MoveTo ( x + 1, y )

                        South ->
                            MoveTo ( x, y + 1 )

                        West ->
                            MoveTo ( x - 1, y )
    in
    onKeyDown keyDecoder



---- MODEL ----


type alias Model =
    { grids : Grids
    , direction : Direction
    }


initGrids : Grids
initGrids =
    let
        { rows, cols } =
            gridsConfig

        listRow =
            List.range 0 (rows - 1)

        listCol =
            List.range 0 (cols - 1)

        mapCols y =
            List.map
                (\x ->
                    if x == 0 && y == 0 then
                        Active ( 0, 0 )

                    else
                        Normal ( x, y )
                )
                listCol
    in
    List.map mapCols listRow


init : ( Model, Cmd Msg )
init =
    ( { grids = initGrids
      , direction = East
      }
    , Cmd.none
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveTo point ->
            let
                { grids } =
                    model

                activeCell =
                    getActiveCell grids

                direction =
                    activeCell
                        |> Maybe.map (getDirection point)
                        |> Maybe.withDefault East

                moveAble =
                    activeCell
                        |> Maybe.map (ableToMove direction point)
                        |> Maybe.withDefault False
            in
            if moveAble then
                ( { model | grids = setActiveCell point grids, direction = direction }, Cmd.none )

            else
                ( model, Cmd.none )

        Noop ->
            ( model, Cmd.none )


getDirection : Point -> Cell -> Direction
getDirection ( x, y ) cell =
    let
        ( currentX, currentY ) =
            getCellPoint cell
    in
    if x == currentX && y < currentY then
        North

    else if x > currentX && y == currentY then
        East

    else if x == currentX && y > currentY then
        South

    else
        West


getActiveCell : Grids -> Maybe Cell
getActiveCell grids =
    let
        isActive cell =
            case cell of
                Active _ ->
                    True

                Normal _ ->
                    False
    in
    List.concat grids
        |> List.filter isActive
        |> List.head


getCellPoint : Cell -> Point
getCellPoint cell =
    case cell of
        Normal point ->
            point

        Active point ->
            point


ableToMove : Direction -> Point -> Cell -> Bool
ableToMove direction ( x, y ) cell =
    let
        { rows, cols } =
            gridsConfig

        ( currentX, currentY ) =
            getCellPoint cell
    in
    case direction of
        North ->
            (currentX == x) && (currentY - 1 == y && y > -1)

        East ->
            (currentX + 1 == x && x < cols) && (currentY == y)

        South ->
            (currentX == x) && (currentY + 1 == y && y < rows)

        West ->
            (currentX - 1 == x && x > -1) && (currentY == y)


setActiveCell : Point -> Grids -> Grids
setActiveCell ( x, y ) grids =
    let
        mapCells cells =
            List.map
                (\cell ->
                    let
                        ( currentX, currentY ) =
                            getCellPoint cell
                    in
                    if currentX == x && currentY == y then
                        Active ( currentX, currentY )

                    else
                        Normal ( currentX, currentY )
                )
                cells
    in
    List.map mapCells grids



---- VIEW ----


view : Model -> Html Msg
view { grids, direction } =
    div [ css style.container ]
        [ div [ css style.header ] [ text "ROBOT" ]
        , div [ css style.intro ] [ text "Use your cursors or click on the next cell to move the Robot" ]
        , gridsView direction grids
        ]


gridsView : Direction -> Grids -> Html Msg
gridsView direction grids =
    div [ css style.grids ] <|
        List.map (\cells -> div [ css style.row ] (List.map (cellView direction) cells)) grids


cellView : Direction -> Cell -> Html Msg
cellView direction cell =
    let
        ( cellStyle, point ) =
            case cell of
                Normal point_ ->
                    ( [], point_ )

                Active point_ ->
                    ( style.activeCell direction, point_ )
    in
    div [ css <| style.cell ++ cellStyle, onClick <| MoveTo point ] []



---- STYLE ----


style =
    { container =
        [ displayFlex
        , flexDirection column
        , alignItems center
        ]
    , header =
        [ fontSize (px 20)
        , padding (px 20)
        , color (hex "E15A1D")
        ]
    , intro =
        [ fontSize (px 12)
        , color (hex "333333")
        ]
    , grids =
        [ marginTop (px 20)
        ]
    , row =
        [ displayFlex
        , flexDirection row
        ]
    , cell =
        [ width (px 50)
        , height (px 50)
        , border3 (px 1) solid (hex "BDBDBD")
        ]
    , activeCell =
        \direction ->
            let
                robotImage =
                    case direction of
                        North ->
                            "/assets/images/north.png"

                        East ->
                            "/assets/images/east.png"

                        South ->
                            "/assets/images/south.png"

                        West ->
                            "/assets/images/west.png"
            in
            [ backgroundImage (url robotImage)
            , backgroundPosition center
            , backgroundSize contain
            ]
    , grid = []
    }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = toUnstyled << view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
