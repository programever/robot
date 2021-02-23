module Main exposing (main)

import Browser
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



---- CONST ----


gridsConfig : { rows : Int, cols : Int }
gridsConfig =
    { rows = 5, cols = 5 }



---- TYPE ----


type Msg
    = MoveTo Int Int


type Direction
    = North
    | East
    | South
    | West


type alias Grids =
    List (List Cell)


type Cell
    = Normal Int Int
    | Active Int Int



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
                        Active 0 0

                    else
                        Normal x y
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
        MoveTo x y ->
            let
                { grids } =
                    model

                activeCell =
                    getActiveCell grids

                direction =
                    activeCell
                        |> Maybe.map (getDirection x y)
                        |> Maybe.withDefault East

                moveAble =
                    activeCell
                        |> Maybe.map (ableToMove direction x y)
                        |> Maybe.withDefault False
            in
            if moveAble then
                ( { model | grids = setActiveCell x y grids, direction = direction }, Cmd.none )

            else
                ( model, Cmd.none )


getDirection : Int -> Int -> Cell -> Direction
getDirection x y cell =
    let
        ( currentX, currentY ) =
            getCellXY cell
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
                Active _ _ ->
                    True

                Normal _ _ ->
                    False
    in
    List.concat grids
        |> List.filter isActive
        |> List.head


getCellXY : Cell -> ( Int, Int )
getCellXY cell =
    case cell of
        Normal x y ->
            ( x, y )

        Active x y ->
            ( x, y )


ableToMove : Direction -> Int -> Int -> Cell -> Bool
ableToMove direction x y cell =
    let
        { rows, cols } =
            gridsConfig

        ( currentX, currentY ) =
            getCellXY cell
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


setActiveCell : Int -> Int -> Grids -> Grids
setActiveCell x y grids =
    let
        mapCells cells =
            List.map
                (\cell ->
                    let
                        ( currentX, currentY ) =
                            getCellXY cell
                    in
                    if currentX == x && currentY == y then
                        Active currentX currentY

                    else
                        Normal currentX currentY
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
        ( cellStyle, x, y ) =
            case cell of
                Normal x_ y_ ->
                    ( [], x_, y_ )

                Active x_ y_ ->
                    ( style.activeCell direction, x_, y_ )
    in
    div [ css <| style.cell ++ cellStyle, onClick <| MoveTo x y ] []



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
        , subscriptions = always Sub.none
        }
