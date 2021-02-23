module Data exposing
    ( Cell(..)
    , Direction(..)
    , Grids
    , Point
    , ableToMove
    , arrowKeysDecoder
    , directionToPoint
    , getActiveCell
    , getCellPoint
    , init
    , setActiveCell
    , toDirection
    )

---- CONST ----


gridsConfig : { rows : Int, cols : Int }
gridsConfig =
    { rows = 5, cols = 5 }



---- TYPE ----


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



---- INIT ----


init : Grids
init =
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



---- UPDATE ----


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



---- DECODER ----


arrowKeysDecoder : Grids -> Int -> Maybe Point
arrowKeysDecoder grids keyCode =
    let
        ( x, y ) =
            getActiveCell grids
                |> Maybe.map getCellPoint
                |> Maybe.withDefault ( 0, 0 )
    in
    case keyCode of
        38 ->
            Just
                ( x, y - 1 )

        39 ->
            Just
                ( x + 1, y )

        40 ->
            Just
                ( x, y + 1 )

        37 ->
            Just
                ( x - 1, y )

        _ ->
            Nothing



---- HELPER ----


toDirection : Point -> Cell -> Direction
toDirection ( x, y ) cell =
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


ableToMove : Direction -> Point -> Point -> Bool
ableToMove direction ( x, y ) ( currentX, currentY ) =
    let
        { rows, cols } =
            gridsConfig
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


directionToPoint : Direction -> Point -> Point
directionToPoint direction ( x, y ) =
    case direction of
        North ->
            ( x, y - 1 )

        East ->
            ( x + 1, y )

        South ->
            ( x, y + 1 )

        West ->
            ( x - 1, y )

