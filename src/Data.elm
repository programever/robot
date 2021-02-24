module Data exposing
    ( Cell(..)
    , Direction(..)
    , Grids
    , Point
    , init
    , keyToPointDecoder
    , setActiveCell
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
    { cells : List (List Cell)
    , direction : Direction
    }


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
    { cells = List.map mapCols listRow
    , direction = East
    }



---- UPDATE ----


setActiveCell : Point -> Grids -> Grids
setActiveCell point grids =
    let
        { cells } =
            grids

        ( x, y ) =
            point

        activeCell =
            getActiveCell grids

        direction =
            activeCell
                |> Maybe.map (toDirection point)
                |> Maybe.withDefault East

        moveAble =
            activeCell
                |> Maybe.map (getCellPoint >> ableToMove direction point)
                |> Maybe.withDefault False

        mapCells =
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
    in
    if moveAble then
        { cells = List.map mapCells cells
        , direction = direction
        }

    else
        grids



---- DECODER ----


keyToPointDecoder : Grids -> Int -> Maybe Point
keyToPointDecoder grids keyCode =
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
getActiveCell { cells } =
    let
        isActive cell =
            case cell of
                Active _ ->
                    True

                Normal _ ->
                    False
    in
    List.concat cells
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

