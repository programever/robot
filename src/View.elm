module View exposing (view)

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
import Data exposing (Cell(..), Direction(..), Grids)
import Html.Styled exposing (Html, div, text)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Type exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    div [ css style.container ]
        [ div [ css style.header ] [ text "ROBOT" ]
        , div [ css style.intro ] [ text "Use your cursors or click on the next cell to move the Robot" ]
        , gridsView model
        ]


gridsView : Grids -> Html Msg
gridsView { direction, cells } =
    div [ css style.grids ] <|
        List.map (\cells_ -> div [ css style.row ] (List.map (cellView direction) cells_)) cells


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

