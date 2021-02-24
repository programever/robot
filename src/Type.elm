module Type exposing (Model, Msg(..))

import Data exposing (Grids, Point)


type Msg
    = MoveTo Point
    | Noop


type alias Model =
    Grids
