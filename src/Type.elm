module Type exposing (Model, Msg(..))

import Data exposing (Direction, Grids, Point)


type Msg
    = MoveTo Point
    | Noop


type alias Model =
    { grids : Grids
    , direction : Direction
    }
