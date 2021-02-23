module Subscription exposing (subscription)

import Browser.Events exposing (onKeyDown)
import Data as Data
import Json.Decode as JD
import Type exposing (Model, Msg(..))



---- SUBSCRIPTION ----


subscription : Model -> Sub Msg
subscription { grids } =
    let
        keyDecoder =
            JD.map (Data.arrowKeysDecoder grids >> toMsg) (JD.field "keyCode" JD.int)

        toMsg point =
            Maybe.map MoveTo point
                |> Maybe.withDefault Noop
    in
    onKeyDown keyDecoder
