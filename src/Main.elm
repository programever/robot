module Main exposing (main)

import Browser
import Html.Styled exposing (toUnstyled)
import Model exposing (init, update)
import Subscription exposing (subscription)
import Type exposing (Model, Msg)
import View exposing (view)


main : Program () Model Msg
main =
    Browser.element
        { view = toUnstyled << view
        , init = \_ -> init
        , update = update
        , subscriptions = subscription
        }
