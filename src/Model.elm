module Model exposing (init, update)

import Data as Data
import Type exposing (Model, Msg(..))


init : ( Model, Cmd Msg )
init =
    ( Data.init
    , Cmd.none
    )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MoveTo point ->
            ( Data.setActiveCell point model, Cmd.none )

        Noop ->
            ( model, Cmd.none )

