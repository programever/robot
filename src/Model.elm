module Model exposing (init, update)

import Data as Data exposing (Direction(..))
import Type exposing (Model, Msg(..))


init : ( Model, Cmd Msg )
init =
    ( { grids = Data.init
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
                    Data.getActiveCell grids

                direction =
                    activeCell
                        |> Maybe.map (Data.toDirection point)
                        |> Maybe.withDefault East

                moveAble =
                    activeCell
                        |> Maybe.map (Data.getCellPoint >> Data.ableToMove direction point)
                        |> Maybe.withDefault False
            in
            if moveAble then
                ( { model | grids = Data.setActiveCell point grids, direction = direction }, Cmd.none )

            else
                ( model, Cmd.none )

        Noop ->
            ( model, Cmd.none )

