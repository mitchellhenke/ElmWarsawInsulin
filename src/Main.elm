module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, input, text)
import Html.Attributes exposing (placeholder, src, step, type_, value)
import Html.Events exposing (onInput)



---- MODEL ----


type alias Model =
    { gramsCarbohydrate : Float
    , gramsFat : Float
    , gramsProtein : Float
    , carbRatio : Float
    , bolusNow : Float
    , bolusLater : Float
    , bolusHours : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { gramsCarbohydrate = 0.0
      , gramsFat = 0.0
      , gramsProtein = 0.0
      , carbRatio = 10.0
      , bolusNow = 0.0
      , bolusLater = 0.0
      , bolusHours = 0.0
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ChangeCarbohydrate String
    | ChangeFat String
    | ChangeProtein String
    | Recalculate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeCarbohydrate carbohydrates ->
            let
                gramsCarbohydrate =
                    Maybe.withDefault 0.0 (String.toFloat carbohydrates)
            in
            update Recalculate { model | gramsCarbohydrate = gramsCarbohydrate }

        ChangeFat fats ->
            let
                gramsFat =
                    Maybe.withDefault 0.0 (String.toFloat fats)
            in
            update Recalculate { model | gramsFat = gramsFat }

        ChangeProtein proteins ->
            let
                gramsProtein =
                    Maybe.withDefault 0.0 (String.toFloat proteins)
            in
            update Recalculate { model | gramsProtein = gramsProtein }

        Recalculate ->
            let
                carbUnits =
                    model.gramsCarbohydrate / 10

                fatProteinUnits =
                    ((model.gramsFat * 9) + (model.gramsProtein * 4)) / 100

                carbUnitsPercentage =
                    carbUnits / (carbUnits + fatProteinUnits)

                bolusNow =
                    if carbUnitsPercentage < 0.2 then
                        0.0

                    else
                        carbUnits * 10 / model.carbRatio

                bolusLater =
                    if fatProteinUnits < 1.0 then
                        0.0

                    else if carbUnitsPercentage <= 0.8 then
                        fatProteinUnits * 10 / model.carbRatio

                    else
                        0.0

                bolusHours =
                    if fatProteinUnits < 1.0 || carbUnitsPercentage > 0.8 then
                        0.0

                    else if fatProteinUnits < 2.0 then
                        3.0

                    else if fatProteinUnits < 3.0 then
                        4.0

                    else if fatProteinUnits < 4.0 then
                        5.0

                    else
                        8.0
            in
            ( { model | bolusNow = bolusNow, bolusLater = bolusLater, bolusHours = bolusHours }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text "Carbohydrates (g)" ]
        , input [ placeholder "Carbohydrates (g)", value (String.fromFloat model.gramsCarbohydrate), onInput ChangeCarbohydrate ] []
        , div [] [ text "Fat (g)" ]
        , input [ placeholder "Fat (g)", value (String.fromFloat model.gramsFat), onInput ChangeFat ] []
        , div [] [ text "Protein (g)" ]
        , input [ placeholder "Protein (g)", value (String.fromFloat model.gramsProtein), onInput ChangeProtein ] []
        , div [] [ text ("Now: " ++ String.fromFloat model.bolusNow) ]
        , div [] [ text ("Later: " ++ String.fromFloat model.bolusLater) ]
        , div [] [ text ("Hours: " ++ String.fromFloat model.bolusHours) ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
