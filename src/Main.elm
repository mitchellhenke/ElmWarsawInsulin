port module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, a, div, form, h1, h2, hr, input, label, span, text)
import Html.Attributes exposing (class, for, href, placeholder, src, value)
import Html.Events exposing (onInput)


port carbRatioCache : String -> Cmd msg



---- MODEL ----


type alias Model =
    { gramsCarbohydrate : String
    , gramsFat : String
    , gramsProtein : String
    , carbRatio : String
    , bolusNow : Float
    , bolusLater : Float
    , bolusHours : Float
    , fatProteinUnits : Float
    }


type alias CalculationResult =
    { bolusNow : Float
    , bolusLater : Float
    , bolusHours : Float
    , fatProteinUnits : Float
    }


init : String -> ( Model, Cmd Msg )
init cached =
    ( { gramsCarbohydrate = ""
      , gramsFat = ""
      , gramsProtein = ""
      , carbRatio = cached
      , bolusNow = 0.0
      , bolusLater = 0.0
      , bolusHours = 0.0
      , fatProteinUnits = 0.0
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ChangeCarbohydrate String
    | ChangeFat String
    | ChangeProtein String
    | ChangeCarbRatio String
    | Recalculate (Cmd Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeCarbohydrate carbohydrates ->
            update (Recalculate Cmd.none) { model | gramsCarbohydrate = carbohydrates }

        ChangeFat fats ->
            update (Recalculate Cmd.none) { model | gramsFat = fats }

        ChangeProtein proteins ->
            update (Recalculate Cmd.none) { model | gramsProtein = proteins }

        ChangeCarbRatio ratio ->
            let
                updatedModel =
                    { model | carbRatio = ratio }
            in
            update (Recalculate (carbRatioCache ratio)) updatedModel

        Recalculate cmd ->
            case [ String.toFloat model.gramsCarbohydrate, String.toFloat model.gramsFat, String.toFloat model.gramsProtein, String.toFloat model.carbRatio ] of
                [ Just carbs, Just fats, Just proteins, Just carbRatio ] ->
                    let
                        { bolusNow, bolusLater, bolusHours, fatProteinUnits } =
                            calculateEverything carbs fats proteins carbRatio
                    in
                    ( { model | bolusNow = bolusNow, bolusLater = bolusLater, bolusHours = bolusHours, fatProteinUnits = fatProteinUnits }, cmd )

                _ ->
                    ( { model | bolusNow = 0.0, bolusLater = 0.0, bolusHours = 0.0, fatProteinUnits = 0.0 }, cmd )


calculateEverything : Float -> Float -> Float -> Float -> CalculationResult
calculateEverything carbs fats proteins carbRatio =
    let
        carbUnits =
            carbs / 10

        fatProteinUnits =
            ((fats * 9) + (proteins * 4)) / 100

        carbUnitsPercentage =
            carbUnits / (carbUnits + fatProteinUnits)

        bolusNow =
            if carbUnitsPercentage < 0.2 then
                0.0

            else
                carbUnits * 10 / carbRatio

        bolusLater =
            if fatProteinUnits < 1.0 then
                0.0

            else if carbUnitsPercentage <= 0.8 then
                fatProteinUnits * 10 / carbRatio

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
    { bolusNow = bolusNow, bolusLater = bolusLater, bolusHours = bolusHours, fatProteinUnits = fatProteinUnits * 10 }



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ] [ div [ class "col-sm-12" ] [ h1 [] [ text "Calculator" ] ] ]
        , div [ class "row" ]
            [ div [ class "col-sm-12" ]
                [ form []
                    [ div [ class "form-group" ]
                        [ label [ for "carbs" ] [ text "Carbohydrates (g)" ]
                        , input
                            [ class "form-control", placeholder "Carbohydrates (g)", value model.gramsCarbohydrate, onInput ChangeCarbohydrate ]
                            []
                        ]
                    , div [ class "form-group" ]
                        [ label [ for "fats" ] [ text "Fats (g)" ]
                        , input
                            [ class "form-control", placeholder "Fats (g)", value model.gramsFat, onInput ChangeFat ]
                            []
                        ]
                    , div [ class "form-group" ]
                        [ label [ for "proteins" ] [ text "Proteins (g)" ]
                        , input
                            [ class "form-control", placeholder "Proteins (g)", value model.gramsProtein, onInput ChangeProtein ]
                            []
                        ]
                    , div [ class "form-group" ]
                        [ label [ for "carbRatio" ] [ text "Carb Ratio" ]
                        , input
                            [ class "form-control", placeholder "Carb Ratio", value model.carbRatio, onInput ChangeCarbRatio ]
                            []
                        ]
                    ]
                , h2 [] [ text ("Now: " ++ renderFloat model.bolusNow ++ "u") ]
                , h2 [] [ text ("Later: " ++ renderFloat model.bolusLater ++ "u") ]
                , h2 [] [ text ("Hours: " ++ renderFloat model.bolusHours ++ "h") ]
                , h2 [] [ text ("Fat/Protein Units: " ++ renderFloat model.fatProteinUnits ++ "g") ]
                , div []
                    [ a [ href "https://github.com/mitchellhenke/ElmWarsawInsulin" ] [ text "Source Code" ]
                    ]
                ]
            ]
        ]


renderFloat float =
    let
        string =
            String.fromFloat float

        splitString =
            String.split "." string

        first =
            List.head splitString

        end =
            List.tail splitString

        final =
            case ( first, end ) of
                ( Just integerPart, Just [ floatPart ] ) ->
                    integerPart
                        ++ "."
                        ++ String.left 2 floatPart

                _ ->
                    string
    in
    final



---- PROGRAM ----


main : Program String Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
