module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, a, div, form, h1, h2, hr, input, label, span, text)
import Html.Attributes exposing (class, for, href, placeholder, src, step, type_, value)
import Html.Events exposing (onInput)



---- MODEL ----


type alias Model =
    { gramsCarbohydrate : Maybe Float
    , gramsFat : Maybe Float
    , gramsProtein : Maybe Float
    , carbRatio : Maybe Float
    , bolusNow : Float
    , bolusLater : Float
    , bolusHours : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { gramsCarbohydrate = Nothing
      , gramsFat = Nothing
      , gramsProtein = Nothing
      , carbRatio = Just 10.0
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
    | ChangeCarbRatio String
    | Recalculate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeCarbohydrate carbohydrates ->
            update Recalculate { model | gramsCarbohydrate = String.toFloat carbohydrates }

        ChangeFat fats ->
            update Recalculate { model | gramsFat = String.toFloat fats }

        ChangeProtein proteins ->
            update Recalculate { model | gramsProtein = String.toFloat proteins }

        ChangeCarbRatio ratio ->
            update Recalculate { model | carbRatio = String.toFloat ratio }

        Recalculate ->
            case [ model.gramsCarbohydrate, model.gramsFat, model.gramsProtein, model.carbRatio ] of
                [ Just carbs, Just fats, Just proteins, Just carbRatio ] ->
                    let
                        ( bolusNow, bolusLater, bolusHours ) =
                            calculateEverything carbs fats proteins carbRatio
                    in
                    ( { model | bolusNow = bolusNow, bolusLater = bolusLater, bolusHours = bolusHours }, Cmd.none )

                _ ->
                    ( { model | bolusNow = 0.0, bolusLater = 0.0, bolusHours = 0.0 }, Cmd.none )


calculateEverything : Float -> Float -> Float -> Float -> ( Float, Float, Float )
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
    ( bolusNow, bolusLater, bolusHours )



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
                            [ class "form-control", placeholder "Carbohydrates (g)", value (renderMaybeFloat model.gramsCarbohydrate), type_ "number", step "0.1", onInput ChangeCarbohydrate ]
                            []
                        ]
                    , div [ class "form-group" ]
                        [ label [ for "fats" ] [ text "Fats (g)" ]
                        , input
                            [ class "form-control", placeholder "Fats (g)", value (renderMaybeFloat model.gramsFat), type_ "number", step "0.1", onInput ChangeFat ]
                            []
                        ]
                    , div [ class "form-group" ]
                        [ label [ for "proteins" ] [ text "Proteins (g)" ]
                        , input
                            [ class "form-control", placeholder "Proteins (g)", value (renderMaybeFloat model.gramsProtein), type_ "number", step "0.1", onInput ChangeProtein ]
                            []
                        ]
                    , div [ class "form-group" ]
                        [ label [ for "carbRatio" ] [ text "Carb Ratio" ]
                        , input
                            [ class "form-control", placeholder "Carb Ratio", value (renderMaybeFloat model.carbRatio), type_ "number", step "0.1", onInput ChangeCarbRatio ]
                            []
                        ]
                    ]
                , h2 [] [ text ("Now: " ++ renderMaybeFloat (Just model.bolusNow)) ]
                , h2 [] [ text ("Later: " ++ renderMaybeFloat (Just model.bolusLater)) ]
                , h2 [] [ text ("Hours: " ++ renderMaybeFloat (Just model.bolusHours) ++ "h") ]
                , div []
                    [ hr [] []
                    , span [] [ text "Source Code: " ]
                    , a [ href "https://github.com/mitchellhenke/ElmWarsawInsulin" ] [ text "https://github.com/mitchellhenke/ElmWarsawInsulin" ]
                    ]
                ]
            ]
        ]


renderMaybeFloat maybeFloat =
    case maybeFloat of
        Just float ->
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

        Nothing ->
            ""



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
