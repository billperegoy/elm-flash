module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias FlashElement =
    { id : Int
    , text : String
    , color : String
    , expirationTime : Time
    }


type alias Model =
    { flashElements : List FlashElement
    , nextId : Int
    , newText : String
    , newDuration : Time
    , newColor : String
    , currentTime : Time
    , subscriptions : List (Sub Msg)
    }


init : ( Model, Cmd Msg )
init =
    { flashElements = []
    , nextId = 0
    , newText = ""
    , newDuration = 0
    , newColor = ""
    , currentTime = 0
    , subscriptions =
        [ every second UpdateCurrentTime
        , every second TimeoutFlashElements
        , every (60 * second) (DeleteFlashElement 0)
        ]
    }
        ! []



-- Update


type Msg
    = UpdateCurrentTime Time
    | TimeoutFlashElements Time
    | UpdateFormText String
    | UpdateFormDuration String
    | UpdateFormColor String
    | CreateFlashElement
    | DeleteFlashElement Int Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateCurrentTime time ->
            { model | currentTime = time } ! []

        TimeoutFlashElements time ->
            let
                newList =
                    List.filter
                        (\elem -> elem.expirationTime > time)
                        model.flashElements
            in
                { model | flashElements = newList } ! []

        UpdateFormText text ->
            { model | newText = text } ! []

        UpdateFormDuration durationString ->
            let
                duration =
                    String.toFloat durationString
                        |> Result.withDefault 0
            in
                { model | newDuration = duration } ! []

        UpdateFormColor color ->
            { model | newColor = color } ! []

        CreateFlashElement ->
            let
                expirationTime =
                    model.currentTime
                        + (model.newDuration * Time.second)

                newFlashElement =
                    { id = model.nextId
                    , text = model.newText
                    , color = model.newColor
                    , expirationTime = expirationTime
                    }

                newList =
                    newFlashElement :: model.flashElements
            in
                { model
                    | flashElements = newList
                    , nextId = model.nextId + 1
                    , newDuration = 0
                    , newText = ""
                    , newColor = ""
                }
                    ! []

        DeleteFlashElement id time ->
            let
                newList =
                    List.filter
                        (\elem -> elem.id /= id)
                        model.flashElements
            in
                { model | flashElements = newList } ! []



-- View


durationString : Time -> String
durationString val =
    if val == 0 then
        ""
    else
        toString val


form : Model -> Html Msg
form model =
    div [ class "form-group" ]
        [ label [] [ text "Text" ]
        , input [ class "form-control", value model.newText, onInput UpdateFormText ] []
        , label [] [ text "Timeout (seconds)" ]
        , input [ class "form-control", value (durationString model.newDuration), onInput UpdateFormDuration ] []
        , label [] [ text "Type (sucess, info, warning, danger)" ]
        , input [ class "form-control", value model.newColor, onInput UpdateFormColor ] []
        , button [ type_ "submit", class "btn btn-default", onClick CreateFlashElement ] [ text "Create" ]
        ]


flashView : List FlashElement -> Html Msg
flashView elements =
    div []
        (flashViewElements elements)


flashViewElements : List FlashElement -> List (Html Msg)
flashViewElements elements =
    List.map
        (\elem ->
            div
                [ class ("alert alert-" ++ elem.color) ]
                [ text elem.text ]
        )
        elements


view : Model -> Html Msg
view model =
    div [ style [ ( "width", "400px" ), ( "margin", "40px" ) ] ]
        [ flashView model.flashElements
        , form model
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch model.subscriptions
