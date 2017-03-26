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
    , creationTime : Time
    , expirationTime : Time
    }


type alias Model =
    { flashElements : List FlashElement
    , nextId : Int
    , newText : String
    , newDuration : Time
    , currentTime : Time
    }


init : ( Model, Cmd Msg )
init =
    Model [] 4 "" 0 0
        ! []



-- Update


type Msg
    = NoOp
    | UpdateCurrentTime Time
    | TimeoutFlashElements Time
    | UpdateFormText String
    | UpdateFormDuration String
    | CreateFlashElement
    | DeleteFlashElement Time Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

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

        CreateFlashElement ->
            let
                expirationTime =
                    model.currentTime
                        + model.newDuration
                        * Time.second

                newFlashElement =
                    FlashElement model.nextId model.newText model.currentTime expirationTime

                newList =
                    newFlashElement :: model.flashElements
            in
                { model
                    | flashElements = newList
                    , nextId = model.nextId + 1
                    , newDuration = 0
                    , newText = ""
                }
                    ! []

        DeleteFlashElement time id ->
            let
                newList =
                    List.filter (\elem -> elem.id /= id) model.flashElements
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
    div []
        [ input [ value model.newText, onInput UpdateFormText ] []
        , input [ value (durationString model.newDuration), onInput UpdateFormDuration ] []
        , button [ onClick CreateFlashElement ] [ text "Create" ]
        ]


flashView : List FlashElement -> Html Msg
flashView elements =
    div []
        [ div []
            (flashViewElements elements)
        ]


flashViewElements : List FlashElement -> List (Html Msg)
flashViewElements elements =
    List.map (\elem -> div [ class "alert alert-warning" ] [ text elem.text ]) elements


view : Model -> Html Msg
view model =
    div []
        [ flashView model.flashElements
        , (form model)
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ every second UpdateCurrentTime
        , every second TimeoutFlashElements
        ]
