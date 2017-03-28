module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)
import Task exposing (..)
import Process exposing (..)


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
    , duration : Time
    }


type alias Model =
    { flashElements : List FlashElement
    , nextId : Int
    , newText : String
    , newDuration : Time
    , newColor : String
    }


init : ( Model, Cmd Msg )
init =
    { flashElements = []
    , nextId = 0
    , newText = ""
    , newDuration = 0
    , newColor = ""
    }
        ! []



-- Update


type Msg
    = UpdateFormText String
    | UpdateFormDuration String
    | UpdateFormColor String
    | CreateFlashElement
    | DeleteFlashElement Int Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                newFlashElement =
                    { id = model.nextId
                    , text = model.newText
                    , color = model.newColor
                    , duration = model.newDuration
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
                    ! [ deleteCmd
                            model.nextId
                            newFlashElement.duration
                      ]

        DeleteFlashElement id time ->
            let
                newList =
                    List.filter
                        (\elem -> elem.id /= id)
                        model.flashElements
            in
                { model | flashElements = newList } ! []


deleteCmd : Int -> Time -> Cmd Msg
deleteCmd id duration =
    Process.sleep (duration * second)
        |> Task.perform
            (\_ -> DeleteFlashElement id duration)



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
    Sub.none
