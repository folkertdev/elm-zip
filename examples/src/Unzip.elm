module Unzip exposing (main)

import Browser
import Bytes
import Bytes.Decode
import Bytes.Encode as Encode
import File
import File.Select
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Task
import Zip
import ZipDecode


type alias Model =
    ()


type Msg
    = SelectFileClicked
    | FileSelected File.File
    | FileLoaded Bytes.Bytes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectFileClicked ->
            ( model
            , File.Select.file [ "application/zip" ] FileSelected
            )

        FileSelected file ->
            ( model
            , Task.perform FileLoaded (File.toBytes file)
            )

        FileLoaded bytes ->
            let
                uncompressed =
                    Bytes.Decode.decode ZipDecode.decodeZipFile bytes
                        |> Debug.log "decoded"
            in
            ( model
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SelectFileClicked ] [ text "Upload file" ]
        ]


main : Program () Model Msg
main =
    Browser.document
        { init = \() -> ( (), Cmd.none )
        , view =
            \model ->
                { title = "page"
                , body = [ view model ]
                }
        , update = update
        , subscriptions = \_ -> Sub.none
        }
