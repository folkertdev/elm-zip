module Incremental exposing (main)

import Browser
import Bytes
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import File
import File.Select
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Process
import Task
import Zip


type alias Model =
    { zipfile : Maybe Zip.ZipFile }


type Msg
    = SelectFileClicked
    | FileSelected File.File
    | FileLoaded Bytes.Bytes
    | Decompress Zip.ZipFile


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
            ( { zipfile = Zip.read bytes }
            , Cmd.none
            )

        Decompress zip ->
            let
                extracted =
                    Zip.extract
                        { count = 1
                        , maxSize = Nothing
                        , hasStringContent = 
                            \filename ->
                                filename |> String.endsWith ".txt" 
                        }
                        zip

               
            in
            case extracted of
                Zip.Done decompressedFiles ->
                     -- do some work once everything is decompressed
                    ( model, Cmd.none )

                Zip.Loop zipped ->
                    let
                         progress =
                            Zip.progress zipped
                    in
                    ( model
                    , Task.perform
                        (always (Decompress zipped))
                        (Process.sleep 1)
                    )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SelectFileClicked ] [ text "Upload file" ]
        , text (Debug.toString model.zipfile)
        ]


main : Program () Model Msg
main =
    Browser.document
        { init = \() -> ( { zipfile = Nothing }, Cmd.none )
        , view =
            \model ->
                { title = "page"
                , body = [ view model ]
                }
        , update = update
        , subscriptions = \_ -> Sub.none
        }
