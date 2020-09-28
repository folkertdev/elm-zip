module Main exposing (main)

import Browser
import Bytes.Encode as Encode
import File.Download
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import ZipEncode


myFiles =
    [ { name = "test.txt"
      , bytes = Encode.encode (Encode.string fooText)
      }
        , { name = "lorum.txt"
          , bytes = Encode.encode (Encode.string loremText)
          }
    ]


fooText =
    """foo bar baz
"""


loremText =
    """lorem ipsum
"""


type alias Model =
    { count : Int }


initialModel : Model
initialModel =
    { count = 0 }


type Msg
    = Increment
    | Decrement


download files =
    let
        bytes =
            ZipEncode.constructZip files
    in
    File.Download.bytes "test.zip" "application/zip" bytes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }
              -- , Cmd.none
            , download myFiles
            )

        Decrement ->
            ( { model | count = model.count - 1 }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "+1" ]
        , div [] [ text <| String.fromInt model.count ]
        , button [ onClick Decrement ] [ text "-1" ]
        ]


main : Program () Model Msg
main =
    Browser.document
        { init = \() -> ( initialModel, Cmd.none )
        , view = \model -> { title = "page", body = [ view model ] }
        , update = update
        , subscriptions = \_ -> Sub.none
        }
