module Main exposing (main)

import Browser
import Bytes.Encode as Encode
import File.Download
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Zip


myFiles =
    -- [ Zip.stringEntry "test.txt"  "besit stjert, sibben stjerre; dosels stjerst as harren; mar ien ding wit ik, dat iivig libbet"
    [ Zip.stringEntry "test.txt"  "foo bar baz\n"
    -- , Zip.stringEntry "lorum.txt" "lorem ipsum\n"
    --[ Zip.stringEntry "laulu.txt" laulu 

    ]

laulu = 
    """Jokaisille loistaen
Näyttävät ne tien
Karttaan piirtäen
Hiljainena tuikkeena
Kuiskausten kaikuna
Kuukan ei kuule
Niiden loputonta laulua

Laulakka, hiljaa laulakaa
Toisinne valovuosien matka
Olkaa valona sen kaipussa
Laulaka laulanne
Mutta kukaan ei kuule..
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
            if True then
                Zip.withoutCompression files
            else 
                Zip.withDeflateCompression files
    in
    File.Download.bytes "test.zip" "application/zip" bytes


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | count = model.count + 1 }
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
