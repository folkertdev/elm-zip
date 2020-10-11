module Zip exposing
    ( uncompressed, compressed
    , Entry, string, bytes, file
    , ZipFile, overview, read
    , extractFile, extractStringFile
    , extractAll, Content(..)
    , extract, Step(..), progress
    , bytesToString
    )

{-| Create & Extract Zip files


# Create Zip File

@docs uncompressed, compressed


# Create zip file entries

@docs Entry, string, bytes, file


# Extract files from a zip file

@docs ZipFile, overview, read

@docs extractFile, extractStringFile

@docs extractAll, Content

@docs extract, Step, progress

@docs bytesToString

-}

import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Dict exposing (Dict)
import File
import Flate
import Task
import ZipDecode
import ZipEncode exposing (CompressionMethod(..))


{-| An entry (file) that will be part of the Zip file
-}
type Entry
    = StringData { name : String, content : String }
    | BytesData { name : String, content : Bytes }


{-| Create a string entry

    Zip.string "foo.txt" "foo bar baz"

-}
string : String -> String -> Entry
string name content =
    StringData { name = name, content = content }


{-| Create a binary data entry

    import Bytes.Encode

    Zip.bytes "bytes.bc" (Bytes.Encode.encode [])

-}
bytes : String -> Bytes -> Entry
bytes name content =
    BytesData { name = name, content = content }


{-| Create an entry from a `elm/file` `File`.
-}
file : File.File -> Task.Task x Entry
file fileEntry =
    let
        name =
            File.name fileEntry
    in
    fileEntry
        |> File.toBytes
        |> Task.map (\content -> BytesData { name = name, content = content })


toFileRecord : CompressionMethod -> Entry -> ZipEncode.File
toFileRecord compression entry =
    let
        -- general note: when compression grows the size of the data (e.g. for small files)
        -- actually use no compression
        helper name bytesContent =
            let
                uncompressedSize =
                    Bytes.width bytesContent

                compressedBytes =
                    case compression of
                        NoCompression ->
                            bytesContent

                        DeflateCompression ->
                            Flate.deflate bytesContent

                compressedSize =
                    Bytes.width compressedBytes
            in
            { name = name
            , content =
                if compressedSize < uncompressedSize then
                    compressedBytes

                else
                    bytesContent
            , uncompressedSize = uncompressedSize
            , compressedSize = min compressedSize uncompressedSize
            , compression =
                if compressedSize < uncompressedSize then
                    compression

                else
                    NoCompression
            , crc = Flate.crc32 bytesContent
            }
    in
    case entry of
        StringData { name, content } ->
            let
                bytesContent =
                    Encode.encode (Encode.string content)
            in
            helper name bytesContent

        BytesData { name, content } ->
            helper name content


{-| Create a zip file without compressing any of the file contents

    Zip.uncompressed
        [ Zip.string "foo.elm" "module Foo exposing (..)"
        , Zip.string "bar.hs" "module Foo where"
        ]

-}
uncompressed : List Entry -> Bytes
uncompressed entries =
    create NoCompression entries


{-| Create a zip file, compressing file contents with the [DEFLATE](https://en.wikipedia.org/wiki/DEFLATE) algorithm

    Zip.compressed
        [ Zip.string "foo.elm" "module Foo exposing (..)"
        , Zip.string "bar.hs" "module Foo where"
        ]

-}
compressed : List Entry -> Bytes
compressed entries =
    create DeflateCompression entries


create : CompressionMethod -> List Entry -> Bytes
create compression entries =
    let
        bytesEntries =
            List.map (toFileRecord compression) entries
    in
    ZipEncode.constructZip bytesEntries



-- DECODING


type ZipFile
    = ZipFile ZipDecode.ZipFile


{-| -}
overview :
    ZipFile
    ->
        List
            { filename : String
            , compressedSize : Int
            , uncompressedSize : Int
            }
overview (ZipFile zip) =
    List.map
        (\header ->
            { filename = header.fileName
            , compressedSize = header.compressedSize
            , uncompressedSize = header.uncompressedSize
            }
        )
        zip.centrals


read : Bytes -> Maybe ZipFile
read buffer =
    ZipDecode.readZipFile buffer
        |> Maybe.map ZipFile


extractFile : String -> ZipFile -> Maybe Bytes
extractFile name (ZipFile zipfile) =
    case match name zipfile.uncompressedFiles of
        Just ( filename, content ) ->
            Just content

        Nothing ->
            case match name zipfile.possiblyCompressedFiles of
                Nothing ->
                    Nothing

                Just ( filename, { header, compressedContent } ) ->
                    case header.compressionMethod of
                        0 ->
                            Just compressedContent

                        8 ->
                            case Flate.inflate compressedContent of
                                Just inflated ->
                                    Just inflated

                                Nothing ->
                                    Nothing

                        _ ->
                            Nothing


match targetFilename files =
    case files of
        [] ->
            Nothing

        ( name, content ) :: remain ->
            if targetFilename == name then
                Just ( name, content )

            else
                match targetFilename files


extractStringFile : String -> ZipFile -> Maybe String
extractStringFile name zipfile =
    extractFile name zipfile
        |> Maybe.andThen bytesToString


{-| -}
extractAll : ZipFile -> Dict String Bytes
extractAll (ZipFile zipfile) =
    let
        folder ( key, { header, compressedContent } ) accum =
            case header.compressionMethod of
                0 ->
                    Dict.insert key compressedContent accum

                8 ->
                    case Flate.inflate compressedContent of
                        Just inflated ->
                            Dict.insert key inflated accum

                        Nothing ->
                            accum

                _ ->
                    accum
    in
    List.foldl folder (Dict.fromList zipfile.uncompressedFiles) zipfile.possiblyCompressedFiles


{-| -}
progress : ZipFile -> { uncompressed : Int, compressed : Int, total : Int }
progress (ZipFile zip) =
    let
        uncompressedCount =
            List.length zip.uncompressedFiles

        compressedCount =
            List.length zip.possiblyCompressedFiles
    in
    { uncompressed = uncompressedCount
    , compressed = compressedCount
    , total = uncompressedCount + compressedCount
    }


{-| -}
type Step
    = Loop ZipFile
    | Done (List ( String, Content ))


{-| -}
type Content
    = StringContent String
    | BytesContent Bytes
    | Failed Bytes


{-| This function will allow you to incrementally decompress a zip file.

Check out the `examples/src/Incremental.elm` example for a working example but

    Decompress zip ->
        let
            extracted =
                -- here we specify the number of files to attempt in this go
                --
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
                -- There is still more to decompress
                let
                    -- retrieve some data representing the current level of progress
                    progress =
                        Zip.progress zipped
                in
                ( model
                -- Here we're waiting till the next frame before doing any more work
                -- this allows us to report progress to the user incrementally
                , Task.perform
                    (always (Decompress zipped))
                    (Process.sleep 1)
                )

-}
extract :
    { maxSize : Maybe Int
    , count : Int
    , hasStringContent : String -> Bool
    }
    -> ZipFile
    -> Step
extract config (ZipFile zipfile) =
    case extractHelp config 0 zipfile.possiblyCompressedFiles zipfile.uncompressedFiles of
        Decode.Done extractedFiles ->
            Done (interpretStringContent config extractedFiles [])

        Decode.Loop ( newPossiblyCompressed, newUncompressed ) ->
            { zipfile
                | possiblyCompressedFiles = newPossiblyCompressed
                , uncompressedFiles = newUncompressed
            }
                |> ZipFile
                |> Loop


interpretStringContent :
    { maxSize : Maybe Int
    , count : Int
    , hasStringContent : String -> Bool
    }
    -> List ( String, Bytes )
    -> List ( String, Content )
    -> List ( String, Content )
interpretStringContent config files captured =
    case files of
        [] ->
            captured

        ( filename, byteContent ) :: remain ->
            if config.hasStringContent filename then
                case bytesToString byteContent of
                    Nothing ->
                        interpretStringContent config
                            remain
                            (( filename, Failed byteContent ) :: captured)

                    Just stringContent ->
                        interpretStringContent config
                            remain
                            (( filename, StringContent stringContent ) :: captured)

            else
                interpretStringContent config
                    remain
                    (( filename, BytesContent byteContent ) :: captured)


extractHelp config index possiblyCompressedFiles uncompressedFiles =
    case possiblyCompressedFiles of
        [] ->
            Decode.Done uncompressedFiles

        ( key, { header, compressedContent } ) :: rest ->
            case header.compressionMethod of
                0 ->
                    extractHelp config
                        index
                        rest
                        (( key, compressedContent ) :: uncompressedFiles)

                8 ->
                    let
                        belowMaxSize =
                            case config.maxSize of
                                Nothing ->
                                    True

                                Just maxSize ->
                                    Bytes.width compressedContent < maxSize
                    in
                    if index < config.count && belowMaxSize then
                        case Flate.inflate compressedContent of
                            Just inflated ->
                                extractHelp
                                    config
                                    (index + 1)
                                    rest
                                    (( key, inflated ) :: uncompressedFiles)

                            Nothing ->
                                let
                                    _ =
                                        Debug.log "failed to inflate" key
                                in
                                extractHelp
                                    config
                                    index
                                    rest
                                    uncompressedFiles

                    else
                        Decode.Loop
                            ( possiblyCompressedFiles, uncompressedFiles )

                _ ->
                    extractHelp
                        config
                        index
                        rest
                        uncompressedFiles



-- HELPERS


{-| Attempt to convert a `Bytes` to a `String`

Can be used to convert the binary contents of a file to its string representation.

-}
bytesToString : Bytes -> Maybe String
bytesToString buffer =
    Decode.decode (Decode.string (Bytes.width buffer)) buffer
