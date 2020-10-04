module Zip exposing
    ( withoutCompression, withDeflateCompression
    , Entry, stringEntry, bytesEntry, fileEntry
    , ZipFile, readZipFile
    , extractFile, extractStringFile
    , bytesToString
    )

{-| Create & Extract Zip files


# Create Zip File

@docs withoutCompression, withDeflateCompression


# Create zip file entries

@docs Entry, stringEntry, bytesEntry, fileEntry


# Extract files from a zip file

@docs ZipFile, readZipFile

@docs extractFile, extractStringFile

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

    stringEntry "foo.txt" "foo bar baz"

-}
stringEntry : String -> String -> Entry
stringEntry name content =
    StringData { name = name, content = content }


{-| Create a binary data entry

    import Bytes.Encode

    stringEntry "bytes.bc" (Bytes.Encode.encode [])

-}
bytesEntry : String -> Bytes -> Entry
bytesEntry name content =
    BytesData { name = name, content = content }


{-| Create an entry from a `elm/file` `File`.
-}
fileEntry : File.File -> Task.Task x Entry
fileEntry file =
    let
        name =
            File.name file
    in
    file
        |> File.toBytes
        |> Task.map (\content -> BytesData { name = name, content = content })


toFileRecord : CompressionMethod -> Entry -> ZipEncode.File
toFileRecord compression entry =
    let
        -- general note: when compression grows the size of the data (e.g. for small files)
        -- actually use no compression
        helper name bytes =
            let
                uncompressedSize =
                    Bytes.width bytes

                compressedBytes =
                    case compression of
                        NoCompression ->
                            bytes

                        DeflateCompression ->
                            Flate.deflate bytes

                compressedSize =
                    Bytes.width compressedBytes
            in
            { name = name
            , content =
                if compressedSize < uncompressedSize then
                    compressedBytes

                else
                    bytes
            , uncompressedSize = uncompressedSize
            , compressedSize = min compressedSize uncompressedSize
            , compression =
                if compressedSize < uncompressedSize then
                    compression

                else
                    NoCompression
            , crc = Flate.crc32 bytes
            }
    in
    case entry of
        StringData { name, content } ->
            let
                bytes =
                    Encode.encode (Encode.string content)
            in
            helper name bytes

        BytesData { name, content } ->
            helper name content


{-| Create a zip file without compressing any of the file contents

    withoutCompression
        [ stringEntry "foo.elm" "module Foo exposing (..)"
        , stringEntry "bar.hs" "module Foo where"
        ]

-}
withoutCompression : List Entry -> Bytes
withoutCompression entries =
    create NoCompression entries


{-| Create a zip file, compressing file contents with the [DEFLATE](https://en.wikipedia.org/wiki/DEFLATE) algorithm

    withDeflateCompression
        [ stringEntry "foo.elm" "module Foo exposing (..)"
        , stringEntry "bar.hs" "module Foo where"
        ]

-}
withDeflateCompression : List Entry -> Bytes
withDeflateCompression entries =
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


readZipFile : Bytes -> Maybe ZipFile
readZipFile buffer =
    ZipDecode.readZipFile buffer
        |> Maybe.map ZipFile


extractFile : String -> ZipFile -> Maybe Bytes
extractFile name (ZipFile zipfile) =
    case Dict.get name zipfile.uncompressedFiles of
        Just content ->
            Just content

        Nothing ->
            case Dict.get name zipfile.possiblyCompressedFiles of
                Nothing ->
                    Nothing

                Just { header, compressedContent } ->
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


extractStringFile : String -> ZipFile -> Maybe String
extractStringFile name zipfile =
    extractFile name zipfile
        |> Maybe.andThen bytesToString


extractFiles : ZipFile -> Dict String Bytes
extractFiles (ZipFile zipfile) =
    let
        folder key { header, compressedContent } accum =
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
    Dict.foldl folder zipfile.uncompressedFiles zipfile.possiblyCompressedFiles


extractFilesStep : Int -> ZipFile -> Decode.Step ZipFile (Dict String Bytes)
extractFilesStep maxSize (ZipFile zipfile) =
    case extractFilesStepHelp maxSize (Dict.toList zipfile.possiblyCompressedFiles) zipfile.uncompressedFiles of
        Decode.Done uncompressed ->
            Decode.Done uncompressed

        Decode.Loop ( newPossiblyCompressed, newUncompressed ) ->
            { zipfile
                | possiblyCompressedFiles = Dict.fromList newPossiblyCompressed
                , uncompressedFiles = newUncompressed
            }
                |> ZipFile
                |> Decode.Loop


extractFilesStepHelp maxSize possiblyCompressedFiles uncompressedFiles =
    case possiblyCompressedFiles of
        [] ->
            Decode.Done uncompressedFiles

        ( key, { header, compressedContent } ) :: rest ->
            case header.compressionMethod of
                0 ->
                    extractFilesStepHelp maxSize rest (Dict.insert key compressedContent uncompressedFiles)

                8 ->
                    if Bytes.width compressedContent < maxSize then
                        case Flate.inflate compressedContent of
                            Just inflated ->
                                extractFilesStepHelp
                                    (max 0 (maxSize - Bytes.width compressedContent))
                                    rest
                                    (Dict.insert key inflated uncompressedFiles)

                            Nothing ->
                                Decode.Loop ( possiblyCompressedFiles, uncompressedFiles )

                    else
                        extractFilesStepHelp
                            maxSize
                            rest
                            uncompressedFiles

                _ ->
                    extractFilesStepHelp
                        maxSize
                        rest
                        uncompressedFiles



-- HELPERS


{-| Attempt to convert a `Bytes` to a `String`

Can be used to convert the binary contents of a file to its string representation.

-}
bytesToString : Bytes -> Maybe String
bytesToString buffer =
    Decode.decode (Decode.string (Bytes.width buffer)) buffer
