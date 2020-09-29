module Zip exposing
    ( withoutCompression, withDeflateCompression
    , Entry, stringEntry, bytesEntry, fileEntry
    , decodeToBytes, bytesToString
    )

{-| Create & Extract Zip files


# Create Zip File

@docs withoutCompression, withDeflateCompression


# Create zip file entries

@docs Entry, stringEntry, bytesEntry, fileEntry


# Extract files from a zip file

@docs decodeToBytes, bytesToString

-}

import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
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


{-| Convert a zip file into its constituent files

A file is represented as a name and a `Bytes`, representing the uncompressed contents of the file.

-}
decodeToBytes : Bytes -> Maybe (List { name : String, content : Bytes })
decodeToBytes buffer =
    case Decode.decode ZipDecode.decodeZipFile buffer of
        Just result ->
            result.files
                |> List.map (\( header, content ) -> { name = header.fileName, content = content })
                |> Just

        Nothing ->
            Nothing


{-| Attempt to convert a `Bytes` to a `String`

Can be used to convert the binary contents of a file to its string representation.

-}
bytesToString : Bytes -> Maybe String
bytesToString buffer =
    Decode.decode (Decode.string (Bytes.width buffer)) buffer
