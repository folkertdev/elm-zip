module Zip exposing
    ( Entry, stringEntry, bytesEntry
    , withoutCompression, withDeflateCompression
    )

{-| Create Zip files

@docs Entry, stringEntry, bytesEntry

@docs withoutCompression, withDeflateCompression

-}

import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Flate
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


{-| Create a string entry

    import Bytes.Encode

    stringEntry "bytes.bc" (Bytes.Encode.encode [])

-}
bytesEntry : String -> Bytes -> Entry
bytesEntry name content =
    BytesData { name = name, content = content }


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
-}
withoutCompression : List Entry -> Bytes
withoutCompression entries =
    create NoCompression entries


{-| Create a zip file, compressing file contents with the [DEFLATE](https://en.wikipedia.org/wiki/DEFLATE) algorithm
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



{-
   decode : Bytes -> List { name : String, content : Bytes }
   decode buffer =
       case Decode.decode ZipDecode.decodeZipFile buffer of
           Just result ->
               result.files

           Nothing ->
               []


   bytesToString : Bytes -> Maybe String
   bytesToString buffer =
       Decode.decode (Decode.string (Bytes.width buffer)) buffer
-}
