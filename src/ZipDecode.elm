module ZipDecode exposing (decodeCentralDirectoryHeader, decodeEndOfCentralDirectory, decodeFile, decodeLocalFileHeader, decodeZipFile)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Decode.Extra as Decode
import Bytes.Extra


type alias ZipFile =
    { files : List ( LocalFileHeader, Bytes )
    , centrals : List CentralDirectoryHeader
    , end : EndOfCentralDirectory
    }


decodeZipFile : Decoder ZipFile
decodeZipFile =
    Decode.loop { files = [], centrals = [] } looper


{-| Decode all the parts

The zip file format does not give the size of each segment, instead each segment starts with a specific header.
So we decode that header, and branch on it to decode the appropriate.

-}
looper accum =
    u32
        |> Decode.andThen
            (\header ->
                case header of
                    0x04034B50 ->
                        -- local file header
                        decodeLocalFileHeader
                            |> Decode.andThen
                                (\localFileHeader ->
                                    Decode.bytes localFileHeader.compressedSize
                                        |> Decode.map
                                            (\content ->
                                                Loop { accum | files = ( localFileHeader, content ) :: accum.files }
                                            )
                                )

                    0x02014B50 ->
                        -- central directory header
                        decodeCentralDirectoryHeader
                            |> Decode.map
                                (\central ->
                                    Loop { accum | centrals = central :: accum.centrals }
                                )

                    0x06054B50 ->
                        -- end of central directory
                        decodeEndOfCentralDirectory
                            |> Decode.map
                                (\end ->
                                    Done { files = List.reverse accum.files, centrals = List.reverse accum.centrals, end = end }
                                )

                    _ ->
                        Debug.todo "unknown header" header
            )


decodeFile : Decoder { name : String, bytes : Bytes }
decodeFile =
    decodeLocalFileHeader
        |> Decode.andThen
            (\{ fileName, compressedSize } ->
                Decode.bytes compressedSize
                    |> Decode.map
                        (\content ->
                            { name = fileName, bytes = content }
                        )
            )


type alias LocalFileHeader =
    { versionToExtract : Int
    , generalPurposeBitFlag : Int
    , compressionMethod : Int
    , lastModifiedTime : Int
    , lastModifiedDate : Int
    , crc32 : Int
    , compressedSize : Int
    , uncompressedSize : Int
    , fileName : String
    , extraField : Bytes
    }


decodeLocalFileHeader : Decoder LocalFileHeader
decodeLocalFileHeader =
    let
        helper =
            Decode.map2 Tuple.pair u16 u16
                |> Decode.andThen
                    (\( fileNameLength, extraFieldLength ) ->
                        Decode.map2 Tuple.pair
                            (Decode.string fileNameLength)
                            (Decode.bytes extraFieldLength)
                    )
    in
    Decode.succeed LocalFileHeader
        |> keep u16
        |> keep u16
        |> keep u16
        |> keep u16
        |> keep u16
        |> keep u32
        |> keep u32
        |> keep u32
        -- NOTE: order of evaluating dfunction and the remaining fields!
        |> (\dfunction ->
                dfunction
                    |> Decode.andThen
                        (\function ->
                            u16
                                |> Decode.andThen
                                    (\a ->
                                        u16
                                            |> Decode.andThen
                                                (\b ->
                                                    Decode.succeed function
                                                        |> keep (Decode.string a)
                                                        |> keep (Decode.bytes b)
                                                )
                                    )
                        )
           )


type alias CentralDirectoryHeader =
    { versionMadeBy : Int
    , versionToExtract : Int
    , generalPurposeBitFlag : Int
    , compressionMethod : Int
    , lastModifiedTime : Int
    , lastModifiedDate : Int
    , crc32 : Int
    , compressedSize : Int
    , uncompressedSize : Int
    , diskNumberStart : Int
    , internalFileAttributes : Int
    , externalFileAttributes : Int
    , relativeOffset : Int
    , fileName : String
    , extraField : Bytes
    , fileComment : String
    }


type alias CentralDirectoryHeaderInternal =
    { versionMadeBy : Int
    , versionToExtract : Int
    , generalPurposeBitFlag : Int
    , compressionMethod : Int
    , lastModifiedTime : Int
    , lastModifiedDate : Int
    , crc32 : Int
    , compressedSize : Int
    , uncompressedSize : Int
    , fileNameLength : Int
    , extraFieldLength : Int
    , fileCommentLength : Int
    , diskNumberStart : Int
    , internalFileAttributes : Int
    , externalFileAttributes : Int
    , relativeOffset : Int
    }


decodeCentralDirectoryHeader =
    let
        helper internal name extra comment =
            { versionMadeBy = internal.versionMadeBy
            , versionToExtract = internal.versionToExtract
            , generalPurposeBitFlag = internal.generalPurposeBitFlag
            , compressionMethod = internal.compressionMethod
            , lastModifiedTime = internal.lastModifiedTime
            , lastModifiedDate = internal.lastModifiedDate
            , crc32 = internal.crc32
            , compressedSize = internal.compressedSize
            , uncompressedSize = internal.uncompressedSize
            , diskNumberStart = internal.diskNumberStart
            , internalFileAttributes = internal.internalFileAttributes
            , externalFileAttributes = internal.externalFileAttributes
            , relativeOffset = internal.relativeOffset
            , fileName = name
            , extraField = extra
            , fileComment = comment
            }
    in
    decodeCentralDirectoryHeaderInternal
        |> Decode.andThen
            (\internal ->
                Decode.succeed (helper internal)
                    |> keep (Decode.string internal.fileNameLength)
                    |> keep (Decode.bytes internal.extraFieldLength)
                    |> keep (Decode.string internal.fileCommentLength)
            )


decodeCentralDirectoryHeaderInternal =
    Decode.succeed CentralDirectoryHeaderInternal
        |> keep u16
        |> keep u16
        |> keep u16
        |> keep u16
        |> keep u16
        |> keep u16
        |> keep u32
        |> keep u32
        |> keep u32
        |> keep u16
        |> keep u16
        |> keep u16
        |> keep u16
        |> keep u16
        |> keep u32
        |> keep u32


type alias EndOfCentralDirectory =
    { diskNumber : Int
    , diskNumberStart : Int
    , diskEntries : Int
    , totalEntries : Int
    , size : Int
    , offset : Int
    , zipComment : String
    }


decodeEndOfCentralDirectory =
    Decode.succeed EndOfCentralDirectory
        |> keep u16
        |> keep u16
        |> keep u16
        |> keep u16
        |> keep u32
        |> keep u32
        |> keep (u16 |> Decode.andThen (\size -> Decode.string size))



-- HELPERS


u16 =
    Decode.unsignedInt16 LE


u32 =
    Decode.unsignedInt32 LE


keep : Decoder a -> Decoder (a -> b) -> Decoder b
keep arg func =
    Decode.map2 (<|) func arg


drop : Decoder a -> Decoder b -> Decoder b
drop ignoreDecoder keepDecoder =
    Decode.map2 always keepDecoder ignoreDecoder
