module ZipDecode exposing (ZipFile, decodeCentralDirectoryHeader, decodeEndOfCentralDirectory, decodeFile, decodeLocalFileHeader, readZipFile)

import Bitwise
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as Decode exposing (Decoder, Step(..))
import Bytes.Decode.Extra as Decode
import Bytes.Extra
import Dict exposing (Dict)


endOfCentralDirectorySize : Int
endOfCentralDirectorySize =
    22


type alias ZipFile =
    { possiblyCompressedFiles :
        Dict String
            { header : LocalFileHeader
            , compressedContent : Bytes
            }
    , uncompressedFiles : Dict String Bytes
    , centrals : List CentralDirectoryHeader
    , end : EndOfCentralDirectory
    }


readZipFile : Bytes -> Maybe ZipFile
readZipFile buffer =
    case readEndOfCentralDirectory buffer of
        Nothing ->
            Nothing

        Just end ->
            case readCentralDirectory buffer end of
                Nothing ->
                    Nothing

                Just centrals ->
                    case readLocalFiles buffer centrals of
                        Nothing ->
                            Nothing

                        Just locals ->
                            Just
                                { possiblyCompressedFiles = locals
                                , uncompressedFiles = Dict.empty
                                , centrals = centrals
                                , end = end
                                }


readEndOfCentralDirectory : Bytes -> Maybe EndOfCentralDirectory
readEndOfCentralDirectory buffer =
    let
        endBuffer =
            Bytes.Extra.drop (Bytes.width buffer - endOfCentralDirectorySize) buffer
    in
    case Decode.decode decodeEndOfCentralDirectory endBuffer of
        Just x ->
            Just x

        Nothing ->
            -- TODO try to find the header
            Nothing


readCentralDirectory : Bytes -> EndOfCentralDirectory -> Maybe (List CentralDirectoryHeader)
readCentralDirectory buffer end =
    let
        centralBuffer =
            Bytes.Extra.drop (Bytes.width buffer - (endOfCentralDirectorySize + end.size)) buffer
    in
    case Decode.decode (decodeCentralDirectory end.totalEntries) centralBuffer of
        Just x ->
            Just x

        Nothing ->
            Nothing


readLocalFiles : Bytes -> List CentralDirectoryHeader -> Maybe (Dict String { header : LocalFileHeader, compressedContent : Bytes })
readLocalFiles buffer centrals =
    case Decode.decode (decodeLocalFileHeaders centrals) buffer of
        Just x ->
            Just x

        Nothing ->
            Nothing



-- LOCAL FILE


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


decodeLocalFileHeaders : List CentralDirectoryHeader -> Decoder (Dict String { header : LocalFileHeader, compressedContent : Bytes })
decodeLocalFileHeaders headers =
    Decode.loop { headers = headers, files = Dict.empty } decodeLocalFileHeadersHelp


decodeCompressedContent { compressedSize } =
    Decode.bytes compressedSize


decodeLocalFileHeadersHelp { headers, files } =
    case headers of
        [] ->
            Decode.succeed (Done files)

        first :: rest ->
            let
                finalize file =
                    let
                        result =
                            Loop
                                { headers = rest
                                , files = Dict.insert first.fileName file files
                                }
                    in
                    if Bitwise.and 8 file.header.generalPurposeBitFlag > 0 then
                        -- there is some extra stuff we should decode here
                        Decode.succeed result
                            |> drop decodeDataDescriptor

                    else
                        Decode.succeed result
            in
            Decode.succeed
                (\localFileHeader content ->
                    { header = localFileHeader, compressedContent = content }
                )
                |> keep decodeLocalFileHeader
                |> keep (decodeCompressedContent first)
                |> Decode.andThen finalize


decodeDataDescriptor =
    u32
        |> Decode.andThen
            (\found ->
                if found == 0x08074B50 then
                    -- three fields remain
                    Decode.succeed ()
                        |> drop u32
                        |> drop u32
                        |> drop u32

                else
                    -- the first field was the crc32, two fields remain
                    Decode.succeed ()
                        |> drop u32
                        |> drop u32
            )


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
        |> drop (header 0x04034B50)
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



-- CENTRAL DIRECTORY


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


decodeCentralDirectory n =
    Decode.list n decodeCentralDirectoryHeader


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
        |> drop (header 0x02014B50)
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



-- END OF CENTRAL DIRECTORY


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
        |> drop (header 0x06054B50)
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


header constant =
    u32
        |> Decode.andThen
            (\found ->
                if found == constant then
                    Decode.succeed ()

                else
                    failure ("invalid header found: " ++ String.fromInt found ++ " instead of " ++ String.fromInt constant)
            )


keep : Decoder a -> Decoder (a -> b) -> Decoder b
keep arg func =
    Decode.map2 (<|) func arg


drop : Decoder a -> Decoder b -> Decoder b
drop ignoreDecoder keepDecoder =
    Decode.map2 always keepDecoder ignoreDecoder


{-| Helper for development
-}
failure : String -> Decoder a
failure message =
    -- let _ = Debug.log "error" message in
    Decode.fail
