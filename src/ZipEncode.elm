module ZipEncode exposing (..)

import Bytes exposing (Bytes, Endianness(..))
import Bytes.Encode as Encode exposing (Encoder)
import Flate


type CompressionMethod
    = NoCompression


encodeCompressionMethod : CompressionMethod -> Encoder
encodeCompressionMethod method =
    case method of
        NoCompression ->
            Encode.unsignedInt16 LE 0


constructZip : List File -> Bytes
constructZip files =
    let
        encodedFiles =
            files
                |> List.map encodeFile
                |> Encode.sequence

        segments =
            files
                |> List.foldl
                    (\file ( offset, accum ) ->
                        let
                            buffer =
                                centralDirectoryHeader file offset
                                    |> Encode.encode
                        in
                        ( offset + Bytes.width buffer, Encode.bytes buffer :: accum )
                    )
                    ( 0, [] )
                |> Tuple.second
                |> Encode.sequence
                |> Encode.encode

        encoder =
            Encode.sequence
                [ encodedFiles
                , Encode.bytes segments
                , endOfCentralDirectoryRecord files (Bytes.width segments)
                ]
    in
    Encode.encode encoder


endOfCentralDirectoryRecord files centralDirectorySize =
    let
        signature =
            Encode.unsignedInt32 LE 0x06054B50

        numberOfThisDisk =
            Encode.unsignedInt16 LE 0

        numberOfThisDiskWithTheStack =
            Encode.unsignedInt16 LE 0

        numberOfEntriesOnThisDisk =
            Encode.unsignedInt16 LE (List.length files)

        numberOfEntries =
            Encode.unsignedInt16 LE (List.length files)

        size =
            Encode.unsignedInt32 LE centralDirectorySize

        offset =
            Encode.unsignedInt32 LE centralDirectorySize

        zipCommentLength =
            Encode.unsignedInt16 LE 0

        zipComment =
            Encode.sequence []
    in
    Encode.sequence
        [ signature
        , numberOfThisDisk
        , numberOfThisDiskWithTheStack
        , numberOfEntriesOnThisDisk
        , numberOfEntries
        , size
        , offset
        , zipCommentLength
        , zipComment
        ]


type alias File =
    { name : String, bytes : Bytes }


encodeFile : File -> Encoder
encodeFile file =
    -- TODO do we need an encryption header or data descriptor?
    Encode.sequence
        [ localFileHeader file
        , encryptionHeader file
        , Encode.bytes file.bytes
        ]


encryptionHeader file =
    -- Encode.sequence (List.repeat (8 + 16) (Encode.unsignedInt8 0))
    Encode.sequence []


localFileHeader : File -> Encoder
localFileHeader file =
    let
        localFileHeaderSignature =
            Encode.unsignedInt32 LE 0x04034B50

        versionNeededToExtract =
            Encode.unsignedInt16 LE 0

        generalPurposeBitFlag =
            Encode.unsignedInt16 LE 0

        compressionMethod =
            encodeCompressionMethod NoCompression

        lastModifiedFileTime =
            Encode.unsignedInt16 LE 0

        lastModifiedFileDate =
            Encode.unsignedInt16 LE 0

        crc32 =
            Encode.unsignedInt32 LE (Flate.crc32 file.bytes)

        compressedSize =
            Bytes.width file.bytes
                |> Encode.unsignedInt32 LE

        uncompressedSize =
            Bytes.width file.bytes
                |> Encode.unsignedInt32 LE

        fileNameLength =
            Encode.unsignedInt16 LE (Encode.getStringWidth file.name)

        extraFieldLength =
            Encode.unsignedInt16 LE (List.length cheaty2)

        fileName =
            Encode.string file.name

        extraField =
            cheaty2 |> List.map Encode.unsignedInt8 |> Encode.sequence
    in
    Encode.sequence
        [ localFileHeaderSignature
        , versionNeededToExtract
        , generalPurposeBitFlag
        , compressionMethod
        , lastModifiedFileTime
        , lastModifiedFileDate
        , crc32
        , compressedSize
        , uncompressedSize
        , fileNameLength
        , extraFieldLength
        , fileName
        , extraField
        ]


centralDirectoryHeader : File -> Int -> Encoder
centralDirectoryHeader file offset =
    let
        centralFileHeaderSignature =
            Encode.unsignedInt32 LE 0x02014B50

        versionMadeBy =
            Encode.unsignedInt16 LE 0x031E

        versionNeededToExtract =
            Encode.unsignedInt16 LE 0x0A

        generalPurposeBitFlag =
            Encode.unsignedInt16 LE 0

        compressionMethod =
            encodeCompressionMethod NoCompression

        lastModifiedFileTime =
            Encode.unsignedInt16 LE 0

        lastModifiedFileDate =
            Encode.unsignedInt16 LE 0

        crc32 =
            Encode.unsignedInt32 LE (Flate.crc32 file.bytes)

        compressedSize =
            Bytes.width file.bytes
                |> Encode.unsignedInt32 LE

        uncompressedSize =
            Bytes.width file.bytes
                |> Encode.unsignedInt32 LE

        fileNameLength =
            Encode.unsignedInt16 LE (Encode.getStringWidth file.name)

        extraFieldLength =
            Encode.unsignedInt16 LE 0

        fileCommentLength =
            Encode.unsignedInt16 LE 0

        diskNumberStart =
            Encode.unsignedInt16 LE 0

        internalFileAttributes =
            Encode.unsignedInt16 LE 0

        externalFileAttributes =
            Encode.unsignedInt32 LE 0x81A40000

        relativeOffsetToLocalHeader =
            Encode.unsignedInt32 LE offset

        fileName =
            Encode.string file.name

        extraField =
            Encode.sequence []

        fileComment =
            Encode.sequence []
    in
    Encode.sequence
        [ centralFileHeaderSignature
        , versionMadeBy
        , versionNeededToExtract
        , generalPurposeBitFlag
        , compressionMethod
        , lastModifiedFileTime
        , lastModifiedFileDate
        , crc32
        , compressedSize
        , uncompressedSize
        , fileNameLength
        , extraFieldLength
        , fileCommentLength
        , diskNumberStart
        , internalFileAttributes
        , externalFileAttributes
        , relativeOffsetToLocalHeader
        , fileName
        , extraField
        , fileComment
        ]


cheaty2 =
    List.take 4
        [ 0x55
        , 0x54
        , 0x09
        , 0x00
        ]


cheaty3 =
    [ 0x55
    , 0x54
    , 0x09
    , 0x00
    , 0x03
    , 0x3C
    , 0xBB
    , 0x70
    , 0x5F
    , 0x41
    , 0xBB
    , 0x70
    , 0x5F
    , 0x75
    , 0x78
    , 0x0B
    , 0x00
    , 0x01
    , 0x04
    , 0xE8
    , 0x03
    , 0x00
    , 0x00
    , 0x04
    , 0xE8
    , 0x03
    , 0x00
    , 0x00
    ]


cheaty =
    [ 0x50
    , 0x4B
    , 0x03
    , 0x04
    , 0x0A
    , 0x00
    , 0x00
    , 0x00
    , 0x00
    , 0x00
    , 0x42
    , 0x92
    , 0x3B
    , 0x51
    , 0x83
    , 0x35
    , 0x90
    , 0x08
    , 0x0C
    , 0x00
    , 0x00
    , 0x00
    , 0x0C
    , 0x00
    , 0x00
    , 0x00
    , 0x08
    , 0x00
    , 0x1C
    , 0x00
    , 0x74
    , 0x65
    , 0x73
    , 0x74
    , 0x2E
    , 0x74
    , 0x78
    , 0x74
    , 0x55
    , 0x54
    , 0x09
    , 0x00
    , 0x03
    , 0x3C
    , 0xBB
    , 0x70
    , 0x5F
    , 0x41
    , 0xBB
    , 0x70
    , 0x5F
    , 0x75
    , 0x78
    , 0x0B
    , 0x00
    , 0x01
    , 0x04
    , 0xE8
    , 0x03
    , 0x00
    , 0x00
    , 0x04
    , 0xE8
    , 0x03
    , 0x00
    , 0x00
    , 0x66
    , 0x6F
    , 0x6F
    , 0x20
    , 0x62
    , 0x61
    , 0x72
    , 0x20
    , 0x62
    , 0x61
    , 0x7A
    , 0x0A
    , 0x50
    , 0x4B
    , 0x01
    , 0x02
    , 0x1E
    , 0x03
    , 0x0A
    , 0x00
    , 0x00
    , 0x00
    , 0x00
    , 0x00
    , 0x42
    , 0x92
    , 0x3B
    , 0x51
    , 0x83
    , 0x35
    , 0x90
    , 0x08
    , 0x0C
    , 0x00
    , 0x00
    , 0x00
    , 0x0C
    , 0x00
    , 0x00
    , 0x00
    , 0x08
    , 0x00
    , 0x18
    , 0x00
    , 0x00
    , 0x00
    , 0x00
    , 0x00
    , 0x00
    , 0x00
    , 0x00
    , 0x00
    , 0xA4
    , 0x81
    , 0x00
    , 0x00
    , 0x00
    , 0x00
    , 0x74
    , 0x65
    , 0x73
    , 0x74
    , 0x2E
    , 0x74
    , 0x78
    , 0x74
    , 0x55
    , 0x54
    , 0x05
    , 0x00
    , 0x03
    , 0x3C
    , 0xBB
    , 0x70
    , 0x5F
    , 0x75
    , 0x78
    , 0x0B
    , 0x00
    , 0x01
    , 0x04
    , 0xE8
    , 0x03
    , 0x00
    , 0x00
    , 0x04
    , 0xE8
    , 0x03
    , 0x00
    , 0x00
    , 0x50
    , 0x4B
    , 0x05
    , 0x06
    , 0x00
    , 0x00
    , 0x00
    , 0x00
    , 0x01
    , 0x00
    , 0x01
    , 0x00
    , 0x4E
    , 0x00
    , 0x00
    , 0x00
    , 0x4E
    , 0x00
    , 0x00
    , 0x00
    , 0x00
    , 0x00
    ]
