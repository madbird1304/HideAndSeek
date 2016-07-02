module Main where

import qualified Data.ByteString as B
import Data.Word 
import Data.Bits
import Control.Monad

--splitByte = 

type Offset = Word32
type Frequency = Word32
type Mask = Word8

data EncInfo = EI Offset Frequency

data EncReadyList = ERL Mask [Word8] deriving (Show,Eq)
--packKey = 

maskForBitOffset offset = return 0xFF >>= shiftR >>= shiftL $ offset 

--splitByte :: Int -> [Word8] -> [Word8]
splitByte mask offset b =  map (\i -> (complement mask) .&. (shiftR b i)) . reverse $ [0, offset .. 7] where
    --offset = 8 `div` n
    --mask = complement $ return 0xFF >>= shiftR >>= shiftL $ offset 
                --shift (0xF0 .&. b) (-4),
                --(0x0F .&. b) ]

splitByteList :: Int -> [Word8] -> EncReadyList
splitByteList n d = ERL mask (concatMap (splitByte mask offset) d) where
    mask = maskForBitOffset offset
    offset = 8 `div` n

encrypt domain_offset (ERL mask source) domain = domain_prefix ++ encrypted ++ domain_postfix where
    encrypted = zipWith encFn source domain_data
    encFn = \ src_byte dmn_byte -> dmn_byte .&. mask .|. src_byte
    (domain_prefix,domain_data) = splitAt domain_offset domain
    domain_postfix = drop (length encrypted) domain_data

testMain = do 
    [raw_src,raw_dmn] <- mapM ((return . B.unpack) <=< B.readFile) ["source.bmp","domain.bmp"]
    let enc_ready = splitByteList 1 raw_src
    let out = B.pack $ encrypt 1024 enc_ready raw_dmn
    B.writeFile "encrypted.bmp" out
    return ()




main = testMain