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

data EncReadyList = ERL Mask Word8 B.ByteString  deriving (Show,Eq)
--packKey = 

block_offset_mask = complement 0x07 :: Word8

maskForBitOffset offset = complement $ return 0xFF >>= shiftR >>= shiftL $ offset 

splitByte :: Word8 -> Int -> Word8 -> B.ByteString
splitByte mask offset b = B.pack . map (\i -> mask .&. (shiftR b i)) . reverse $ [0, offset .. 7] where



splitByteList :: Word8 -> B.ByteString -> EncReadyList
splitByteList n d = ERL (complement mask) n (B.concatMap (splitByte mask offset) d) where
    mask = maskForBitOffset offset
    offset = fromIntegral $ 8 `div` n

encrypt domain_offset (ERL mask offset source) domain = B.concat [domain_prefix, encrypted, domain_postfix] where
    encrypted = B.pack $ B.zipWith encFn source domain_data
    encFn = \ src_byte dmn_byte -> dmn_byte .&. mask .|. src_byte
    (domain_prefix , domain_data) = B.splitAt domain_offset domain
--    Just (offset_byte,domain_data) = B.uncons domain_data'
    domain_postfix = B.drop (B.length encrypted) domain_data

testMain = do 
    [raw_src,raw_dmn] <- mapM  B.readFile ["source.bmp","domain.bmp"]
    let enc_ready@(ERL _ _ s) = splitByteList 2 raw_src
    print $ B.length s
    let out = encrypt 1024 enc_ready raw_dmn
    B.writeFile "encrypted.bmp" out
    return ()




main = testMain
