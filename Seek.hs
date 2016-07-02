module Main where

import qualified Data.ByteString as B
import Data.Word
import Data.Bits
import Control.Monad

type Mask = Word8
type Offset = Word32

--data DecReadyList = DRL Mask 
offset_block_mask = 0x07

maskForBitOffset offset = return 0xFF >>= shiftR >>= shiftL $ offset 

joinByte :: Mask -> B.ByteString -> Word8
joinByte mask bs = foldl1 (.|.)  $  zipWith (\ b n -> shiftL (not_mask .&. b) n) (B.unpack bs) (reverse [0,offset..7]) where 
  offset = 8 `div` B.length bs
  not_mask = complement mask

joinByteList :: Int -> B.ByteString -> B.ByteString
joinByteList n d = B.pack $ map (joinByte mask) groupN  where
  groupN = map (\i -> B.take n $ B.drop i d) [0,n .. pred (B.length d)]
  mask = maskForBitOffset $ 8 `div` n

decode :: Int -> Int -> Int -> B.ByteString -> B.ByteString
decode source_offset data_len bit_offset source = joinByteList bit_offset mean_block  where
  --bit_offset = fromIntegral $ offset_block_mask .&. offset_byte 
  mean_block = B.take data_len . B.drop source_offset $ source


testMain = do
  raw_src <- B.readFile "encrypted.bmp"
  let decoded = decode 1024 1572972 2 raw_src
  print $ B.length decoded
  B.writeFile "decrypted.bmp" decoded

main = testMain
