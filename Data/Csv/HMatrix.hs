module Data.Csv.HMatrix
    ( decodeMatrix
    , decodeMatrixWith
    , encodeMatrix
    , encodeMatrixWith
    ) where

import Data.Csv
import Numeric.LinearAlgebra.HMatrix
import qualified Data.Vector as V
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Char (ord)

decodeMatrix :: HasHeader -> ByteString -> Matrix Double
decodeMatrix header s = decodeMatrixWith header ',' s

decodeMatrixWith :: HasHeader -> Char -> ByteString -> Matrix Double
decodeMatrixWith header del s =
    case decodeWith opt header s of
        Left err -> error err
        Right v  -> fromLists . V.toList . V.map V.toList $ v
    where opt = defaultDecodeOptions { decDelimiter = fromIntegral (ord del) }

rowToRecord :: [Double] -> Record
rowToRecord x = record $ map (C.pack . show) x

encodeMatrix :: Matrix Double -> ByteString
encodeMatrix m = encodeMatrixWith ',' m

encodeMatrixWith :: Char -> Matrix Double -> ByteString
encodeMatrixWith del m = encodeWith opt s
    where opt = defaultEncodeOptions { encDelimiter = fromIntegral (ord del) }
          s = map rowToRecord $ toLists m
