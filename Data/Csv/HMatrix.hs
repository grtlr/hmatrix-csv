module Data.Csv.HMatrix
    (
      -- * Usage example
      -- $example

      -- * Information
      -- $info

      decodeMatrix
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

-- $example
--
-- For decoding we have to specify, if there is a header. This can be done with the HasHeader/NoHeader type:
--
-- > >>> decodeMatrix NoHeader "1.0,2.0,3.0\r\n4.0,5.0,6.0\r\n7.0,8.0,9.0\r\n"
-- > (3><3)
-- > [ 1.0, 2.0, 3.0
-- > , 4.0, 5.0, 6.0
-- > , 7.0, 8.0, 9.0 ]
--
-- Cassava, which is used for parsing the .csv files uses overloaded string literals. If you try this in ghci, make sure to start it with the right flag:
--
-- > >>> ghci -XOverloadedStrings
--
-- Encoding a file works pretty much the same, except that we do not need to specify a header.
--
-- > >>> encodeMatrix $ matrix 3 [1,2,3,4,5,6,7,8,9]
-- > "1.0,2.0,3.0\r\n4.0,5.0,6.0\r\n7.0,8.0,9.0\r\n"

-- $info
-- If you want to help improve this library, feel free to file an issue or send a pull-request on github. Every feedback is appreciated.
-- As of now only matrices of type Double are supported.

-- | Decodes a matrix.
decodeMatrix :: HasHeader       -- ^ From Data.Csv: specify if the CSV string has a header
             -> ByteString      -- ^ The 'ByteString' containing the CSVs
             -> Matrix Double   -- ^ The parsed 'Matrix'
decodeMatrix header = decodeMatrixWith header ','

-- | Decodes a matrix from ByteString and additionally allow
-- to specify the delimter which was used.
decodeMatrixWith :: HasHeader       -- ^ From Data.Csv: specify if the CSV string has a header
                 -> Char            -- ^ The delimiter
                 -> ByteString      -- ^ The 'ByteString' containing the CSVs
                 -> Matrix Double   -- ^ The parsed 'Matrix'
decodeMatrixWith header del s =
    case decodeWith opt header s of
        Left err -> error err
        Right v  -> fromLists . V.toList . V.map V.toList $ v
    where opt = defaultDecodeOptions { decDelimiter = fromIntegral (ord del) }

rowToRecord :: [Double] -> Record
rowToRecord x = record $ map (C.pack . show) x

-- | Encodes a matrix with comma as delimiter.
encodeMatrix :: Matrix Double   -- ^ The 'Matrix' to encode
             -> ByteString      -- ^ The resulting 'ByteString'
encodeMatrix = encodeMatrixWith ','

-- | Encodes a matrix but allows to specify a delimiter.
encodeMatrixWith :: Char            -- ^ The delimiter for separating the values
                 -> Matrix Double   -- ^ The 'Matrix' to encode
                 -> ByteString      -- ^ The resulting 'ByteString'
encodeMatrixWith del m = encodeWith opt s
    where opt = defaultEncodeOptions { encDelimiter = fromIntegral (ord del) }
          s = map rowToRecord $ toLists m
