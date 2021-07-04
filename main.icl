module main
/* Testing driver module for Drawille. */

import StdEnv
import StdDebug
import Data.Word8

import Drawille

import Gast

// Necessary functions for testing with Word8, since it is not part of GAST
ggen{|Word8|} s = map fromInt [1..255]
genShow{|Word8|} sep p x rest = [toString x: rest]
gPrint{|Word8|}          x st = gPrint{|*|} (toInt x) st
instance toString Word8 where toString x = toString (toInt x)

// Various property functions
// Checks the generation of Braille characters & that the size
// of canvas does not exceed 2/3 * single char dimensions (2 x 4).
propertyBrailleFunctions :: Word8 -> Bool
propertyBrailleFunctions byte
    | code` == 0 = True // When code` is 0, the character is NOT generated!
    | otherwise = code` == (toBrailleCodes canvas).[0].[0] &&
                           canvas.size_x <= 3 && canvas.size_y <= 5 &&
                           canvas.real_size_x <= 5 && canvas.real_size_y <= 8
    where code`  = toInt byte
          canvas = fromList (brailleToList code`)

// All the points in clear canvas are 0
propertyClearCanvasIsUnset :: Word8 Word8 Word8 Word8 -> Bool
propertyClearCanvasIsUnset size_x` size_y` x` y`
    | (size_x == 0) || (size_y == 0) = True
    | (x >= size_x) || (y >= size_y) = True
    | otherwise = get (create size_x size_y) x y == unsetPixel
    where (size_x, size_y) = (toInt size_x`, toInt size_y`)
          (x, y) = (toInt x`, toInt y`)

test x = quietn (256^2) aStream x
test` x = testn 20000 x

Start :: [[String]]
Start = [test propertyBrailleFunctions,
         test propertyClearCanvasIsUnset]
