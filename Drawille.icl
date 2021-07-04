module Drawille

import StdEnv
import StdDebug

// FIXME: change the type of the value.
/**
 * Canvas for drawing with Braille characters.
 * @var current X dimension
 * @var current Y dimension
 * @var array, which contains the bitmap in rows
 */
:: Canvas = { size_x :: !Int, size_y :: !Int,
              real_size_x :: !Int, real_size_y :: !Int, data :: !.{#Bool}}

valueToInt True = 1
valueToInt False = 0

pixmap :: {#{#Int}}
pixmap = {{0x01, 0x08},
          {0x02, 0x10},
          {0x04, 0x20},
          {0x40, 0x80}}

// Braille character code to the list of coordinates
brailleToList :: !Int -> [(Int, Int)]
brailleToList n = filter ((<>) (-1, -1))
        (flatten [ [(coord px.[0] 0 i), (coord px.[1] 1 i)] \\ px <-: pixmap & i <- [0..3]])
    where coord b x y
            | ((n bitand b) > 0) = (x, y)
            | otherwise = (-1, -1)

/**
 * Render canvas contents as a monochrome Braille character
 * string (UTF-8 encoded).
 *
 * @param Canvas to render
 *
 * @result List of UTF-8 encoded strings
 */
frame :: !Canvas -> [String]
frame c = [lineToString line \\ line <-: toBrailleCodes c]
    where lineToString line = go 0 line (createArray (3*size line) ' ')
          go :: Int .{#Int} *String -> *String
          go i line=:{[i] = c} s
            | i == size line = s
            | otherwise =
                go (i + 1) line
                    {s & [3*i] = '\xE2',
                         [3*i + 1] = toChar v,
                         [3*i + 2] = toChar ((0xBF bitand c) bitor 0x80)}
                where v = 0xA0 + (c bitand pixmap.[3].[0])/pixmap.[3].[0]
                               + (c bitand pixmap.[3].[1])/pixmap.[3].[0]

/**
 * Recognise canvas contents as a bitmap of
 * Braille characters. Simple bit encoding based on `pixmap`
 * is used, character codes are in the range [0..255].
 *
 * @param Canvas to parse
 *
 * @result 2D array (first index - rows) of Braille chars
 */
toBrailleCodes :: !Canvas -> {*{#Int}}
toBrailleCodes c=:{ size_x, size_y, real_size_x, real_size_y, data} =
    goRows bitmap 0
    where (bitmap_x, bitmap_y) = ((size_x + 1)/2, (size_y + 3)/4)
          bitmap = {(createArray bitmap_x 0) \\ i <- [1..bitmap_y]}

          goRows bitmap row
            | row < bitmap_y = goRows (goColumns bitmap row 0) (row + 1)
            | otherwise = bitmap

          goColumns bitmap row col
            | col < bitmap_x = goColumns {bitmap & [row,col] = value } row (col + 1)
            | otherwise = bitmap
            where value = sum [(valueToInt (get c (2*col + x) (4*row + y))) * pixmap.[y, x]\\ x <- [0..1], y <- [0..3]
                                    | x < size_x - 2*col && y < size_y - 4*row]

/**
 * Empty Canvas without any drawings.
 */
empty :: .Canvas
empty => { size_x = 0, size_y = 0,
           real_size_x = 0, real_size_y = 0, data = {}}

/**
 * Get the value of a pixel on canvas at given coordinates.
 *
 * @param The canvas
 * @param x coordinate of the pixel (column)
 * @param y coordinate of the pixel (row)
 *
 * @result The value of pixel
 */
get :: !Canvas !Int !Int -> Bool
get c x y = fst (uget c x y)

/**
 * Get the value of a pixel on canvas at given coordinates.
 * Unique version.
 *
 * @param The canvas
 * @param x coordinate of the pixel (column)
 * @param y coordinate of the pixel (row)
 *
 * @result The value of pixel and unmodified canvas 
 */
uget :: !u:Canvas !Int !Int -> *(Bool, v:Canvas), [u <= v]
uget c=:{ size_x, size_y, real_size_x, data} x y
    # (v, data`) = uselect data (x + y * real_size_x)
    = (v, { c & data = data` })

/**
 * Resize canvas to match given new size_x = x and
 * size_y = y. If new canvas does not fit underlying
 * array, it reallocates array. It does not shrink
 * underlying array.
 *
 * @param The canvas
 * @param x coordinate of the pixel (column)
 * @param y coordinate of the pixel (row)
 *
 * @result Canvas with updates size_x and size_y
 */
resize :: !*Canvas !Int !Int -> *Canvas
resize c=:{ size_x, size_y, real_size_x, real_size_y, data} x y
    | (x < c.real_size_x && y < c.real_size_y) = {c & size_x = x, size_y = y} // FIXME: < -> <=
    | otherwise = { size_x = x, size_y = y,
                    real_size_x = rx, real_size_y = ry,
                    data = go 0 data (createArray (rx * ry) False) }
        where (rx, ry) = (3*x/2 + 2, 3*y/2 + 2)

              go :: Int !.{#Bool} !*{#Bool} -> *{#Bool}
              go j src dst
                 | j == size_y = dst
                 # dst = {dst & [i + dst_shift] = src.[i + src_shift] \\ i <- [0..(size_x - 1)]}
                 | otherwise = go (j + 1) src dst

                    where src_shift = j*real_size_x
                          dst_shift = j*rx

updateWithValue :: !*Canvas !Int !Int !Bool -> *Canvas
updateWithValue c=:{ size_x, size_y, real_size_x, data} x y v
    | (x >= size_x && y >= size_y) = updateWithValue (resize c (x + 1) (y + 1)) x y v
    | x >= size_x = updateWithValue (resize c (x + 1) size_y) x y v
    | y >= size_y = updateWithValue (resize c size_x (y + 1)) x y v
    | otherwise = {c & data = { data & [x + real_size_x * y] = v }}

/**
 * Set pixel with coordinates on Canvas.
 *
 * @param The canvas
 *
 * @param x coordinate of the pixel (column)
 * @param y coordinate of the pixel (row)
 *
 * @result Updated canvas
 */
set :: !*Canvas !Int !Int -> *Canvas
set c x y = updateWithValue c x y True

/**
 * Set pixel with coordinates on Canvas.
 *
 * @param The canvas
 *
 * @param x coordinate of the pixel (column)
 * @param y coordinate of the pixel (row)
 *
 * @result Updated canvas
 */
unset :: !*Canvas !Int !Int -> *Canvas
unset c x y = updateWithValue c x y False

/**
 * Toggle pixel with coordinates on Canvas.
 *
 * @param The canvas
 *
 * @param x coordinate of the pixel (column)
 * @param y coordinate of the pixel (row)
 *
 * @result Updated canvas
 */
toggle :: !*Canvas !Int !Int -> *Canvas
toggle c x y
    # (v, c) = uget c x y
    = updateWithValue c x y (not v)

/**
 * Create Canvas from list of pixel coordinates. These pixels
 * will be set, canvas will be autoadjusted.
 * @param The number of columns
 * @param The number of rows
 * @result The blank canvas.
 */
fromList :: [(Int, Int)] -> .Canvas
fromList lst = go lst empty
    where go [(x, y):px] c = go px (set c x y)
          go [] c = c

/**
 * Create Canvas with given dimensions. Also see {{`empty`}}.
 * @param The number of columns
 * @param The number of rows
 * @result The blank canvas.
 */
create :: !Int !Int -> .Canvas
create size_x size_y =
    { size_x = size_x, size_y = size_y,
      real_size_x = size_x, real_size_y = size_y,
      data = createArray (size_x * size_y) False}

/**
 * Debug print canvas.
 *
 * @param canvas to print.
 *
 * @result string, where each character represents canvas pixel.
 */
toString` :: !.Canvas -> String
toString` { size_x = sx, size_y = sy, real_size_x, data = d } = go 0 (createArray ((sx + 1)*sy) 'u')
    where go :: !Int !*String -> *String
          go j arr
            | j < sy = go (j + 1)
                {{ arr & [shift + i] = charOf d.[j*real_size_x + i] \\ i <- [0..(sx - 1)]} & [shift + sx] = '\n'}
            | otherwise = arr
            where shift = j * (sx + 1)
          charOf e
            | not e = '_'
            | e = '*'
            | otherwise = '?'

Start :: String
Start = "Hello Drawille \xE2\xA0\x81\xE2\xA0\xB2\n" +++ toString` (set (set (create 8 9) 0 0) 1 2) +++
        "================================================================================\n" +++
        toString` (fromList (brailleToList 0xF8))
