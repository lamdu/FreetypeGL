-- | Bindings for markup
{-# LANGUAGE RecordWildCards #-}
module Graphics.FreetypeGL.Markup
    ( Markup(..), def
    , RGBA(..)
    , withMarkupPtr
    ) where

import qualified Bindings.FreetypeGL.Markup as MU
import           Bindings.FreetypeGL.Vec234 (C'vec4(..))
import           Data.Function ((&))
import           Data.Maybe (fromMaybe)
import           Data.Maybe (isJust)
import           Foreign.C.Types (CInt)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr, nullPtr)
import           Foreign.Storable (Storable(..))
import           Graphics.FreetypeGL.TextureFont (TextureFont(..))
import qualified Graphics.FreetypeGL.TextureFont as TF

data RGBA = RGBA Float Float Float Float

-- We intentionally omit the family,size,bold,italic fields which
-- really don't belong in Markup, and are used to select a font. We
-- prefer to load fonts via file paths directly.

-- Ignoring the 'rise' field which is ignored by
-- the library
data Markup = Markup
    { spacing :: !Float
    , gamma :: !Float
    , foregroundColor :: !RGBA
    , backgroundColor :: !RGBA
    , outlineColor :: !(Maybe RGBA)
    , underlineColor :: !(Maybe RGBA)
    , overlineColor :: !(Maybe RGBA)
    , strikethroughColor :: !(Maybe RGBA)
    }

def :: Markup
def =
    Markup
    { spacing = 0.0
    , gamma = 1.0
    , foregroundColor = white
    , backgroundColor = noColor
    , outlineColor = Nothing
    , underlineColor = Nothing
    , overlineColor = Nothing
    , strikethroughColor = Nothing
    }
    where
        white = RGBA 1.0 1.0 1.0 1.0

noColor :: RGBA
noColor = RGBA 0.0 0.0 0.0 0.0

rgbaToVec4 :: RGBA -> C'vec4
rgbaToVec4 (RGBA r g b a) =
    C'vec4
    (realToFrac r)
    (realToFrac g)
    (realToFrac b)
    (realToFrac a)

withMarkupPtr :: Markup -> TextureFont -> (Ptr MU.C'markup_t -> IO a) -> IO a
withMarkupPtr Markup{..} font@(TextureFont fontPtr) act =
    alloca $ \ptr ->
    do
        size <- TF.size font
        let cMarkup =
                MU.C'markup_t
                -- family,bold,italic unneeded, we always give a font:
                nullPtr (realToFrac size) 0 0
                0 -- rise is ignored
                (realToFrac spacing)
                (realToFrac gamma)
                (rgbaToVec4 foregroundColor)
                (rgbaToVec4 backgroundColor)
                & passColorAsCParams outlineColor
                & passColorAsCParams underlineColor
                & passColorAsCParams overlineColor
                & passColorAsCParams strikethroughColor
                & ($ fontPtr)
        poke ptr cMarkup
        act ptr

passColorAsCParams :: Maybe RGBA -> (CInt -> C'vec4 -> a) -> a
passColorAsCParams mColor f =
    f
    ((fromIntegral . fromEnum . isJust) mColor)
    (rgbaToVec4 (fromMaybe noColor mColor))
