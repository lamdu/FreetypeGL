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
import           Data.Maybe (fromMaybe, isJust)
import           Foreign.C.Types (CInt)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr, nullPtr)
import           Foreign.Storable (Storable(..))
import           Graphics.FreetypeGL.RGBA (RGBA(..))
import qualified Graphics.FreetypeGL.RGBA as RGBA
import           Graphics.FreetypeGL.TextureFont (TextureFont(..))
import qualified Graphics.FreetypeGL.TextureFont as TF

-- We intentionally omit the family,size,bold,italic fields which
-- really don't belong in Markup, and are used to select a font. We
-- prefer to load fonts via file paths directly.

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
    , backgroundColor = RGBA.noColor
    , outlineColor = Nothing
    , underlineColor = Nothing
    , overlineColor = Nothing
    , strikethroughColor = Nothing
    }
    where
        white = RGBA 1.0 1.0 1.0 1.0

withMarkupPtr :: Markup -> TextureFont -> (Ptr MU.C'markup_t -> IO a) -> IO a
withMarkupPtr Markup{..} font@(TextureFont fontPtr) act =
    alloca $ \ptr ->
    do
        size <- TF.size font
        let cMarkup =
                MU.C'markup_t
                -- family,bold,italic unneeded, we always give a font:
                nullPtr (realToFrac size) 0 0
                (realToFrac spacing)
                (realToFrac gamma)
                (RGBA.toVec4 foregroundColor)
                (RGBA.toVec4 backgroundColor)
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
    (RGBA.toVec4 (fromMaybe RGBA.noColor mColor))
