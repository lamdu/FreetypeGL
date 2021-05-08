-- | Texture fonts

module Graphics.FreetypeGL.TextureFont
    ( TextureFont(..), newFromFile, newFromMemory, delete
    , TextureGlyph(..), glyph, glyphAdvanceX, glyphAdvanceY, glyphKerning
    , RenderMode(..), PtSize, size, OutlineThickness
    , height, lineGap, ascender, descender, underlinePosition, underlineThickness
    ) where

import qualified Bindings.FreetypeGL.TextureFont as TF
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Foreign.C.String (withCString)
import           Foreign.Marshal.Error (throwIfNull)
import           Foreign.Ptr (Ptr, castPtr)
import           Foreign.Storable (peek, poke)
import           Graphics.FreetypeGL.TextureAtlas (TextureAtlas)
import qualified Graphics.FreetypeGL.TextureAtlas as TextureAtlas

newtype TextureGlyph = TextureGlyph (Ptr TF.C'texture_glyph_t)
newtype TextureFont = TextureFont (Ptr TF.C'texture_font_t)

type PtSize = Float
type OutlineThickness = Float

data RenderMode
    = RenderNormal
    | RenderOutlineEdge OutlineThickness
    | RenderOutlinePositive OutlineThickness
    | RenderOutlineNegative OutlineThickness
    | RenderSignedDistanceField

c'renderMode :: RenderMode -> (TF.C'rendermode_t, OutlineThickness)
c'renderMode RenderNormal = (TF.c'RENDER_NORMAL, 0)
c'renderMode (RenderOutlineEdge x) = (TF.c'RENDER_OUTLINE_EDGE, x)
c'renderMode (RenderOutlinePositive x) = (TF.c'RENDER_OUTLINE_POSITIVE, x)
c'renderMode (RenderOutlineNegative x) = (TF.c'RENDER_OUTLINE_NEGATIVE, x)
c'renderMode RenderSignedDistanceField = (TF.c'RENDER_SIGNED_DISTANCE_FIELD, 0)

setRenderMode :: RenderMode -> Ptr TF.C'texture_font_t -> IO ()
setRenderMode mode ptr =
    do
        poke (TF.p'texture_font_t'rendermode ptr) cRenderMode
        poke (TF.p'texture_font_t'outline_thickness ptr) (realToFrac outline)
    where
        (cRenderMode, outline) = c'renderMode mode

newFromFile :: TextureAtlas -> PtSize -> RenderMode -> FilePath -> IO TextureFont
newFromFile atlas size_ mode path =
    withCString path $ \cPath ->
    TextureFont <$>
    do
        font <-
            throwIfNull "texture_font_new_from_file failed" $
            TF.c'texture_font_new_from_file (TextureAtlas.ptr atlas)
            (realToFrac size_) cPath
        setRenderMode mode font
        return font

newFromMemory :: TextureAtlas -> PtSize -> RenderMode -> ByteString -> IO TextureFont
newFromMemory atlas size_ mode mem =
    BS.useAsCStringLen mem $ \(cStr, len) ->
    TextureFont <$>
    do
        font <-
            throwIfNull "texture_font_new_from_memory failed" $
            TF.c'texture_font_new_from_memory (TextureAtlas.ptr atlas)
            (realToFrac size_) (castPtr cStr) (fromIntegral len)
        setRenderMode mode font
        return font

delete :: TextureFont -> IO ()
delete (TextureFont ptr) = TF.c'texture_font_delete ptr

lineGap :: TextureFont -> IO Float
lineGap (TextureFont ptr) = realToFrac <$> peek (TF.p'texture_font_t'linegap ptr)

height :: TextureFont -> IO Float
height (TextureFont ptr) = realToFrac <$> peek (TF.p'texture_font_t'height ptr)

ascender :: TextureFont -> IO Float
ascender (TextureFont ptr) = realToFrac <$> peek (TF.p'texture_font_t'ascender ptr)

descender :: TextureFont -> IO Float
descender (TextureFont ptr) = realToFrac <$> peek (TF.p'texture_font_t'descender ptr)

underlinePosition :: TextureFont -> IO Float
underlinePosition (TextureFont ptr) = realToFrac <$> peek (TF.p'texture_font_t'underline_position ptr)

underlineThickness :: TextureFont -> IO Float
underlineThickness (TextureFont ptr) = realToFrac <$> peek (TF.p'texture_font_t'underline_thickness ptr)

size :: TextureFont -> IO PtSize
size (TextureFont ptr) = realToFrac <$> peek (TF.p'texture_font_t'size ptr)

glyph :: Char -> TextureFont -> IO TextureGlyph
glyph c (TextureFont ptr) =
    TextureGlyph <$> withCString [c] (TF.c'texture_font_get_glyph ptr)

glyphAdvanceX :: TextureGlyph -> IO Float
glyphAdvanceX (TextureGlyph ptr) = realToFrac <$> peek (TF.p'texture_glyph_t'advance_x ptr)

glyphAdvanceY :: TextureGlyph -> IO Float
glyphAdvanceY (TextureGlyph ptr) = realToFrac <$> peek (TF.p'texture_glyph_t'advance_y ptr)

glyphKerning :: Char -> TextureGlyph -> IO Float
glyphKerning prevChar (TextureGlyph ptr) =
    withCString [prevChar] $ \prevCharUtf8 ->
    realToFrac <$> TF.c'texture_glyph_get_kerning ptr prevCharUtf8
