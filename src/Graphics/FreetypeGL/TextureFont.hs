-- | Texture fonts

module Graphics.FreetypeGL.TextureFont
    ( TextureFont(..), newFromFile, newFromMemory, delete
    , PtSize, size, atlas
    , height, lineGap, ascender, descender, underlinePosition, underlineThickness
    ) where

import qualified Bindings.FreetypeGL.TextureFont as TF
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Foreign.C.String (withCString)
import           Foreign.Marshal.Error (throwIfNull)
import           Foreign.Ptr (Ptr, castPtr)
import           Foreign.Storable (peek)
import           Graphics.FreetypeGL.TextureAtlas (TextureAtlas(..))

data TextureFont = TextureFont (Ptr TF.C'texture_font_t)

type PtSize = Float

newFromFile :: TextureAtlas -> PtSize -> FilePath -> IO TextureFont
newFromFile (TextureAtlas atlas_) size_ path =
    withCString path $ \cPath ->
    TextureFont
    <$> throwIfNull "texture_font_new_from_file failed"
        ( TF.c'texture_font_new_from_file atlas_ (realToFrac size_) cPath )

newFromMemory :: TextureAtlas -> PtSize -> ByteString -> IO TextureFont
newFromMemory (TextureAtlas atlas_) size_ mem =
    BS.useAsCStringLen mem $ \(cStr, len) ->
    TextureFont
    <$> throwIfNull "texture_font_new_from_memory failed"
        ( TF.c'texture_font_new_from_memory atlas_
          (realToFrac size_) (castPtr cStr) (fromIntegral len) )

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

atlas :: TextureFont -> IO TextureAtlas
atlas (TextureFont ptr) = TextureAtlas <$> peek (TF.p'texture_font_t'atlas ptr)
