-- | Texture fonts

module Graphics.FreetypeGL.TextureFont
    ( TextureFont(..), newFromFile, newFromMemory, delete
    , PtSize, getSize, getAtlas
    , getHeight, ascender, descender, underlinePosition, underlineThickness
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
newFromFile (TextureAtlas atlas) size path =
    withCString path $ \cPath ->
    TextureFont
    <$> throwIfNull "texture_font_new_from_file failed"
        ( TF.c'texture_font_new_from_file atlas (realToFrac size) cPath )

newFromMemory :: TextureAtlas -> PtSize -> ByteString -> IO TextureFont
newFromMemory (TextureAtlas atlas) size mem =
    BS.useAsCStringLen mem $ \(cStr, len) ->
    TextureFont
    <$> throwIfNull "texture_font_new_from_memory failed"
        ( TF.c'texture_font_new_from_memory atlas
          (realToFrac size) (castPtr cStr) (fromIntegral len) )

delete :: TextureFont -> IO ()
delete (TextureFont ptr) = TF.c'texture_font_delete ptr

getHeight :: TextureFont -> IO Float
getHeight (TextureFont ptr) = realToFrac <$> peek (TF.p'texture_font_t'height ptr)

ascender :: TextureFont -> IO Float
ascender (TextureFont ptr) = realToFrac <$> peek (TF.p'texture_font_t'ascender ptr)

descender :: TextureFont -> IO Float
descender (TextureFont ptr) = realToFrac <$> peek (TF.p'texture_font_t'descender ptr)

underlinePosition :: TextureFont -> IO Float
underlinePosition (TextureFont ptr) = realToFrac <$> peek (TF.p'texture_font_t'underline_position ptr)

underlineThickness :: TextureFont -> IO Float
underlineThickness (TextureFont ptr) = realToFrac <$> peek (TF.p'texture_font_t'underline_thickness ptr)

getSize :: TextureFont -> IO PtSize
getSize (TextureFont ptr) = realToFrac <$> peek (TF.p'texture_font_t'size ptr)

getAtlas :: TextureFont -> IO TextureAtlas
getAtlas (TextureFont ptr) = TextureAtlas <$> peek (TF.p'texture_font_t'atlas ptr)
