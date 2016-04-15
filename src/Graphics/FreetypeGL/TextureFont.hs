-- | Texture fonts

module Graphics.FreetypeGL.TextureFont
    ( TextureFont(..), withFontFromFile, withFontFromMemory
    , PtSize, getSize
    , getAtlas
    ) where

import qualified Bindings.FreetypeGL.TextureFont as TF
import           Control.Exception (bracket)
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

withFontFromFile :: TextureAtlas -> PtSize -> FilePath -> (TextureFont -> IO a) -> IO a
withFontFromFile atlas size path =
    bracket (newFromFile atlas size path) delete

withFontFromMemory :: TextureAtlas -> PtSize -> ByteString -> (TextureFont -> IO a) -> IO a
withFontFromMemory atlas size bs =
    bracket (newFromMemory atlas size bs) delete

getSize :: TextureFont -> IO PtSize
getSize (TextureFont ptr) = realToFrac <$> peek (TF.p'texture_font_t'size ptr)

getAtlas :: TextureFont -> IO TextureAtlas
getAtlas (TextureFont ptr) = TextureAtlas <$> peek (TF.p'texture_font_t'atlas ptr)
