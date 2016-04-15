module Graphics.FreetypeGL.TextureAtlas
    ( TextureAtlas(..)
    , RenderDepth(..), c'renderDepth
    , withTextureAtlas
    , upload
    ) where

import qualified Bindings.FreetypeGL.TextBuffer as TB
import qualified Bindings.FreetypeGL.TextureAtlas as TA
import           Control.Exception (bracket)
import           Foreign.C.Types (CSize(..))
import           Foreign.Marshal.Error (throwIfNull)
import           Foreign.Ptr (Ptr)

data RenderDepth = LCD_FILTERING_ON | LCD_FILTERING_OFF

data TextureAtlas = TextureAtlas (Ptr TA.C'texture_atlas_t)

c'renderDepth :: RenderDepth -> CSize
c'renderDepth LCD_FILTERING_ON = TB.c'LCD_FILTERING_ON
c'renderDepth LCD_FILTERING_OFF = TB.c'LCD_FILTERING_OFF

delete :: TextureAtlas -> IO ()
delete (TextureAtlas ptr) = TA.c'texture_atlas_delete ptr

new :: Word -> Word -> RenderDepth -> IO TextureAtlas
new width height depth =
    TextureAtlas
    <$> throwIfNull "texture_atlas_new failed"
        ( TA.c'texture_atlas_new
          (fromIntegral width) (fromIntegral height) (c'renderDepth depth) )

withTextureAtlas
    :: Word -- ^ width
    -> Word -- ^ height
    -> RenderDepth
    -> (TextureAtlas -> IO a) -> IO a
withTextureAtlas width height depth =
    bracket (new width height depth) delete

upload :: TextureAtlas -> IO ()
upload (TextureAtlas ptr) = TA.c'texture_atlas_upload ptr
