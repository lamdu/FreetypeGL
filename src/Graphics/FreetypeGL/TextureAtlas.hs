module Graphics.FreetypeGL.TextureAtlas
    ( TextureAtlas(..)
    , RenderDepth(..), c'renderDepth
    , new, delete
    , upload
    , Mode(..), setMode, getMode
    ) where

import qualified Bindings.FreetypeGL.TextBuffer as TB
import qualified Bindings.FreetypeGL.TextureAtlas as TA
import           Foreign.C.Types (CInt(..), CSize(..))
import           Foreign.Marshal.Error (throwIfNull)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (Storable(..))

data RenderDepth = LCD_FILTERING_ON | LCD_FILTERING_OFF

data TextureAtlas = TextureAtlas (Ptr TA.C'texture_atlas_t)

c'renderDepth :: RenderDepth -> CSize
c'renderDepth LCD_FILTERING_ON = TB.c'LCD_FILTERING_ON
c'renderDepth LCD_FILTERING_OFF = TB.c'LCD_FILTERING_OFF

delete :: TextureAtlas -> IO ()
delete (TextureAtlas ptr) = TA.c'texture_atlas_delete ptr

new
    :: Word -- ^ width
    -> Word -- ^ height
    -> RenderDepth -> IO TextureAtlas
new width height depth =
    TextureAtlas
    <$> throwIfNull "texture_atlas_new failed"
        ( TA.c'texture_atlas_new
          (fromIntegral width) (fromIntegral height) (c'renderDepth depth) )

upload :: TextureAtlas -> IO ()
upload (TextureAtlas ptr) = TA.c'texture_atlas_upload ptr

data Mode
    = Normal
    | DistanceField
    deriving (Eq, Ord, Read, Show)

modeToCInt :: Mode -> CInt
modeToCInt Normal = 0
modeToCInt DistanceField = 1

modeFromCInt :: CInt -> Mode
modeFromCInt 0 = Normal
modeFromCInt 1 = DistanceField
modeFromCInt _ = error "Invalid mode in TextureAtlas"

getMode :: TextureAtlas -> IO Mode
getMode (TextureAtlas ptr) = modeFromCInt <$> peek (TA.p'texture_atlas_t'p_distance_field ptr)

setMode :: TextureAtlas -> Mode -> IO ()
setMode (TextureAtlas ptr) mode = poke (TA.p'texture_atlas_t'p_distance_field ptr) (modeToCInt mode)
