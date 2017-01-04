module Graphics.FreetypeGL.TextureAtlas
    ( TextureAtlas(..)
    , RenderDepth(..), c'renderDepth
    , new, delete
    , uploadIfNeeded, getTexture
    ) where

import qualified Bindings.FreetypeGL.TextBuffer as TB
import qualified Bindings.FreetypeGL.TextureAtlas as TA
import           Control.Monad (when)
import           Data.Foldable (traverse_)
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Foreign.C.Types (CSize(..))
import           Foreign.Marshal.Error (throwIfNull)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (peek)
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

data RenderDepth = LCD_FILTERING_ON | LCD_FILTERING_OFF

data TextureAtlas =
    TextureAtlas
    { ptr :: Ptr TA.C'texture_atlas_t
    , glTextureRef :: IORef (Maybe GL.TextureObject)
    , taUploadedRef :: IORef CSize
    }

getTexture :: TextureAtlas -> IO GL.TextureObject
getTexture atlas =
    do
        mTexture <- readIORef (glTextureRef atlas)
        case mTexture of
            Just texture -> return texture
            Nothing ->
                do
                    texture <- GL.genObjectName
                    writeIORef (glTextureRef atlas) (Just texture)
                    return texture

c'renderDepth :: RenderDepth -> CSize
c'renderDepth LCD_FILTERING_ON = TB.c'LCD_FILTERING_ON
c'renderDepth LCD_FILTERING_OFF = TB.c'LCD_FILTERING_OFF

delete :: TextureAtlas -> IO ()
delete (TextureAtlas p t _) =
    do
        TA.c'texture_atlas_delete p
        readIORef t >>= traverse_ GL.deleteObjectName

new
    :: Word -- ^ width
    -> Word -- ^ height
    -> RenderDepth
    -> IO TextureAtlas
new width height depth =
    TextureAtlas
    <$> throwIfNull "texture_atlas_new failed"
        ( TA.c'texture_atlas_new
          (fromIntegral width) (fromIntegral height) (c'renderDepth depth) )
    <*> newIORef Nothing
    <*> newIORef 0

upload :: TextureAtlas -> IO ()
upload atlas =
    do
        texture <- getTexture atlas
        GL.textureBinding GL.Texture2D $= Just texture
        GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToEdge)
        GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToEdge)
        GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
        TA.C'texture_atlas_t width height depth _ _ dataPtr <- peek (ptr atlas)
        let size = GL.TextureSize2D (fromIntegral width) (fromIntegral height)
        let format
                | depth == 1 = GL.R8
                | otherwise = GL.RGB8
        let pixelFormat
                | depth == 1 = GL.Red
                | otherwise = GL.RGB
        let pixelData = GL.PixelData pixelFormat GL.UnsignedByte dataPtr
        GL.texImage2D GL.Texture2D GL.NoProxy 0 format size 0 pixelData

uploadIfNeeded :: TextureAtlas -> IO ()
uploadIfNeeded atlas@(TextureAtlas atlasPtr _ upRef) =
    do
        uploaded <- readIORef upRef
        used <- peek (TA.p'texture_atlas_t'used atlasPtr)
        when (uploaded /= used) $ do
            writeIORef upRef used
            upload atlas
