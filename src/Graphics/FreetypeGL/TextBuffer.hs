-- | A text buffer

module Graphics.FreetypeGL.TextBuffer
    ( TextBuffer
    , withTextBuffer
    , getFontManager
    , RenderDepth(..)
    , Pen(..)
    , clear
    , addText
    , Align(..), align
    , render
    ) where

import qualified Bindings.FreetypeGL.TextBuffer as TB
import qualified Bindings.FreetypeGL.Vec234 as Vec234
import           Control.Exception (bracket, finally)
import           Data.IORef
import           Foreign.C.String (withCStringLen)
import           Foreign.C.Types (CUInt)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Marshal.Error (throwIfNull)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (Storable(..))
import           Graphics.FreetypeGL.Markup (Markup(..))
import qualified Graphics.FreetypeGL.Markup as MU
import           Graphics.FreetypeGL.Shader (Shader(..))
import           Graphics.FreetypeGL.TextureAtlas (RenderDepth(..), c'renderDepth)
import           Graphics.FreetypeGL.TextureFont (TextureFont(..))
import           Graphics.FreetypeGL.FontManager (FontManager(..))

data Pen = Pen { penX :: !Float, penY :: !Float }
    deriving (Eq, Ord, Read, Show)

penOfVec :: Vec234.C'vec2 -> Pen
penOfVec (Vec234.C'vec2 x y) = Pen (realToFrac x) (realToFrac y)

vecOfPen :: Pen -> Vec234.C'vec2
vecOfPen (Pen x y) = Vec234.C'vec2 (realToFrac x) (realToFrac y)

newtype TextBuffer = TextBuffer (Ptr TB.C'text_buffer_t)

getFontManager :: TextBuffer -> IO FontManager
getFontManager (TextBuffer ptr) = FontManager <$> peek (TB.p'text_buffer_t'manager ptr)

new :: RenderDepth -> Shader -> IO TextBuffer
new renderDepth (Shader program) =
    TextBuffer
    <$> throwIfNull "text_buffer_new failed"
        ( TB.c'text_buffer_new_with_program
          (c'renderDepth renderDepth)
          (fromIntegral program)
        )

delete :: TextBuffer -> IO ()
delete (TextBuffer ptr) = TB.c'text_buffer_delete ptr

withTextBuffer :: RenderDepth -> Shader -> (TextBuffer -> IO a) -> IO a
withTextBuffer renderDepth shader = bracket (new renderDepth shader) delete

withPen :: IORef Pen -> (Ptr Vec234.C'vec2 -> IO a) -> IO a
withPen penRef act =
    do
        penVec <- vecOfPen <$> readIORef penRef
        alloca $ \penPtr ->
            do
                poke penPtr penVec
                act penPtr
                    `finally`
                    (penOfVec <$> peek penPtr >>= writeIORef penRef)

addText :: TextBuffer -> IORef Pen -> Markup -> TextureFont -> String -> IO ()
addText (TextBuffer ptr) penRef markup font str =
    withPen penRef $ \penPtr ->
    MU.withMarkupPtr markup font $ \markupPtr ->
    withCStringLen str $ \(cStr, len) ->
    TB.c'text_buffer_add_text ptr penPtr markupPtr cStr (fromIntegral len)

clear :: TextBuffer -> IO ()
clear (TextBuffer ptr) = TB.c'text_buffer_clear ptr

data Align = AlignLeft | AlignCenter | AlignRight

c'Align :: Align -> CUInt
c'Align AlignLeft   = TB.c'ALIGN_LEFT
c'Align AlignCenter = TB.c'ALIGN_CENTER
c'Align AlignRight  = TB.c'ALIGN_RIGHT

align :: TextBuffer -> IORef Pen -> Align -> IO ()
align (TextBuffer ptr) penRef algn =
    withPen penRef $ \penPtr ->
    TB.c'text_buffer_align ptr penPtr (c'Align algn)

render :: TextBuffer -> IO ()
render (TextBuffer ptr) = TB.c'text_buffer_render ptr
