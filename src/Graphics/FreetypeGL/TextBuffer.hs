-- | A text buffer

module Graphics.FreetypeGL.TextBuffer
    ( TextBuffer
    , new, delete
    , Pen(..)
    , clear
    , addText
    , Align(..), align
    , render
    , BoundingBox(..)
    , boundingBox
    ) where

import qualified Bindings.FreetypeGL.TextBuffer as TB
import qualified Bindings.FreetypeGL.Vec234 as Vec234
import           Control.Monad.Trans.State (StateT(..))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Foreign as TextForeign
import           Foreign.C.Types (CUInt)
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Marshal.Error (throwIfNull)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (Storable(..))
import           Graphics.FreetypeGL.Markup (Markup(..))
import qualified Graphics.FreetypeGL.Markup as MU
import           Graphics.FreetypeGL.TextureFont (TextureFont(..))

data Pen = Pen { penX :: !Float, penY :: !Float }
    deriving (Eq, Ord, Read, Show)

penOfVec :: Vec234.C'vec2 -> Pen
penOfVec (Vec234.C'vec2 x y) = Pen (realToFrac x) (realToFrac y)

vecOfPen :: Pen -> Vec234.C'vec2
vecOfPen (Pen x y) = Vec234.C'vec2 (realToFrac x) (realToFrac y)

newtype TextBuffer = TextBuffer (Ptr TB.C'text_buffer_t)

new :: IO TextBuffer
new = TextBuffer <$> throwIfNull "text_buffer_new failed" TB.c'text_buffer_new

delete :: TextBuffer -> IO ()
delete (TextBuffer ptr) = TB.c'text_buffer_delete ptr

withPen :: (Ptr Vec234.C'vec2 -> IO a) -> StateT Pen IO a
withPen act = StateT $ \oldPen ->
    alloca $ \penPtr ->
    do
        poke penPtr (vecOfPen oldPen)
        res <- act penPtr
        newPen <- penOfVec <$> peek penPtr
        return (res, newPen)

addText :: TextBuffer -> Markup -> TextureFont -> Text -> StateT Pen IO ()
addText (TextBuffer ptr) markup font text
    | Text.null text = return ()
    | otherwise =
        withPen $ \penPtr ->
        MU.withMarkupPtr markup font $ \markupPtr ->
        TextForeign.withCStringLen text $ \(charsUtf8, _byteLen) ->
        TB.c'text_buffer_add_text ptr penPtr markupPtr charsUtf8
        (fromIntegral (Text.length text))

clear :: TextBuffer -> IO ()
clear (TextBuffer ptr) = TB.c'text_buffer_clear ptr

data Align = AlignLeft | AlignCenter | AlignRight

c'Align :: Align -> CUInt
c'Align AlignLeft   = TB.c'ALIGN_LEFT
c'Align AlignCenter = TB.c'ALIGN_CENTER
c'Align AlignRight  = TB.c'ALIGN_RIGHT

align :: TextBuffer -> Align -> StateT Pen IO ()
align (TextBuffer ptr) algn =
    withPen $ \penPtr ->
    TB.c'text_buffer_align ptr penPtr (c'Align algn)

data BoundingBox = BoundingBox
    { bbLeft :: !Float
    , bbTop :: !Float
    , bbWidth :: !Float
    , bbHeight :: !Float
    } deriving (Eq, Ord, Read, Show)

boundingBox :: TextBuffer -> StateT Pen IO BoundingBox
boundingBox (TextBuffer ptr) =
    withPen $ \penPtr ->
    alloca $ \boundsVec4 ->
    do
        TB.c'wrapper__text_buffer_get_bounds boundsVec4 ptr penPtr
        Vec234.C'vec4 left top width height <- peek boundsVec4
        return (BoundingBox (f left) (f top) (f width) (f height))
    where
        f = realToFrac

render :: TextBuffer -> IO ()
render (TextBuffer ptr) = TB.c'text_buffer_render ptr
