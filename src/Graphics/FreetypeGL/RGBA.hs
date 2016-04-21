-- | RGBA

module Graphics.FreetypeGL.RGBA
    ( RGBA(..), noColor, toVec4, withVec
    ) where

import Bindings.FreetypeGL.Vec234 (C'vec4(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))

data RGBA = RGBA !Float !Float !Float !Float
    deriving (Eq, Ord, Read, Show)

noColor :: RGBA
noColor = RGBA 0.0 0.0 0.0 0.0

toVec4 :: RGBA -> C'vec4
toVec4 (RGBA r g b a) =
    C'vec4
    (realToFrac r)
    (realToFrac g)
    (realToFrac b)
    (realToFrac a)

withVec :: RGBA -> (Ptr C'vec4 -> IO a) -> IO a
withVec rgba act =
    alloca $ \vec4 ->
    do
        poke vec4 (toVec4 rgba)
        act vec4
