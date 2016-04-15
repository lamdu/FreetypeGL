-- | Simple linear matrix of 4x4
{-# LANGUAGE RecordWildCards #-}
module Graphics.FreetypeGL.Mat4
    ( Mat4(..), withMat4Ptr
    , identity, ortho
    ) where

import qualified Bindings.FreetypeGL.Mat4 as M4
import           Foreign.C.Types (CFloat(..))
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (Storable(..))

data Mat4 = Mat4
    { m00 :: !CFloat
    , m01 :: !CFloat
    , m02 :: !CFloat
    , m03 :: !CFloat
    , m10 :: !CFloat
    , m11 :: !CFloat
    , m12 :: !CFloat
    , m13 :: !CFloat
    , m20 :: !CFloat
    , m21 :: !CFloat
    , m22 :: !CFloat
    , m23 :: !CFloat
    , m30 :: !CFloat
    , m31 :: !CFloat
    , m32 :: !CFloat
    , m33 :: !CFloat
    } deriving (Eq, Show)

identity :: Mat4
identity =
    Mat4
    1 0 0 0
    0 1 0 0
    0 0 1 0
    0 0 0 1

ortho
    :: Float -- ^ left
    -> Float -- ^ right
    -> Float -- ^ bottom
    -> Float -- ^ top
    -> Float -- ^ znear
    -> Float -- ^ zfar
    -> Mat4
ortho left right bottom top znear zfar =
    Mat4
    m00 0   0   0
    0   m11 0   0
    0   0   m22 0
    m30 m31 m32 1.0
    where
        m00 = realToFrac $ 2.0/(right-left)
        m30 = realToFrac $ -(right+left)/(right-left)
        m11 = realToFrac $ 2.0/(top-bottom)
        m31 = realToFrac $ -(top+bottom)/(top-bottom)
        m22 = realToFrac $ -2.0/(zfar-znear)
        m32 = realToFrac $ -(zfar+znear)/(zfar-znear)

withMat4Ptr :: Mat4 -> (Ptr M4.C'mat4 -> IO a) -> IO a
withMat4Ptr Mat4{..} act =
    alloca $ \ptr ->
    do
        let mat4 = M4.C'mat4 m00 m01 m02 m03 m10 m11 m12 m13 m20 m21 m22 m23 m30 m31 m32 m33
        poke ptr mat4
        act ptr
