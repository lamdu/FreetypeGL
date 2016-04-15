-- | A mini wrapper for the GLEW init function

module Graphics.GL.GLEW.Init
    ( initGlew
    ) where

import qualified Bindings.GLEW.Init as GLEW
import           Foreign.Marshal.Error (throwIf_)

initGlew :: IO ()
initGlew =
    throwIf_ (/= GLEW.c'GLEW_OK)
    (error . ("glewInit error: " ++) . show)
    GLEW.c'glewInit
