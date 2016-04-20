-- | A mini wrapper for the GLEW init function
{-# LANGUAGE CPP #-}
module Graphics.GL.GLEW.Init
    ( initGlew
    ) where

#ifndef darwin_HOST_OS
import qualified Bindings.GLEW.Init as GLEW
#endif
import           Foreign.Marshal.Error (throwIf_)

initGlew :: IO ()
#ifdef darwin_HOST_OS
initGlew = return ()
#else
initGlew =
    throwIf_ (/= GLEW.c'GLEW_OK)
    (error . ("glewInit error: " ++) . show)
    GLEW.c'glewInit
#endif
