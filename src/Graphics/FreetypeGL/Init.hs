-- | A mini wrapper for the GLEW init function
{-# LANGUAGE CPP #-}
module Graphics.FreetypeGL.Init
    ( initFreetypeGL
    ) where

#ifndef darwin_HOST_OS
import qualified Bindings.GLEW.Init as GLEW
import           Foreign.Marshal.Error (throwIf_)
#endif

initFreetypeGL :: IO ()
#ifdef darwin_HOST_OS
initFreetypeGL = return ()
#else
initFreetypeGL =
    throwIf_ (/= GLEW.c'GLEW_OK)
    (error . ("glewInit error: " ++) . show)
    GLEW.c'glewInit
#endif
