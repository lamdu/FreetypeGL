-- | shader.h support

module Graphics.FreetypeGL.Shader
    ( Shader(..), new
    , newTextShader, newDistanceFieldShader
    , TextShaderUniforms(..), bindTextShaderUniforms
    ) where

import qualified Bindings.FreetypeGL.Paths as Paths
import qualified Bindings.FreetypeGL.Shader as Shader
import           Control.Monad (forM_, unless)
import           Foreign.C.String (withCString)
import           Graphics.FreetypeGL.Mat4 (Mat4)
import qualified Graphics.FreetypeGL.Mat4 as Mat4
import           Graphics.FreetypeGL.RGBA (RGBA)
import qualified Graphics.FreetypeGL.RGBA as RGBA
import           System.Directory (doesFileExist)

newtype Shader = Shader Word

-- TODO: Replace this, the C side crashes if the opens/reads fail!
new :: FilePath -> FilePath -> IO Shader
new vertPath fragPath =
    do
        forM_ [vertPath, fragPath] $ \path -> do
            exists <- doesFileExist path
            unless exists $ fail $ "Missing file: " ++ show path
        withCString vertPath $ \cStrVertPath ->
            withCString fragPath $ \cStrFragPath ->
            Shader . fromIntegral <$> Shader.c'shader_load cStrVertPath cStrFragPath

data TextShaderUniforms = TextShaderUniforms
    { textShaderModel :: !Mat4
    , textShaderView :: !Mat4
    , textShaderProjection :: !Mat4
    }

newTextShader :: IO Shader
newTextShader =
    do
        v <- Paths.textShaderVert
        f <- Paths.textShaderFrag
        new v f

newDistanceFieldShader :: IO Shader
newDistanceFieldShader =
    do
        v <- Paths.textDistanceFieldShaderVert
        f <- Paths.textDistanceFieldShaderFrag
        new v f

bindTextShaderUniforms :: Shader -> TextShaderUniforms -> IO ()
bindTextShaderUniforms (Shader shader) uniforms =
    Mat4.withMat4Ptr (textShaderModel uniforms) $ \model ->
    Mat4.withMat4Ptr (textShaderView uniforms) $ \view ->
    Mat4.withMat4Ptr (textShaderProjection uniforms) $ \projection ->
    Shader.c'wrapper__bind_text_shader_uniforms (fromIntegral shader) model view projection
