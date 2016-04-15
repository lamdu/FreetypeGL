-- | shader.h support

module Graphics.FreetypeGL.Shader
    ( Shader(..), new
    , newTextShader
    , TextShaderUniforms(..), bindTextShaderUniforms
    ) where

import           Bindings.FreetypeGL.Paths (textShaderVertPath, textShaderFragPath)
import qualified Bindings.FreetypeGL.Shader as Shader
import           Foreign.C.String (withCString)
import           Graphics.FreetypeGL.Mat4 (Mat4)
import qualified Graphics.FreetypeGL.Mat4 as Mat4

newtype Shader = Shader Word

-- TODO: Replace this, the C side crashes if the opens/reads fail!
new :: FilePath -> FilePath -> IO Shader
new vertPath fragPath =
    withCString vertPath $ \cStrVertPath ->
    withCString fragPath $ \cStrFragPath ->
    Shader . fromIntegral <$> Shader.c'shader_load cStrVertPath cStrFragPath

-- requires uniform bindings for model, view, projection
newTextShader :: IO Shader
newTextShader =
    do
        v <- textShaderVertPath
        f <- textShaderFragPath
        new v f

data TextShaderUniforms = TextShaderUniforms
    { textShaderModel :: !Mat4
    , textShaderView :: !Mat4
    , textShaderProjection :: !Mat4
    }

bindTextShaderUniforms :: Shader -> TextShaderUniforms -> IO ()
bindTextShaderUniforms (Shader shader) uniforms =
    Mat4.withMat4Ptr (textShaderModel uniforms) $ \model ->
    Mat4.withMat4Ptr (textShaderView uniforms) $ \view ->
    Mat4.withMat4Ptr (textShaderProjection uniforms) $ \projection ->
    Shader.c'wrapper__bind_text_shader_uniforms (fromIntegral shader) model view projection
