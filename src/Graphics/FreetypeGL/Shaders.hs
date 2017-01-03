{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Graphics.FreetypeGL.Shaders
    ( TextShaderProgram(..), TextShaderUniforms(..), TextLcdShaders(..)
    , normalShader, lcdShaders, distanceFieldShader
    ) where

import qualified Bindings.FreetypeGL.Paths as Paths
import           Control.Monad (join, unless, when)
import qualified Data.ByteString as BS
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL

data TextShaderUniforms a = TextShaderUniforms
    { uniformTexture :: a
    , uniformMPixel :: Maybe a
    , uniformModel :: a
    , uniformView :: a
    , uniformProjection :: a
    , uniformMColor :: Maybe a
    } deriving (Functor, Foldable, Traversable)

data TextShaderProgram = TextShaderProgram
    { shaderProgram :: GL.Program
    , shaderBlendFunc :: (GL.BlendingFactor, GL.BlendingFactor)
    , shaderUniforms :: TextShaderUniforms GL.UniformLocation
    }

-- For sub-pixel rendering on LCD screens freetype-gl uses two rendering passes.
-- One pass obscures the background and the other adds the color.
-- This could had been achieved in one pass using OpenGL extensions -
-- either GL_ARB_blend_func_extended or GL_EXT_blend_color
data TextLcdShaders =
    TextLcdShaders
    { textLcdPassA :: TextShaderProgram
    , textLcdPassB :: TextShaderProgram
    }

glOp ::
    String -> (obj -> IO ()) -> (obj -> IO Bool) -> (obj -> IO String) -> obj ->
    IO ()
glOp msg op status infoLog obj =
    do
        op obj
        success <- status obj
        unless success $
            do
                info <- infoLog obj
                fail $ msg ++ " failed: " ++ info

loadShader :: GL.ShaderType -> FilePath -> IO GL.Shader
loadShader shaderType filename =
    do
        shader <- GL.createShader shaderType
        shaderSource <- BS.readFile filename
        GL.shaderSourceBS shader $= shaderSource
        glOp ("Shader compilation of " ++ show filename)
            GL.compileShader GL.compileStatus GL.shaderInfoLog shader
        return shader

loadProgram :: FilePath -> FilePath -> IO GL.Program
loadProgram vertFilename fragFilename =
    do
        vert <- loadShader GL.VertexShader vertFilename
        frag <- loadShader GL.FragmentShader fragFilename
        prog <- GL.createProgram
        GL.attachShader prog vert
        GL.deleteObjectName vert
        GL.attachShader prog frag
        GL.deleteObjectName frag
        glOp "Shader program link"
            GL.linkProgram GL.linkStatus GL.programInfoLog prog
        return prog

getUniformLocation :: GL.Program -> String -> IO GL.UniformLocation
getUniformLocation program name =
    do
        loc@(GL.UniformLocation val) <- GL.uniformLocation program name
        when (val == -1) $
            fail ("Uniform " ++ show name ++ " does not exist in program")
        return loc

commonShader ::
    (GL.BlendingFactor, GL.BlendingFactor) -> IO FilePath ->
    IO TextShaderProgram
commonShader blend fragPath =
    do
        prog <-
            join $ loadProgram <$> Paths.textShaderVert <*> fragPath
        TextShaderProgram prog blend <$>
            traverse (getUniformLocation prog) uniformNames
    where
        uniformNames =
            TextShaderUniforms
            { uniformTexture = "tex"
            , uniformMPixel = Just "pixel"
            , uniformModel = "model"
            , uniformView = "view"
            , uniformProjection = "projection"
            , uniformMColor = Nothing
            }

normalShader :: IO TextShaderProgram
normalShader =
    commonShader (GL.SrcAlpha, GL.OneMinusSrcAlpha) Paths.textShaderFrag

lcdShaders :: IO TextLcdShaders
lcdShaders =
    TextLcdShaders
    <$> commonShader (GL.Zero, GL.OneMinusSrcColor) Paths.textTwoPassAFrag
    <*> commonShader (GL.SrcAlpha, GL.One) Paths.textTwoPassBFrag

distanceFieldShader :: IO TextShaderProgram
distanceFieldShader =
    do
        prog <-
            join $
            loadProgram
            <$> Paths.textDistanceFieldShaderVert
            <*> Paths.textDistanceFieldShaderFrag
        TextShaderProgram prog (GL.SrcAlpha, GL.OneMinusSrcAlpha) <$>
            traverse (getUniformLocation prog) uniformNames
    where
        uniformNames =
            TextShaderUniforms
            { uniformTexture = "u_texture"
            , uniformMPixel = Nothing
            , uniformModel = "u_model"
            , uniformView = "u_view"
            , uniformProjection = "u_projection"
            , uniformMColor = Just "u_color"
            }
