{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Graphics.FreetypeGL.Shaders
    ( TextShaderProgram(..), TextShaderUniforms(..)
    , normalShader, lcdShaders, distanceFieldShader
    ) where

import qualified Bindings.FreetypeGL.Shaders as Shaders
import           Control.Monad (join, unless, when)
import           Data.ByteString (ByteString)
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

loadShader :: GL.ShaderType -> ByteString -> IO GL.Shader
loadShader shaderType shaderSource =
    do
        shader <- GL.createShader shaderType
        GL.shaderSourceBS shader $= shaderSource
        glOp ("Shader compilation of: " ++ show shaderSource)
            GL.compileShader GL.compileStatus GL.shaderInfoLog shader
        return shader

loadProgram :: ByteString -> ByteString -> IO GL.Program
loadProgram vertSource fragSource =
    do
        vert <- loadShader GL.VertexShader vertSource
        frag <- loadShader GL.FragmentShader fragSource
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
    (GL.BlendingFactor, GL.BlendingFactor) -> ByteString ->
    IO TextShaderProgram
commonShader blend fragShader =
    do
        prog <- loadProgram Shaders.textShaderVert fragShader
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
    commonShader (GL.SrcAlpha, GL.OneMinusSrcAlpha) Shaders.textShaderFrag

-- For sub-pixel rendering on LCD screens freetype-gl uses two rendering passes.
-- One pass obscures the background and the other adds the color.
-- Note:
-- This could had been achieved in one pass using OpenGL extensions -
-- either GL_ARB_blend_func_extended or GL_EXT_blend_color
lcdShaders :: IO [TextShaderProgram]
lcdShaders =
    sequence
    [ commonShader (GL.Zero, GL.OneMinusSrcColor) Shaders.textTwoPassAFrag
    , commonShader (GL.SrcAlpha, GL.One) Shaders.textTwoPassBFrag
    ]

distanceFieldShader :: IO TextShaderProgram
distanceFieldShader =
    do
        prog <-
            loadProgram
            Shaders.textDistanceFieldShaderVert
            Shaders.textDistanceFieldShaderFrag
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
