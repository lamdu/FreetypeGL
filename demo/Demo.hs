{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- TODO: Dont use bindings directly in demo
import           Control.Monad (when)
import qualified Bindings.FreetypeGL.Paths as Paths
import           Control.Exception (bracket_, bracket)
import           Control.Monad (forM_, join, unless)
import           Control.Monad.Trans.State (evalStateT)
import qualified Data.ByteString as BS
import           Data.Monoid ((<>))
-- import           Data.Text (Text)
import qualified Data.Text as Text
import           Graphics.FreetypeGL.Init (initFreetypeGL)
-- import           Graphics.FreetypeGL.Markup (Markup)
import qualified Graphics.FreetypeGL.Markup as Markup
-- import           Graphics.FreetypeGL.RGBA (RGBA(..))
import           Graphics.FreetypeGL.TextBuffer (TextBuffer)
import qualified Graphics.FreetypeGL.TextBuffer as TextBuffer
import qualified Graphics.FreetypeGL.TextureAtlas as TextureAtlas
import           Graphics.FreetypeGL.TextureAtlas (TextureAtlas)
import qualified Graphics.FreetypeGL.TextureFont as TextureFont
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
-- import qualified Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects as GL
import qualified Graphics.UI.GLFW as GLFW
import           System.Environment (getArgs)

assert :: String -> Bool -> IO ()
assert _ True = return ()
assert msg False = fail msg

xres :: Num a => a
xres = 320

yres :: Num a => a
yres = 800

getUniformLocation :: GL.Program -> String -> IO GL.UniformLocation
getUniformLocation program name =
    do
        loc@(GL.UniformLocation val) <- GL.uniformLocation program name
        when (val == -1) $
            fail ("Uniform " ++ show name ++ " does not exist in program")
        return loc

setUniform :: GL.Uniform a => GL.Program -> String -> a -> IO ()
setUniform program name val =
    do
        location <- getUniformLocation program name
        GL.uniform location $= val

ortho1 ::
    GL.GLdouble -> GL.GLdouble -> GL.GLdouble -> GL.GLdouble ->
    GL.GLdouble -> GL.GLdouble -> IO (GL.GLmatrix GL.GLdouble)
ortho1 left right bottom top near far =
    GL.newMatrix GL.ColumnMajor
    [ 2/(right-left), 0, 0, -(right+left)/(right-left)
    , 0, 2/(top-bottom), 0, -(top+bottom)/(top-bottom)
    , 0, 0, -2/(far-near), -(far+near)/(far-near)
    , 0, 0, 0, 1
    ]

ortho2 ::
    GL.GLdouble -> GL.GLdouble -> GL.GLdouble -> GL.GLdouble ->
    GL.GLdouble -> GL.GLdouble -> IO (GL.GLmatrix GL.GLdouble)
ortho2 left right bottom top near far =
    GL.newMatrix GL.ColumnMajor
    [ 2/(right-left), 0, 0, 0
    , 0, 2/(top-bottom), 0, 0
    , 0, 0, -2/(far-near), 0
    , -(right+left)/(right-left), -(top+bottom)/(top-bottom), -(far+near)/(far-near), 1
    ]

ident :: IO (GL.GLmatrix GL.GLdouble)
ident =
    GL.newMatrix GL.ColumnMajor
    [ 1, 0, 0, 0
    , 0, 1, 0, 0
    , 0, 0, 1, 0
    , 0, 0, 0, 1
    ]

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

loop :: GLFW.Window -> [(GL.Program, TextureAtlas, TextBuffer)] -> IO ()
loop win tuples =
    go (0::Int)
    where
        go i =
            do
                close <- GLFW.windowShouldClose win
                unless close $
                    do
                        GL.clearColor $= GL.Color4 0 0 0 0
                        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
                        GL.blend $= GL.Enabled
                        GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

                        -- shtut
                        GL.currentProgram $= Nothing
                        GL.matrixMode $= GL.Projection
                        GL.loadIdentity
                        GL.ortho 0 xres 0 yres (-1) 1
                        GL.color (GL.Color4 (1 :: GL.GLfloat) 0 0 1)
                        GL.renderPrimitive GL.Lines $ do
                            GL.vertex (GL.Vertex3 (0::Double) 0 0)
                            GL.vertex (GL.Vertex3 (100::Double) 100 0)
                        GL.loadIdentity

                        identMat <- ident
                        orthoMat <-
                            (if even i then ortho1 else ortho2) 0 xres 0 yres (-1) 1
                        forM_ tuples $ \(shader, atlas, textBuffer) ->
                            do
                                GL.currentProgram $= Just shader
                                setUniform shader "model" identMat
                                setUniform shader "view" identMat
                                setUniform shader "projection" orthoMat
                                tex <- getUniformLocation shader "tex"
                                pixel <- getUniformLocation shader "pixel"
                                let params =
                                        TextBuffer.RenderParams
                                        { TextBuffer.renderShader = shader
                                        , TextBuffer.renderShaderTex = tex
                                        , TextBuffer.renderShaderPixel = pixel
                                        }
                                TextBuffer.render params atlas textBuffer
                        GLFW.swapBuffers win
                        GLFW.pollEvents
                        go (i+1)

withTextBuffer :: (TextBuffer -> IO a) -> IO a
withTextBuffer = bracket TextBuffer.new TextBuffer.delete


main :: IO ()
main =
    do
        [ttfPath] <- getArgs
        atlas <- TextureAtlas.new 512 512 TextureAtlas.LCD_FILTERING_OFF
        lcdAtlas <- TextureAtlas.new 512 512 TextureAtlas.LCD_FILTERING_ON
        normFont <- TextureFont.newFromFile atlas 16 ttfPath
        lcdFont <- TextureFont.newFromFile lcdAtlas 16 ttfPath
        bracket_ (GLFW.init >>= assert "GLFW.init failed") GLFW.terminate $
            do
                Just win <- GLFW.createWindow xres yres "freetype-gl-demo" Nothing Nothing
                GLFW.makeContextCurrent $ Just win
                shader <-
                    join $ loadProgram <$> Paths.textShaderVert <*> Paths.textShaderFrag
                -- dfShader <-
                --     join $
                --     loadProgram
                --     <$> Paths.textDistanceFieldShaderVert
                --     <*> Paths.textDistanceFieldShaderFrag
                GLFW.swapInterval 1
                initFreetypeGL
                GL.viewport $= (GL.Position 0 0, GL.Size xres yres)
                withTextBuffer $ \normTextBuffer ->
                    withTextBuffer $ \lcdTextBuffer ->
                    -- withTextBuffer $ \dfTextBuffer ->
                        do
                            let mkAddText font buf markup =
                                    TextBuffer.addText buf markup font
                            let addTexts =
                                    [ ("Normal", mkAddText normFont normTextBuffer)
                                    -- , ("DF", mkAddText normFont dfTextBuffer)
                                    , ("LCD", mkAddText lcdFont lcdTextBuffer)
                                    ]
                            (`evalStateT` TextBuffer.Pen 0 yres) $
                                forM_ addTexts $ \(annotation, addText) ->
                                do
                                    addText Markup.def (annotation <> "\n")
                                    forM_ [2.5,2..1] $ \g ->
                                        do
                                            let text = addText Markup.def { Markup.gamma = g }
                                            text $ "Gamma = " <> Text.pack (show g) <> "!\n"
                                            text "0123456789ABCDEF abcdef\n\n"
                            loop win
                                [ (shader, atlas, normTextBuffer)
                                , (shader, lcdAtlas, lcdTextBuffer)
                                -- , (dfShader, atlas, dfTextBuffer)
                                ]
