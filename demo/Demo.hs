{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Exception (bracket_, bracket)
import           Control.Monad (forM_, unless)
import           Control.Monad.Trans.State (evalStateT)
import           Data.Monoid ((<>))
import qualified Data.Text as Text
import           Graphics.FreetypeGL.Init (initFreetypeGL)
import qualified Graphics.FreetypeGL.Markup as Markup
import           Graphics.FreetypeGL.Shaders (TextShaderProgram(..), TextShaderUniforms(..))
import qualified Graphics.FreetypeGL.Shaders as Shaders
import           Graphics.FreetypeGL.TextBuffer (TextBuffer)
import qualified Graphics.FreetypeGL.TextBuffer as TextBuffer
import           Graphics.FreetypeGL.TextureAtlas (TextureAtlas)
import qualified Graphics.FreetypeGL.TextureAtlas as TextureAtlas
import qualified Graphics.FreetypeGL.TextureFont as TextureFont
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import           System.Environment (getArgs)

assert :: String -> Bool -> IO ()
assert _ True = return ()
assert msg False = fail msg

ortho ::
    GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.GLfloat ->
    GL.GLfloat -> GL.GLfloat -> IO (GL.GLmatrix GL.GLfloat)
ortho left right bottom top near far =
    GL.newMatrix GL.ColumnMajor
    [ 2/(right-left), 0, 0, 0
    , 0, 2/(top-bottom), 0, 0
    , 0, 0, -2/(far-near), 0
    , -(right+left)/(right-left), -(top+bottom)/(top-bottom), -(far+near)/(far-near), 1
    ]

ident :: IO (GL.GLmatrix GL.GLfloat)
ident =
    GL.newMatrix GL.ColumnMajor
    [ 1, 0, 0, 0
    , 0, 1, 0, 0
    , 0, 0, 1, 0
    , 0, 0, 0, 1
    ]

loop :: GLFW.Window -> [([TextShaderProgram], TextureAtlas, TextBuffer)] -> IO ()
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

                        (xres, yres) <- GLFW.getFramebufferSize win

                        identMat <- ident
                        orthoMat <-
                            ortho 0 (fromIntegral xres) 0 (fromIntegral yres) (-1) 1
                        forM_ tuples $ \(shaders, atlas, textBuffer) ->
                            do
                                TextureAtlas.upload atlas
                                forM_ shaders $ \shader ->
                                    do
                                        GL.currentProgram $= Just (shaderProgram shader)
                                        let uniforms = shaderUniforms shader
                                        GL.uniform (uniformModel uniforms) $= identMat
                                        GL.uniform (uniformView uniforms) $= identMat
                                        GL.uniform (uniformProjection uniforms) $= orthoMat
                                        TextBuffer.render shader atlas textBuffer
                        GLFW.swapBuffers win
                        GLFW.pollEvents
                        go (i+1)

withTextBuffer :: (TextBuffer -> IO a) -> IO a
withTextBuffer = bracket TextBuffer.new TextBuffer.delete

main :: IO ()
main =
    do
        [ttfPath] <- getArgs
        bracket_ (GLFW.init >>= assert "GLFW.init failed") GLFW.terminate $
            do
                Just win <- GLFW.createWindow 320 800 "freetype-gl-demo" Nothing Nothing
                GLFW.makeContextCurrent $ Just win
                atlas <- TextureAtlas.new 512 512 TextureAtlas.LCD_FILTERING_OFF
                lcdAtlas <- TextureAtlas.new 512 512 TextureAtlas.LCD_FILTERING_ON
                (xres, yres) <- GLFW.getFramebufferSize win
                let fontSize = 16 * fromIntegral xres / 320
                normFont <- TextureFont.newFromFile atlas fontSize TextureFont.RenderNormal ttfPath
                dfFont <- TextureFont.newFromFile atlas fontSize TextureFont.RenderSignedDistanceField ttfPath
                lcdFont <- TextureFont.newFromFile lcdAtlas fontSize TextureFont.RenderNormal ttfPath
                shader <- Shaders.normalShader
                lcdShaders <- Shaders.lcdShaders
                dfShader <- Shaders.distanceFieldShader
                GLFW.swapInterval 1
                initFreetypeGL
                GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral xres) (fromIntegral yres))
                withTextBuffer $ \normTextBuffer ->
                    withTextBuffer $ \lcdTextBuffer ->
                    withTextBuffer $ \dfTextBuffer ->
                        do
                            let mkAddText font buf markup =
                                    TextBuffer.addText buf markup font
                            let addTexts =
                                    [ ("Normal", mkAddText normFont normTextBuffer)
                                    , ("DF", mkAddText dfFont dfTextBuffer)
                                    , ("LCD", mkAddText lcdFont lcdTextBuffer)
                                    ]
                            (`evalStateT` TextBuffer.Pen 0 (fromIntegral yres)) $
                                forM_ addTexts $ \(annotation, addText) ->
                                do
                                    addText Markup.def (annotation <> "\n")
                                    forM_ [2.5,2..1] $ \g ->
                                        do
                                            let text = addText Markup.def { Markup.gamma = g }
                                            text $ "Gamma = " <> Text.pack (show g) <> "!\n"
                                            text "0123456789ABCDEF abcdef\n\n"
                            loop win
                                [ ([shader], atlas, normTextBuffer)
                                , ( [ Shaders.textLcdPassA lcdShaders
                                    , Shaders.textLcdPassB lcdShaders]
                                  , lcdAtlas, lcdTextBuffer)
                                , ([dfShader], atlas, dfTextBuffer)
                                ]
