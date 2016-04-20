{-# LANGUAGE CPP #-}

module Main (main) where

import           Control.Exception (bracket_, bracket)
import           Control.Monad (unless)
import           Control.Monad.Trans.State (evalStateT)
import qualified Graphics.FreetypeGL.FontManager as FontManager
import qualified Graphics.FreetypeGL.Markup as Markup
import qualified Graphics.FreetypeGL.Mat4 as Mat4
import           Graphics.FreetypeGL.Shader (Shader)
import qualified Graphics.FreetypeGL.Shader as Shader
import           Graphics.FreetypeGL.TextBuffer (TextBuffer)
import qualified Graphics.FreetypeGL.TextBuffer as TextBuffer
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import           System.Environment (getArgs)

#ifndef darwin_HOST_OS
import qualified Graphics.GL.GLEW.Init as GLEW
#endif

assert :: String -> Bool -> IO ()
assert _ True = return ()
assert msg False = fail msg

loop :: GLFW.Window -> Shader -> TextBuffer -> IO ()
loop win shader textBuffer =
    go
    where
        go =
            do
                close <- GLFW.windowShouldClose win
                unless close $
                    do
                        GL.clearColor $= GL.Color4 0 0 0 0
                        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
                        GL.color $ GL.Color4 (1::Float) 1 1 1
                        Shader.bindTextShaderUniforms shader Shader.TextShaderUniforms
                            { Shader.textShaderModel = Mat4.identity
                            , Shader.textShaderView = Mat4.identity
                            , Shader.textShaderProjection = Mat4.ortho 0 640 0 480 (-1) 1
                            }
                        TextBuffer.render textBuffer
                        GLFW.swapBuffers win
                        GLFW.pollEvents
                        go

main :: IO ()
main =
    do
        [ttfPath] <- getArgs
        bracket_ (GLFW.init >>= assert "GLFW.init failed") GLFW.terminate $
            do
                Just win <- GLFW.createWindow 640 480 "freetype-gl-demo" Nothing Nothing
                GLFW.makeContextCurrent $ Just win
                GLFW.swapInterval 1
#ifndef darwin_HOST_OS
                GLEW.initGlew
#endif
                GL.viewport $= (GL.Position 0 0, GL.Size 640 480)
                shader <- Shader.newTextShader
                bracket
                    (TextBuffer.new TextBuffer.LCD_FILTERING_ON shader)
                    TextBuffer.delete
                    $ \textBuffer ->
                    do
                        manager <- TextBuffer.getFontManager textBuffer
                        font <- FontManager.getFromFileName manager ttfPath 16
                        (`evalStateT` TextBuffer.Pen 0 480) $
                            do
                                TextBuffer.addText textBuffer Markup.def font "Hello world!\n"
                                TextBuffer.addText textBuffer Markup.def font "It finally works!\n"
                                TextBuffer.addText textBuffer Markup.def font "Wowzers!\n"
                                let gamma x = Markup.def { Markup.gamma = x }
                                TextBuffer.addText textBuffer (gamma 0)   font "Gamma = 0!\n"
                                TextBuffer.addText textBuffer (gamma 0.5) font "Gamma = 0.5!\n"
                                TextBuffer.addText textBuffer (gamma 1)   font "Gamma = 1!\n"
                        loop win shader textBuffer
