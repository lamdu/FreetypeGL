module Main (main) where

import           Control.Exception (bracket_, bracket)
import           Control.Monad (forM_, unless)
import           Control.Monad.Trans.State (StateT(..), evalStateT)
import qualified Graphics.FreetypeGL.FontManager as FontManager
import           Graphics.FreetypeGL.Init (initFreetypeGL)
import           Graphics.FreetypeGL.Markup (Markup)
import qualified Graphics.FreetypeGL.Markup as Markup
import qualified Graphics.FreetypeGL.Mat4 as Mat4
import           Graphics.FreetypeGL.RGBA (RGBA(..))
import           Graphics.FreetypeGL.Shader (Shader)
import qualified Graphics.FreetypeGL.Shader as Shader
import           Graphics.FreetypeGL.TextBuffer (TextBuffer, RenderDepth)
import qualified Graphics.FreetypeGL.TextBuffer as TextBuffer
import qualified Graphics.FreetypeGL.TextureAtlas as TextureAtlas
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import           System.Environment (getArgs)

assert :: String -> Bool -> IO ()
assert _ True = return ()
assert msg False = fail msg

xres :: Num a => a
xres = 320

yres :: Num a => a
yres = 800

loop ::
    GLFW.Window ->
    [(Shader, TextBuffer)] ->
    (Shader, TextBuffer) -> IO ()
loop win textPairs (dfShader, dfTextBuffer) =
    go
    where
        go =
            do
                close <- GLFW.windowShouldClose win
                unless close $
                    do
                        GL.clearColor $= GL.Color4 0 0 0 0
                        GL.clear [GL.ColorBuffer, GL.DepthBuffer]
                        GL.blend $= GL.Enabled
                        GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
                        forM_ textPairs $ \(shader, textBuffer) ->
                            do
                                Shader.bindTextShaderUniforms shader Shader.TextShaderUniforms
                                    { Shader.textShaderModel = Mat4.identity
                                    , Shader.textShaderView = Mat4.identity
                                    , Shader.textShaderProjection = Mat4.ortho 0 xres 0 yres (-1) 1
                                    }
                                TextBuffer.render textBuffer
                        Shader.bindDistanceFieldShaderUniforms dfShader Shader.DistanceFieldShaderUniforms
                            { Shader.distanceFieldColor = RGBA 1 1 1 1
                            , Shader.distanceFieldShaderModel = Mat4.identity
                            , Shader.distanceFieldShaderView = Mat4.identity
                            , Shader.distanceFieldShaderProjection = Mat4.ortho 0 xres 0 yres (-1) 1
                            }
                        TextBuffer.render dfTextBuffer
                        GLFW.swapBuffers win
                        GLFW.pollEvents
                        go

withTextBuffer :: Shader -> RenderDepth -> (TextBuffer -> IO a) -> IO a
withTextBuffer shader renderDepth =
    bracket
    (TextBuffer.new renderDepth shader)
    TextBuffer.delete

withDistanceFieldTextBuffer :: (Shader -> TextBuffer -> IO a) -> IO a
withDistanceFieldTextBuffer act =
    do
        shader <- Shader.newDistanceFieldShader
        bracket
            (TextBuffer.new TextBuffer.LCD_FILTERING_OFF shader)
            TextBuffer.delete $
            \textBuffer ->
            do
                manager <- TextBuffer.getFontManager textBuffer
                atlas <- FontManager.getAtlas manager
                TextureAtlas.setMode atlas TextureAtlas.DistanceField
                act shader textBuffer

mkAddText ::
    FilePath -> (String, TextBuffer) ->
    IO (String, Markup -> String -> StateT TextBuffer.Pen IO ())
mkAddText ttfPath (annotation, textBuffer) =
    do
        manager <- TextBuffer.getFontManager textBuffer
        font <- FontManager.getFromFileName manager ttfPath 16
        return . (,) annotation $ \markup -> TextBuffer.addText textBuffer markup font

main :: IO ()
main =
    do
        [ttfPath] <- getArgs
        bracket_ (GLFW.init >>= assert "GLFW.init failed") GLFW.terminate $
            do
                Just win <- GLFW.createWindow xres yres "freetype-gl-demo" Nothing Nothing
                GLFW.makeContextCurrent $ Just win
                GLFW.swapInterval 1
                initFreetypeGL
                GL.viewport $= (GL.Position 0 0, GL.Size xres yres)
                shader <- Shader.newTextShader
                withTextBuffer shader TextBuffer.LCD_FILTERING_OFF $ \normTextBuffer ->
                    withTextBuffer shader TextBuffer.LCD_FILTERING_ON $ \lcdTextBuffer ->
                    withDistanceFieldTextBuffer $ \dfShader dfTextBuffer ->
                        do
                            addTexts <-
                                mapM (mkAddText ttfPath)
                                [ ("Normal", normTextBuffer)
                                , ("DF", dfTextBuffer)
                                , ("LCD", lcdTextBuffer)
                                ]
                            (`evalStateT` TextBuffer.Pen 0 yres) $
                                forM_ addTexts $ \(annotation, addText) ->
                                do
                                    addText Markup.def (annotation ++ "\n")
                                    forM_ [2.5,2..1] $ \g ->
                                        do
                                            let text = addText Markup.def { Markup.gamma = g }
                                            text $ "Gamma = " ++ show g ++ "!\n"
                                            text "0123456789ABCDEF abcdef\n\n"
                            loop win
                                [(shader, normTextBuffer), (shader, lcdTextBuffer)]
                                (dfShader, dfTextBuffer)
