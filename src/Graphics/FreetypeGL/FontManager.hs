-- | Font Manager

module Graphics.FreetypeGL.FontManager
    ( FontManager(..), getFromFileName
    , getAtlas
    , PtSize
    ) where

import qualified Bindings.FreetypeGL.FontManager as FM
import           Foreign.C.String (withCString)
import           Foreign.Marshal.Error (throwIfNull)
import           Foreign.Ptr (Ptr)
import           Foreign.Storable (Storable(..))
import           Graphics.FreetypeGL.TextureAtlas (TextureAtlas(..))
import           Graphics.FreetypeGL.TextureFont (TextureFont(..), PtSize)

data FontManager = FontManager (Ptr FM.C'font_manager_t)

getAtlas :: FontManager -> IO TextureAtlas
getAtlas (FontManager ptr) = TextureAtlas <$> peek (FM.p'font_manager_t'atlas ptr)

getFromFileName :: FontManager -> FilePath -> PtSize -> IO TextureFont
getFromFileName (FontManager ptr) path size =
    withCString path $ \cPath ->
    TextureFont <$>
    throwIfNull ("getFromFileName: " ++ show path ++ " failed")
    (FM.c'font_manager_get_from_filename ptr cPath (realToFrac size))
