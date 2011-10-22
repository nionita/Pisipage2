module Gallery where
import Control.Monad (liftM)
import Data.Char (isDigit)
import Data.List (sort)
import qualified Data.Map as M
import qualified System.FilePath as FP
import System.Directory (getDirectoryContents, doesFileExist)
import System.IO (TextEncoding)
import Text.XHtml
import Text.XHtml.Table (simpleTable)
import Base
import Common
import Helpers
import Catalog

data GalleryArgs = GalleryArgs {
                       gaDir :: String,     -- directory with the gallery files
                       gaTextF :: Maybe String,    -- text file (optional)
                       gaCat :: Catalog     -- the catalog
            }

type GalText = M.Map Lang String

data Gallery = Gallery {
            galId :: Ident,         -- gallery ident
            galDir :: String,       -- gallery directory name
            galFiles :: [String],   -- list of gallery files (names)
            galCat :: Catalog,      -- catalog
            galText :: GalText      -- gallery text per language
        }

instance Page Gallery where
    getIdent = galId
    renderPage = renderGallery

-- This will create a gallery structure given the gallery ident,
-- base directory and gallery arguments
getGallery :: TextEncoding -> Ident -> String -> GalleryArgs -> IO Gallery
getGallery tenc gid dir gargs = do
    let rbdir = dir FP.</> "Bilder" FP.</> gaDir gargs
    fils <- getGalContent tenc rbdir
    mtxt <- case gaTextF gargs of
                Just tfname -> M.fromList `fmap` getGalTexts tenc dir tfname
                Nothing     -> return M.empty
    let gal = Gallery {
                  galId = gid,
                  galDir = gaDir gargs,
                  galFiles = map noFileExt $ filter isJpeg fils,
                  galCat = gaCat gargs,
                  galText = mtxt
              }
    return gal

-- If we have a content file then this are the pictures
-- otherwise, everyt jpg in the directory
getGalContent :: TextEncoding -> String -> IO [String]
getGalContent tenc dir = do
    let cofname = dir FP.</> "content.txt"
    cof <- doesFileExist cofname
    if cof then do cf <- myReadFile tenc cofname
                   return $ lines cf
           else do cf <- getDirectoryContents dir
                   return $ sort cf

getGalTexts tenc dir name
    = sequence [ getGLangText tenc dir name la | la <- [De, En, Ro]]

getGLangText tenc dir name lang = do
    let fn = dir FP.</> "Text" FP.</> show lang FP.</> name
    fex <- doesFileExist fn
    fcont <- if fex then myReadFile tenc fn
                    else return ""
    return (lang, fcont)

isJpeg :: String -> Bool
isJpeg f = snd (FP.splitExtension f) == ".jpg"

noFileExt = fst . FP.splitExtension

-- This function renders the gallery
renderGallery :: Gallery -> Lang -> RenderType -> [(PageIdent, String)]
renderGallery gal la rt
    | rt == NoJS = [ makePIS i | i <- [0..length fils - 1]]
    | otherwise  = [ makePIS 0 ]
    where Gallery {
            galId = myid,
            galDir = gdir,
            galFiles = fils,
            galCat = cat,
            galText = txt } = gal
          pid = PageIdent { ident = myid, rlang = la, rtype = rt, elmt = 0 }
          bbase = "../Bilder/" ++ gdir ++ "/"   -- here file path is unix style
          tbase = "../Thumbs/" ++ gdir ++ "/"   -- and here
          sbase = "../Bilder/Style/"            -- and here too
          t = myStringToHtml (maybe "" id $ M.lookup la txt)
          t1div = thediv `hid` "text" << t
          t2div = thediv `hid` "textshad" << t
          makePIS i = (makePI i, makeS i)
          makePI i = pid { elmt = i }
          makeS i = makeStr (makeCont i) (makeNav i)
          makeStr c n = prettyHtml $ basicStruct la rt pid c n
          makeCont i = thediv `hid` "bigpic" << makeBigImg (fils!!i)
                         +++ t1div +++ t2div
          makeBigImg f = setBTitle f cat
                            $ image `hid` "bild"
                                ! [ alt "Bild", src $ bbase ++ f ++ ".jpg" ]
          makeNav i = jsInitDiv
                     +++ thediv `hid` "thumbs" << (
                             thediv `hid` "thumbsnav" << mkThumbsNav i
                         +++ thediv `hid` "thumbspic" <<
                                (foldl1 (+++) $ map (mkThumb i) $ zip fils [0..])
                         )
          mkThumbsNav i =
              let back = if i == 0 || rt == JS
                            then bt
                            else anchor ! [ href prevp, oncb ] << bt
                  forw = if i == length fils - 1 || rt == JS
                            then ft
                            else anchor ! [ href nextp, oncf ] << ft
                  prevp = htPath la $ pid { elmt = i - 1 }
                  nextp = htPath la $ pid { elmt = i + 1 }
                  bt = image `hid` "back" ! [ oncb, srcb ]
                  ft = image `hid` "forw" ! [ oncf, srcf ]
                  oncb = strAttr "onclick" "javascript:crti--;updatePic()"
                  oncf = strAttr "onclick" "javascript:crti++;updatePic()"
                  srcb = src $ sbase ++ "back.jpg"
                  srcf = src $ sbase ++ "forw.jpg"
              in simpleTable [ identifier "tntab" ]
                             [ theclass "tncell" ]
                             [ [ back, forw ] ]
          mkThumb crt (f, i)
              = let src1 = src $ tbase ++ f ++ ".jpg"
                    onclk = strAttr "onclick"
                               ("javascript:crti=" ++ show i ++ ";updatePic()")
                    iatr = if rt == NoJS then [ src1 ] else [ onclk, src1 ]
                    img = setTTitle f cat $ image `hclass` "thumb" ! iatr 
                in if crt == i || rt == JS
                        then img
                        else anchor `hclass` "thumb"
                               ! [ href $ htPath la $ pid { elmt = i }] << img
          jsInitDiv = if rt == NoJS
                        then noHtml
                        else thediv `hid` "jsinit" << jsinit
          jsinit = script ! [ lang "javascript" ] <<
                     myStringToHtml(
                        "var crti=0;var b='" ++ bbase
                           ++ "';var p=new Array();var t=new Array();"
                           ++ concatMap mkPicArrEl (zip [0..] fils)
                           ++ concatMap mkTxtArrEl (zip [0..] fils)
                       )
          mkPicArrEl (i, f) = "p[" ++ show i ++ "]='" ++ f ++ ".jpg';"
          mkTxtArrEl (i, f) = "t[" ++ show i ++ "]='" ++ t ++ "';"
              where t = catDesc f cat makeBTitle

-- Take a description of a picture from file name, a catalog and a
-- catalog function
catDesc fil cat cfun = maybe fil cfun
                        $ getKeyFromFileName fil >>= flip findCatElem cat

-- Given an Html element, decorate it with a title attribute
-- based on the file name and the given catalog; title, year, technique
-- and dimensions of the painting are considered
setBTitle :: String -> Catalog -> Html -> Html
setBTitle fil cat html = html ! [ strAttr "title" tit ] 
    where tit = catDesc fil cat makeBTitle

-- Given an Html element, decorate it with a title attribute
-- based on the file name and the given catalog; only the paintings title
-- is considered
setTTitle :: String -> Catalog -> Html -> Html
setTTitle fil cat html = html ! [ strAttr "title" tit ] 
    where tit = catDesc fil cat makeTTitle

-- Extract the catalog key from the file name
-- The file name has the structure:
-- [<ord>-]<key>-rest
-- where ord is for correct order of the thumbs
getKeyFromFileName fn
    | len == 3 = Just $ subs!!i
    | len == 2 = Just $ subs!!0
    | otherwise = Nothing
    where subs = mbreak 3 (== '-') fn
          len = length subs
          i = if all isDigit (subs!!0) then 1 else 0
