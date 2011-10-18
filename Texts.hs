module Texts where
import Data.List (intersperse)
import qualified System.FilePath as FP ((</>))
import Text.XHtml
import Base
import Common
import Helpers

-- We need the following information:
-- a list of files, every of which containing (on the first line) a theme
--    and a number of further lines, which will be rendered each as a
--    paragraph, after the theme.
-- a mode to render the themes: as theme with paragraphs or as entries
--   in a two cols table (for rendering the exhibitions)
-- maybe a image file name to be rendered together with the text (for vita)
-- The files are located in Text/<lang> and will be rendered exactly in the
-- order in which they appear in the list

data TextRenderMode = Theme | Exhib

data TextsArgs = TextsArgs {
                    tfiles :: [String],
                    tmode :: TextRenderMode,
                    timg :: Maybe String
                    }

data TextGrp = TextGrp { ttheme :: String, pars :: [String] }

data Texts = Texts {
                txtId :: Ident,
                txtGrps :: [(Lang, [TextGrp])],
                txtMode :: TextRenderMode,
                txtImg :: Maybe String
                }

instance Page Texts where
    getIdent = textsIdent
    renderPage = renderTexts

textsIdent = txtId

-- This will create a texts content structure given the page ident,
-- the home directory and the links parameters
getTexts :: Ident -> String -> TextsArgs -> IO Texts
getTexts pid dir teargs = do
    prs <- sequence [ getTLang la dir1 teargs | la <- [De, En, Ro]]
    return $ Texts {
               txtId = pid, txtGrps = prs,
               txtMode = tmode teargs, txtImg = timg teargs
               }
    where dir1 = dir FP.</> "Text"

getTLang lang dir teargs = do
    let base = dir FP.</> show lang
    tgrps <- mapM (fileToTGrp base) (tfiles teargs)
    return (lang, tgrps)

-- Read a file and make a TextsGrp
-- First line is the theme, the rest is one paragraph per line
-- in the paragraph can be text mixed with images (:<file>:)
fileToTGrp base filn = do
    lns <- lines `fmap` readFile (base FP.</> filn)
    let theme : rest = lns
        grp = if null lns
              then TextGrp { ttheme = "File " ++ filn ++ " is empty!", pars = [] }
              else TextGrp { ttheme = theme, pars = map lineToPar rest }
    return grp

lineToPar = id

-- This will render the texts page
renderTexts :: Texts -> Lang -> RenderType -> [(PageIdent, String)]
renderTexts tx la rt = [(pid, str1)]
    where myid = txtId tx
          gs = txtGrps tx
          pid = PageIdent { ident = myid, rlang = la, rtype = rt, elmt = 0 }
          str1 = prettyHtml $ basicStruct la rt pid cont noHtml
          ls1 = maybe defls id $ lookup la gs
          defls = [TextGrp { ttheme = "No text for " ++ show la, pars = []}]
          rimg = maybe noHtml (\f -> image ! [src f]) $ txtImg tx
          cont = thediv `hid` "normtext"
                     << thediv `hid` myid << (rimg +++ rcont)
          rcont = case txtMode tx of
                    Theme -> renderLCont ls1
                    Exhib -> renderECont ls1

-- render text content as themes:
renderLCont ls = foldl1 (+++) $ intersperse brs $ map renderTextsGrp ls
    where brs = br +++ br

renderTextsGrp lg = foldl (+++) stheme $ map renderOne $ pars lg
    where stheme = if null (ttheme lg)
                      then noHtml
                      else h4 << myStringToHtml (ttheme lg)

-- Render one paragraph with the possibility to insert raw html
-- for strings beginning with '\'
renderOne ps = rend ps +++ br
    where rend ('\\':as) = primHtml as
          rend as        = myStringToHtml as

-- render text content as exhibitions table
renderECont ls = simpleTable [] [] $ concatMap renderAsRows ls

renderAsRows lg = zipWith f (myStringToHtml (ttheme lg) : repeat noHtml)
                      $ map myStringToHtml $ pars lg
    where f a b = [a, b]
