module Links where
import qualified System.FilePath as FP ((</>))
import System.IO (TextEncoding)
import Text.XHtml
import Base
import Common
import Helpers

-- We need the following information:
-- a list of files, every of which containing a theme (on the first line)
--    and a number of links (the further lines), one per line, consisting of
--    the link itself (url) and a description, separated by "|"
-- The files are located in Text/<lang> and will be rendered exactly in the
-- order in which they appear in the list

data LinksArgs = LinksArgs { lfiles :: [String] }

data LinksGrp = LinksGrp { ltheme :: String, links :: [Link] }

data Link = Link { llink :: String, ldesc :: String }

data Links = Links Ident [(Lang, [LinksGrp])]

instance Page Links where
    getIdent = linksIdent
    renderPage = renderLinks

linksIdent (Links pid _) = pid

-- This will create a links content structure given the page ident,
-- the home directory and the links parameters
getLinks :: TextEncoding -> Ident -> String -> LinksArgs -> IO Links
getLinks tenc pid dir liargs = do
    prs <- sequence [ getLLang tenc la dir1 liargs | la <- [De, En, Ro]]
    return . Links pid $ prs
    where dir1 = dir FP.</> "Text"

getLLang tenc lang dir liargs = do
    let base = dir FP.</> show lang
    lgrps <- mapM (fileToLGrp tenc base) (lfiles liargs)
    return (lang, lgrps)

-- Read a file and make a LinksGrp
-- First line is the theme, the rest is one link per line, in the form:
-- url | descrition of the link
fileToLGrp tenc base filn = do
    lns <- lines `fmap` myReadFile tenc (base FP.</> filn)
    let theme : rest = lns
        grp = if null lns
              then LinksGrp { ltheme = "File is empty!", links = [] }
              else LinksGrp { ltheme = theme, links = map lineToLink rest }
    return grp

lineToLink :: String -> Link
lineToLink lin = Link { llink = a, ldesc = if null b then b else tail b }
    where (a, b) = break (== '|') lin

-- This will render the links page
renderLinks :: Links -> Lang -> RenderType -> [(PageIdent, String)]
renderLinks li@(Links myid ls) la rt = [(pid, str1)]
    where pid = PageIdent { ident = myid, rlang = la, rtype = rt, elmt = 0 }
          str1 = prettyHtml $ basicStruct la rt pid cont noHtml
          ls1 = maybe defls id $ lookup la ls
          defls = [LinksGrp { ltheme = "No links for " ++ show la, links = []}]
          cont = thediv `hid` "normtext"
                     << thediv `hid` "links" << renderLCont ls1

-- render links content part
renderLCont ls = foldl (+++) noHtml $ map renderLinksGrp ls

renderLinksGrp lg = foldl (+++) stheme $ map renderLink $ links lg
    where stheme = br +++ br +++ h4 << myStringToHtml (ltheme lg)

renderLink li = br +++ anchor hdesc ! [href (llink li)]
    where hdesc = myStringToHtml $ if null (ldesc li) then llink li else ldesc li
