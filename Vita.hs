module Vita where
-- import qualified System.FilePath as FP ((</>))
import System.FilePath ((</>))
import System.IO (TextEncoding)
import Text.XHtml hiding ((</>))
import Text.XHtml.Table (simpleTable)
import Base
import Common
import Helpers

-- We need following information:
-- 1. for the content of home: now we make vita as first screen
--    Vita was a text, so we use text elements to render it
-- 2. for the news section: per language one file (located in Text/<lang>).
--    The file has one news per line, with 4 columns separated by '|':
--    col 1: a date (or date range)
--    col 2: the text to render for that news
--    col 3: a basic link, i.e. either I <page identifier> or E <url> (optional)
--    col 4: an external address (optional)
--    The news text will be rendered as a link if one of the internal or
--    external link are present. The internal link has priority.
-- 3. A number of news to render. Only the first ones will be rendered. This
--    is good to keep older news in files, without to render them (space).

data VitaArgs = VitaArgs {
                    cfiles :: [String],
                    cimg   :: Maybe String,
                    nfile  :: String,
                    ncnt   :: Int
            }

data Sub = Sub {
               ptitle :: String,
               plines :: [String]
           }

data News = News {
                ndate :: String,
                ntext :: String,
                nlink :: Maybe BasicLink
            }

data VitaL = VitaL {
                 hcont :: [Sub],
                 news  :: [News]
             }

data Vita = Vita {
                vita :: [(Lang, VitaL)],
                vimg :: Maybe String
            }

instance Page Vita where
    getIdent = vitaIdent
    renderPage = renderVita

vitaIdent _ = "vita"

-- This will create a vita content structure given base directory and
-- the vita parameters
getVita :: TextEncoding -> String -> VitaArgs -> IO Vita
getVita tenc dir vargs = do
    prs <- sequence [ getVLang tenc la dir1 vargs | la <- [De, En, Ro]]
    return $ Vita { vita = prs, vimg = cimg vargs }
    where dir1 = dir </> "Text"

-- Get content for one language
getVLang tenc lang dir vargs = do
    let base = dir </> show lang
    pars <- mapM (fileToSub tenc base) (cfiles vargs)
    news <- parseNews tenc (base </> nfile vargs) (ncnt vargs)
    return (lang, VitaL { hcont = pars, news = news })

-- Make a subsection from a file
fileToSub tenc base name = do
    lns <- lines `fmap` myReadFile tenc (base </> name)
    let par = if null lns
                  then Sub { ptitle = "Empty File" ++ name, plines = [] }
                  else Sub { ptitle = head lns, plines = tail lns }
    return par

-- Make the news from one file
parseNews tenc path maxl = do
    lns <- (take maxl . lines) `fmap` myReadFile tenc path 
    return $ map lineToNews lns

-- Parse a line into the max 4 components
lineToNews line
    = foldl comb n0 $
            zip [setNDate, setNText, setNLink] as
    where as = mbreak 3 (== '|') line
          n0 = News { ndate = "", ntext = "", nlink = Nothing }
          comb n0 (f, v) = f n0 v
          setNDate n v = if null v then n else n { ndate = v }
          setNText n v = if null v then n else n { ntext = v }
          setNLink n v = if null v then n else n { nlink = parseLink v }

-- Render the vita page
renderVita :: Vita -> Lang -> RenderType -> [(PageIdent, String)]
renderVita vi la rt = [(pid, str1)]
    where myid = getIdent vi
          vital = maybe defl id $ lookup la $ vita vi
          str1 = prettyHtml $ basicStruct la rt pid cont cnews
          defl = VitaL { hcont = [errs], news = [] }
          errs = Sub { ptitle = "No content for " ++ show la, plines = [] }
          cont = thediv `hid` "normtext" <<
                    thediv `hid` "vita"
                        << (rimg +++ cont1 +++ choosejs +++ chooselang)
          rimg = maybe noHtml (\f -> image ! [src f]) $ vimg vi
          cont1 = renderHCont $ hcont vital
          choosejs = noHtml
          chooselang = noHtml
          cnews = renderNews pid $ news vital
          pid = PageIdent { ident = myid, rlang = la, rtype = rt, elmt = 0 }

-- render vita content
renderHCont :: [Sub] -> Html
renderHCont ss = thediv `hid` "vitatext"
                 << (foldl (+++) noHtml $ map renderSub ss)

renderSub :: Sub -> Html
renderSub s = addBr $ foldl (+++) (h4 << pt) pl
    where pt = addBr . myStringToHtml $ ptitle s
          pl = map (addBr . myStringToHtml) $ plines s
          addBr = (+++ br)

-- The news division
renderNews :: PageIdent -> [News] -> Html
{--
renderNews pid news = thediv `hid` "ralign" << thediv `hid` "valign" << 
    thediv `hid` "news" << (
        h3 (myStringToHtml "News")
    +++ simpleTable [identifier "newstab"] [] rows
    )
--}
renderNews pid news =
    thediv `hid` "news" << (
        h3 (myStringToHtml "News")
    +++ simpleTable [identifier "newstab"] [] rows
    )
    where rows = map (newsToRow pid) news

-- Make a row (i.e. list of cell contents) from a news
newsToRow :: PageIdent -> News -> [Html]
newsToRow pid n = [cdate, cdesc]
    where cdate = myStringToHtml $ ndate n
          htxt = myStringToHtml $ ntext n
          cdesc = case nlink n of
                    Nothing              -> htxt
                    Just (InternLink ip) -> ilink ip
                    Just (ExternLink el) -> anchor htxt ! [ href el ]
          ilink ip = let linki = htPath (rlang pid) pid { ident = ip }
                     in anchor htxt ! [ href linki ]
