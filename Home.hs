module Home where
import qualified System.FilePath as FP ((</>))
import System.IO (TextEncoding)
import Text.XHtml
import Text.XHtml.Table (simpleTable)
import Base
import Common
import Helpers

-- We need following information:
-- 1. for the content of home: per language and subsection (for example "Why?"
--    or "How?") one text file (located in Text/<lang>). The files will be
--    rendered exactly in the order in which they appear in the list. Every
--    line in one file will be rendered as an paragraph, except the first line,
--    which will be rendered as the subsection title
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

data HomeArgs = HomeArgs {
                    cfiles :: [String],
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

data HomeL = HomeL {
                 hcont :: [Sub],
                 news  :: [News]
             }

newtype Home = Home [(Lang, HomeL)]

instance Page Home where
    getIdent = homeIdent
    renderPage = renderHome

homeIdent _ = "home"

-- This will create a home content structure given base directory and
-- the home parameters
getHome :: TextEncoding -> String -> HomeArgs -> IO Home
getHome tenc dir hargs = do
    prs <- sequence [ getHLang tenc la dir1 hargs | la <- [De, En, Ro]]
    return . Home $ prs
    where dir1 = dir FP.</> "Text"

-- Get content for one language
getHLang tenc lang dir hargs = do
    let base = dir FP.</> show lang
    pars <- mapM (fileToSub tenc base) (cfiles hargs)
    news <- parseNews tenc (base FP.</> nfile hargs) (ncnt hargs)
    return (lang, HomeL { hcont = pars, news = news })

-- Make a subsection from a file
fileToSub tenc base name = do
    lns <- lines `fmap` myReadFile tenc (base FP.</> name)
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

-- Render the home page
renderHome :: Home -> Lang -> RenderType -> [(PageIdent, String)]
renderHome ho@(Home h) la rt = [(pid, str1)]
    where myid = getIdent ho
          homel = maybe defl id $ lookup la h
          str1 = prettyHtml $ basicStruct la rt pid cont cnews
          defl = HomeL { hcont = [errs], news = [] }
          errs = Sub { ptitle = "No content for " ++ show la, plines = [] }
          cont = thediv `hid` "normtext" <<
                    thediv `hid` "home"
                        << (cont1 +++ choosejs +++ chooselang)
          cont1 = renderHCont $ hcont homel
          choosejs = noHtml
          chooselang = noHtml
          cnews = renderNews pid $ news homel
          pid = PageIdent { ident = myid, rlang = la, rtype = rt, elmt = 0 }

-- render home content
renderHCont :: [Sub] -> Html
renderHCont ss = thediv `hid` "hometext"
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
