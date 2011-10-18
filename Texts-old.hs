module Texts where
import Data.List (intersperse)
import qualified System.FilePath as FP ((</>))
import Text.XHtml
-- import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec (
    Parser, between, string, many, choice, letter, digit, char, anyChar,
    parse, (<|>), eof
    )
-- import Text.ParserCombinators.Parsec.Prim (errorMessages)
import Text.ParserCombinators.Parsec.Error (errorMessages, messageString)
import Base
import Common
import Helpers

-- We need the following information:
-- a list of files, every of which containing (on the first line) a theme
--    and a number of further lines, which will be rendered each as a
--    paragraph, after the theme. The construction :<file>: is treated special
--    as beeing a reference to an image found in file file
-- The files are located in Text/<lang> and will be rendered exactly in the
-- order in which they appear in the list

data TextsArgs = TextsArgs { tfiles :: [String] }

data TextOrImg = Text String | Img String deriving Show

type Par = [TextOrImg]

data TextGrp = TextGrp { ttheme :: String, pars :: [Par] }

data Texts = Texts Ident [(Lang, [TextGrp])]

instance Page Texts where
    getIdent = textsIdent
    renderPage = renderTexts

textsIdent (Texts pid _) = pid

-- This will create a texts content structure given the page ident,
-- the home directory and the links parameters
getTexts :: Ident -> String -> TextsArgs -> IO Texts
getTexts pid dir teargs = do
    prs <- sequence [ getTLang la dir1 teargs | la <- [De, En, Ro]]
    return . Texts pid $ prs
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
              then TextGrp { ttheme = "File is empty!", pars = [] }
              else TextGrp { ttheme = theme, pars = map lineToPar rest }
    return grp

lineToPar :: String -> Par
lineToPar lin
    | Left pe  <- prez = map (Text . messageString) (errorMessages pe)
    | Right as <- prez = as
    where prez = parse pPar "" lin

pPar :: Parser Par
pPar = pPar0 []

pPar0 as = iseof as <|> pImpPar1 as <|> pImpPar0 as

iseof as = do
    eof
    return $ if null as then [] else [Text $ reverse as]

pImpPar0 as = do
    c <- anyChar
    pPar0 (c:as)

pImpPar1 as = do
    img <- pImgRef
    rs  <- pPar0 []
    if null as
        then return $ Img img : rs
        else return $ Text (reverse as) : Img img : rs

pImgRef :: Parser String
pImgRef = between (string ":<") (string ">:") pFileName

pFileName :: Parser String
pFileName = many $ choice [letter, digit, char '.', char '/']

-- This will render the texts page
renderTexts :: Texts -> Lang -> RenderType -> [(PageIdent, String)]
renderTexts li@(Texts myid ls) la rt = [(pid, str1)]
    where pid = PageIdent { ident = myid, rlang = la, rtype = rt, elmt = 0 }
          str1 = prettyHtml $ basicStruct la rt pid cont noHtml
          ls1 = maybe defls id $ lookup la ls
          defls = [TextGrp { ttheme = "No text for " ++ show la, pars = []}]
          cont = thediv `hid` "normtext"
                     << thediv `hid` myid << renderLCont ls1

-- render text content part
renderLCont ls = foldl1 (+++) $ intersperse brs $ map renderTextsGrp ls
    where brs = br +++ br

renderTextsGrp lg = foldl (+++) stheme $ map renderText $ pars lg
    where stheme = if null (ttheme lg)
                      then noHtml
                      else h4 << stringToHtml (ttheme lg)

renderText li = map renderOne li +++ br

renderOne (Text str) = stringToHtml str
renderOne (Img fil) = image ! [ src fil ]