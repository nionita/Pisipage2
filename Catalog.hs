module Catalog where
import Control.Monad (when)
import qualified Data.Map as M
import Data.Char
import Data.Monoid
import Data.Maybe (fromJust)
import Text.ParserCombinators.Parsec

-- A catalog entry
data CEntry = CEntry {
                  ctitle :: String,
                  cyear  :: String,
                  ctech  :: String,
                  cdims  :: String
              }

newtype Catalog = Catalog (M.Map String CEntry)

instance Monoid Catalog where
    mempty = Catalog M.empty
    mappend (Catalog a) (Catalog b) = Catalog (a `M.union` b)

combCats :: [Catalog] -> Catalog
combCats = mconcat

findCatElem :: String -> Catalog -> Maybe CEntry
findCatElem s (Catalog m) = M.lookup s m

makeBTitle :: CEntry -> String
makeBTitle ce = foldl (+++) (makeTTitle ce) [cyear ce, ctech ce, cmdims]
    where (+++) "" b = b
          (+++) a "" = a
          (+++) a b = a ++ ", " ++ b
          a `endswith` b = (reverse a) `beginswith` (reverse b)
          a `beginswith` b = all id $ zipWith (==) a b
          cmdims = if null (cdims ce)
                     then ""
                     else if cdims ce `endswith` "cm"
                            then cdims ce
                            else cdims ce ++ " cm"

makeTTitle :: CEntry -> String
makeTTitle ce = nicetitle [] $ ctitle ce
    where nicetitle as (a:b:rs)
              | dupspace a b = nicetitle (' ':as) rs
              | separate a b = nicetitle (' ':a:as) (b:rs)
              | otherwise    = nicetitle (a:as) (b:rs)
          nicetitle as bs = reverse (bs ++ as)
          dupspace a b = isSpace a && isSpace b
          separate a b = isAlpha a && isDigit b
                      || isDigit a && isAlpha b
                      || isLower a && isUpper b

namesToCols = M.fromList $ [
        ("code", ["COD", "Cod"]),
        ("title", ["TITEL", "Titel"]),
        ("dims", ["GROSSE", "Dimension"]),
        ("tech", ["TECHNIK", "Technik"]),
        ("year", ["JAHR", "Jahr"])
    ]

parseCat :: String -> IO Catalog
parseCat file = do
    putStrLn $ "Reading catalog " ++ file
    lns <- lines `fmap` readFile file
    when (null lns) $ fail $ "No line in catalog " ++ file
    let parsEn = makeParseFunc $ head lns
    return $ Catalog $ M.fromList $ map parsEn $ tail lns

makeParseFunc :: String -> String -> (String, CEntry)
makeParseFunc hea inp = (k, ce)
    where Right hcols = parse line "Header" hea
          Right cols  = parse line "Line"   inp
          k = getField "code"  hcols cols
          t = getField "title" hcols cols
          y = getField "year"  hcols cols
          h = getField "tech"  hcols cols
          d = getField "dims"  hcols cols
          ce = CEntry { ctitle = t, cyear = y, ctech = h, cdims = d } 

getField :: String -> [String] -> [String] -> String
getField code cs vs
    | null cols = error erm
    | otherwise = vs !! i
    where Just aliases = M.lookup code namesToCols
          cols = dropWhile (== Nothing)
                  $ map (flip lookup (zip cs [0..])) aliases
          i = fromJust $ head cols
          erm = "Field code " ++ code ++ " not found\n"
                ++ "Codes: " ++ show cs
                ++ "\nAliases: " ++ show aliases

line = field `sepBy` (char ',')

field = stringF <|> numberF

stringF = do
    char '"'
    val <- many (noneOf ['"'])
    char '"'
    return val

numberF = many digit