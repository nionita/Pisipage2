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
                  cdims  :: String,
                  cprice :: String
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
makeBTitle ce = foldl (+++) (makeTTitle ce) [cyear ce, ctech ce, cmdims, formprice]
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
          formprice = if null (cprice ce)
                         then ""
                         else cprice ce
{--
                         else if cprice ce `endswith` "€"
                                 then cprice ce
                                 else cprice ce ++ " €"
--}

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
        ("year", ["JAHR", "Jahr"]),
        ("price", ["Preis", "Price"])
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
          k = getField "code"  False hcols cols
          t = getField "title" False hcols cols
          y = getField "year"  False hcols cols
          h = getField "tech"  False hcols cols
          d = getField "dims"  False hcols cols
          p = getField "price" True  hcols cols
          ce = CEntry { ctitle = t, cyear = y, ctech = h, cdims = d, cprice = p } 

getField :: String -> Bool -> [String] -> [String] -> String
getField code opt cs vs
    | null cols = if opt then "" else error erm
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
