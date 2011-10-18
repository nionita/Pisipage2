module Base where
import System.FilePath (addExtension, (</>))
import System.IO
import Data.Ix

-- Languages
data Lang = De | En | Ro
    deriving (Eq, Ord, Ix)

instance Show Lang where
    show De = "de"
    show En = "en"
    show Ro = "ro"

-- Render types: no JavaScript / with JavaScript
data RenderType = NoJS | JS
    deriving (Eq, Show)

-- A string that identifies the page
type Ident = String

-- Page identification considering language, render type and selected element
data PageIdent = PageIdent {
                     ident :: Ident,
                     rlang :: Lang,
                     rtype :: RenderType,
                     elmt  :: Int
                 } deriving Show

-- Internal page class
class Page a where
    getIdent   :: a -> Ident
    renderPage :: a -> Lang -> RenderType -> [(PageIdent, String)]

-- A basic link can be intern (page identifier) or extern (url)
data BasicLink = InternLink String | ExternLink String

renderToFS :: Page a => String -> a -> IO ()
renderToFS root a = do
    putStr $ "Rendering " ++ getIdent a ++ "... "
    hFlush stdout
    let fls = concat [renderPage a l t | l <- [De, En, Ro], t <- [NoJS, JS]]
    -- let fls = concat [renderPage a l t | l <- [De, En, Ro], t <- [NoJS]]
    mapM_ (renderPart root) fls
    putStrLn " done."

renderPart root (pide, cont) = writeFile (fsPath root pide) cont

-- Finds the file name (no path!) for an ident
identToName :: PageIdent -> String
identToName (PageIdent { ident = i, rtype = r, elmt = e })
    = name `addExtension` "html"
    where prfx = show r ++ "_" ++ i
          name = case r of
                    NoJS -> prfx ++ elpa
                    JS   -> prfx
          elpa = if e == 0 then "" else "_" ++ show e

-- Path for IO on file system
fsPath :: String -> PageIdent -> String
fsPath root pide = root </> langDir </> identToName pide 
    where langDir = show $ rlang pide

-- Path for reference in Html
htPath :: Lang -> PageIdent -> String
htPath olang pide
    | rlang pide == olang = name
    | otherwise = "../" ++ show (rlang pide) ++ "/" ++ name
    where name = identToName pide

parseLink :: String -> Maybe BasicLink
parseLink ('I':' ':rs) = Just $ InternLink rs
parseLink ('E':' ':rs) = Just $ ExternLink rs
parseLink _ = Nothing