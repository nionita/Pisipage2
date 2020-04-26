module Main where
import Control.Applicative
import Data.Char (toUpper)
import Data.Monoid (mempty)
import Data.List (intersperse)
import System.Directory (getDirectoryContents, doesFileExist)
import System.Environment (getArgs)
import System.FilePath (addExtension, takeExtensions, (</>))
import System.IO (mkTextEncoding)
import Base
import Catalog
import Vita
import Links
import Gallery
import Texts

-- baseDir = "J:\\Pisi\\o1"
catDir = "KATALOG2009"

homeArgs = VitaArgs {
              cfiles = [ "Vita.txt" ],
              cimg   = Just "/Bilder/mariana.jpg",
              nfile  = "News.txt",
              ncnt   = 3
           }

linksArgs = LinksArgs {
                lfiles = [
                    "Links-Weitere.txt",
                    "Links-Vereine.txt",
                    "Links-Galerien.txt",
                    "Links-Zeitung.txt"
                ]
            }

contactArgs = TextsArgs {
                tfiles = [ "Kontakt.txt" ],
                tmode = Theme,
                timg = Nothing
            }

exposArgs = TextsArgs {
                tfiles = [
                    "Expo2019.txt",
                    "Expo2018.txt",
                    "Expo2017.txt",
                    "Expo2016.txt",
                    "Expo2015.txt",
                    "Expo2014.txt",
                    "Expo2013.txt",
                    "Expo2012.txt",
                    "Expo2011.txt",
                    "Expo2010.txt",
                    "Expo2009.txt",
                    "Expo2008.txt",
                    "Expo2007.txt",
                    "Expo2006.txt",
                    "Expo2005.txt",
                    "Expo1997.txt",
                    "Expo1989.txt",
                    "Expo1988.txt",
                    "Expo1987.txt",
                    "Expo1986.txt",
                    "Expo1983.txt",
                    "Expo1982.txt"
                    ],
                tmode = Exhib,
                timg = Nothing
            }

dankeArgs = TextsArgs {
                tfiles = [ "Dank.txt" ],
                tmode = Theme,
                timg = Nothing
            }

galTextAssocs = [
        ("gal1", "galerie1", Nothing),
        ("gal2", "galerie2", Nothing),
        ("gal3", "galerie3", Nothing),
        ("gal4", "galerie4", Nothing),
        ("gal5", "galerie5", Nothing),
        ("gal6", "galerie6", Nothing),
        ("gal7", "galerie7", Nothing),
        ("gal8", "galerie8", Nothing),
        ("gal9", "galerie9", Nothing),
        ("gal10", "galerie10", Nothing)
    ]

fotoGals = [
        "foto20081002",
        "foto20090703"
    ]

main = do
    args <- getArgs
    case args of
        dir : [] -> generatePage dir
        _        -> argsError args

generatePage :: String -> IO ()
generatePage baseDir = do
    putStrLn $ "Generate page in " ++ baseDir
    let pathToCats = baseDir </> catDir
    encoding <- mkTextEncoding "CP1252"
    catFiles <- map (pathToCats </>) . filter isCat
                    <$> getDirectoryContents pathToCats
    catalog <- combCats <$> mapM (parseCat encoding) catFiles
    -- showCat catalog
    vita    <- getVita  encoding           baseDir homeArgs
    links   <- getLinks encoding "links"   baseDir linksArgs
    -- vita    <- getTexts encoding "vita"    baseDir vitaArgs
    -- danke   <- getTexts encoding "danke"   baseDir dankeArgs
    kontakt <- getTexts encoding "kontakt" baseDir contactArgs
    expos   <- getTexts encoding "expos"   baseDir exposArgs
    gals    <- readTheGalleries encoding baseDir catalog
    fotogals <- readFotoGals encoding baseDir
    renderToFS encoding baseDir vita
    -- renderToFS encoding baseDir home
    renderToFS encoding baseDir links
    -- renderToFS encoding baseDir danke
    renderToFS encoding baseDir kontakt
    renderToFS encoding baseDir expos
    mapM_ (renderToFS encoding baseDir) gals
    mapM_ (renderToFS encoding baseDir) fotogals

readTheGalleries encoding baseDir cat = mapM (readGallery encoding baseDir cat) galTextAssocs

readGallery encoding baseDir cat (g, d, t) = do
    let gArgs = GalleryArgs {
                    gaDir = d,
                    gaTextF = t,
                    gaCat = cat
                }
    getGallery encoding g baseDir gArgs

readFotoGals encoding baseDir = mapM (readGallery encoding baseDir mempty) $ map triple fotoGals
    where triple g = (g, capi g, Nothing)
          capi [] = []
          capi (c:cs) = toUpper c : cs

isCat :: String -> Bool
isCat f = takeExtensions f == ".csv"

showCat cat = mapM_ putStrLn $ map show $ filter ((/= "") . cprice) $ catElems cat

argsError :: [String] -> IO ()
argsError args = do
    let argstr = concat $ intersperse " " args
    putStrLn $ "Falsche Aufruf Parameter:" ++ argstr
    putStrLn "Aufruf: Pisipage2 <base-directory>"
