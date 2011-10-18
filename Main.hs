module Main where
import Control.Applicative
import Data.Monoid (mempty)
import Data.Char (toUpper)
import System.Directory (getDirectoryContents, doesFileExist)
import System.FilePath (addExtension, takeExtensions, (</>))
import Base
import Catalog
import Home
import Links
import Gallery
import Texts

baseDir = "J:\\Pisi\\o1"
catDir = "KATALOG2009"

homeArgs = HomeArgs {
              cfiles = [ "Home-Warum.txt", "Home-Wie.txt" ],
              nfile  = "News.txt",
              ncnt   = 5
           }

linksArgs = LinksArgs {
                lfiles = [
                    "Links-Vereine.txt",
                    "Links-Galerien.txt",
                    "Links-Kuenstler.txt",
                    "Links-Diverse.txt"
                ]
            }

vitaArgs = TextsArgs {
                tfiles = [ "Vita.txt" ],
                tmode = Theme,
                timg = Just "/Bilder/mariana.jpg"
            }

contactArgs = TextsArgs {
                tfiles = [ "Kontakt.txt" ],
                tmode = Theme,
                timg = Nothing
            }

exposArgs = TextsArgs {
                tfiles = [
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

{--
galTextAssocs = [
        ("gal1", "galerie1", Just "Sargent.txt"),
        ("gal2", "galerie2", Just "Cameron.txt"),
        ("gal3", "galerie3", Just "Modersohn.txt"),
        ("gal4", "galerie4", Just "Kandinsky.txt"),
        ("gal5", "galerie5", Just "Michelangelo.txt"),
        ("gal6", "galerie6", Just "Wilde.txt"),
        ("gal7", "galerie7", Just "Klinger.txt"),
        ("gal8", "galerie8", Just "Picasso.txt"),
        ("gal9", "galerie9", Just "Chardin.txt"),
        ("gal10", "galerie10", Just "van Gogh.txt")
    ]
--}
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
    let pathToCats = baseDir </> catDir
    catFiles <- map (pathToCats </>) . filter isCat
                    <$> getDirectoryContents pathToCats
    catalog <- combCats <$> mapM parseCat catFiles
    home  <- getHome baseDir homeArgs
    links <- getLinks "links" baseDir linksArgs
    vita  <- getTexts "vita" baseDir vitaArgs
    danke <- getTexts "danke" baseDir dankeArgs
    kontakt <- getTexts "kontakt" baseDir contactArgs
    expos <- getTexts "expos" baseDir exposArgs
    gals  <- readTheGalleries catalog
    fotogals <- readFotoGals
    renderToFS baseDir home
    renderToFS baseDir links
    renderToFS baseDir vita
    renderToFS baseDir danke
    renderToFS baseDir kontakt
    renderToFS baseDir expos
    mapM_ (renderToFS baseDir) gals
    mapM_ (renderToFS baseDir) fotogals

readTheGalleries cat = mapM (readGallery cat) galTextAssocs

readGallery cat (g, d, t) = do
    let gArgs = GalleryArgs {
                    gaDir = d,
                    gaTextF = t,
                    gaCat = cat
                }
    getGallery g baseDir gArgs

readFotoGals = mapM (readGallery mempty) $ map triple fotoGals
    where triple g = (g, capi g, Nothing)
          capi [] = []
          capi (c:cs) = toUpper c : cs

isCat :: String -> Bool
isCat f = takeExtensions f == ".csv"
