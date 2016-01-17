module Navigation where
import qualified Data.Map as M
import Text.XHtml
import Base
import RenderNavi

-- Here is the definition of the main navigation menu with the idents of
-- the main pages and theyre descriptions in the render languages
links = zip [1..] [
            -- Item "home" [(De, "home"), (En, "home"), (Ro, "home")],
            Item "vita" [(De, "vita"), (En, "vita"), (Ro, "vita")],
            Item "expos" [(De, "ausstellungen"), (En, "exhibitions"), (Ro, "expozitii")],
            Item "links" [(De, "links"), (En, "links"), (Ro, "links")],
            -- Item "danke" [(De, "danke"), (En, "thanks"), (Ro, "multumiri")],
            Item "kontakt" [(De, "kontakt"), (En, "contact"), (Ro, "contact")],
            --
            Item "gal1" [(De, "galerie I"), (En, "gallery I"), (Ro, "galeria I")],
            Item "gal2" [(De, "galerie II"), (En, "gallery II"), (Ro, "galeria II")],
            Item "gal3" [(De, "galerie III"), (En, "gallery III"), (Ro, "galeria III")],
            Item "gal4" [(De, "galerie IV"), (En, "gallery IV"), (Ro, "galeria IV")],
            Item "gal5" [(De, "galerie V"), (En, "gallery V"), (Ro, "galeria V")],
            --
            Item "gal6" [(De, "galerie VI"), (En, "gallery VI"), (Ro, "galeria VI")],
            Item "gal7" [(De, "galerie VII"), (En, "gallery VII"), (Ro, "galeria VII")],
            Item "gal8" [(De, "galerie VIII"), (En, "gallery VIII"), (Ro, "galeria VIII")],
            Item "gal9" [(De, "galerie IX"), (En, "gallery IX"), (Ro, "galeria IX")],
            Item "gal10" [(De, "galerie X"), (En, "gallery X"), (Ro, "galeria X")]
    ]

-- And here are the implementation details:
data Item = Item String [(Lang, String)]

-- reverse of the links list: a translation from page ident to item number
identToItem :: M.Map String Int
identToItem = M.fromList $ map rev links
    where rev (a, Item b _) = (b, a)

-- This is the function called in all pages which show the navigation menu
-- parid is the parent ident of the calling page (to show its menu entry
-- as plain text, not as a link)
menuDiv :: Lang -> RenderType -> PageIdent -> Html
menuDiv la rt parid = navi pid la rt csel ls
    where pid = PageIdent {
                    ident = "menu",
                    rlang = la,
                    rtype = rt,
                    elmt = 0
                    }
          csel = maybe 0 id $ M.lookup (ident parid) identToItem
          ls = map f [De, En, Ro]
          f x = (x, htPath la $ parid { rlang = x })

-- navi pid la rt csel ls = makenavi nads csel la 6 5
navi pid la rt csel ls = makenavi nads csel la 4 5
    where nads = Navi { naMenu = rs, naLang = ls }
          rs = map f links
          f (i, Item p nms) =
            (i, (htPath la pid { ident = p }, maybe "" id $ lookup la nms))
