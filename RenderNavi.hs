module RenderNavi where
import Text.XHtml
import Helpers
import Base

type Ref  = String
type Desc = String
data Navi = Navi {
                naMenu :: [(Int, (Ref, Desc))],
                naLang :: [(Lang, Ref)]
            }

-- Make a list element for one navigation menu
-- The current visited page (as item number) is given i.o. to differentiate
-- between inactive (that one) and active (all others) links
makele :: Int -> Int -> Ref -> Desc -> Html
makele csel crt ref str
    | csel /= crt = lielun << anc << hstr
    | otherwise   = lielse << hstr
    where lielun = li `hclass` "menli" `hid` menliid
          lielse = li `hclass` "menlisel" `hid` menliid
          anc = anchor `hclass` "menua" ! aattrs
          hstr = myStringToHtml str
          menliid = "menli" ++ show crt
          aattrs  = [href ref]

makelist cs = map f
    where f (c, (r, s)) = makele cs c r s

makela :: Lang -> (Lang, Ref) -> Html
makela lang (lang1, ref)
    | lang /= lang1 = anc `hclass` "langli" << hstr
    | otherwise     = hstr `hclass` "langlisel"
    where anc = anchor ! [href ref]
          hstr = myStringToHtml $ case lang1 of
                     De -> "Deutsch"
                     En -> "English"
                     Ro -> "Romana"

makenavi :: Navi -> Int -> Lang -> Int -> Int -> Html
makenavi (Navi { naMenu = rs, naLang = ls }) csel lang split1 split2
    = thediv `hid` "menu" << (
          thediv `hid` "menu1"
          << ulist `hclass` "menuul" << menu m1
      +++ thediv `hid` "menu2"
          << ulist `hclass` "menuul" << menu m2
      +++ thediv `hid` "menu3"
          << ulist `hclass` "menuul" << menu m3
      )
  +++ thediv `hid` "langc" << langli
    where (m1, r1) = splitAt split1 rs
          (m2, m3) = splitAt split2 r1
          menu = foldl (+++) noHtml . makelist csel
          langli = simpleTable [identifier "langtab"] [theclass "langcell" ]
                        [map (makela lang) ls]
