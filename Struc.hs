module Struc (page_gal, page_txt)
where
import Prelude hiding (div, head, map, span)
import qualified Prelude as P (head, map)
import HTMLPrelude
import HTMLTypedBase (rawtext)

myhead :: String -> String -> String -> ELT HTML -> ELT HTML
myhead def_css css_pref js_file
       = head (title (text "Mariana Ionita")
            ## meta (attr HTTP_EQUIV "Content-type"
                  ## attr CONTENT "text/html; charset=ISO-8859-1"
                  )
            ## meta (attr NAME "description"
                  ## attr LANG "de"
                  ## attr CONTENT "Die Internetseite von Mariana Ionita"
                  )
            ## meta (attr NAME "keywords"
                  ## attr LANG "de"
                  ## attr CONTENT "Malerei, Portraet, Zeichnen"
                  )
            ## link (attr HREF def_css
                  ## attr TYPE "text/css"
                  ## attr REL "stylesheet"
                    )
            ## script (attr LANGUAGE "javascript"
                    ## attr SRC js_file
                    )
            ## script (attr LANGUAGE "javascript"
                    ## text ("generateCSSRef('" ++ css_pref ++ "')")
                    )
            )

mylogo :: AddTo a DIV => String -> ELT a -> ELT a
mylogo src = div (attr ID "logo"
               ## img (attr ID "logop"
                    ## attr SRC src
                    )
               )

mkmenugrp :: AddTo a DIV => String -> String -> [(String, String, String)] -> ELT a -> ELT a
mkmenugrp mid gid lis = div (attr ID gid
                   ## ul (
                         foldl (##) (attr CLASS "menuul") (P.map (makeli mid) (zip ['1'..] lis))
                         )
                   )
    where makeli cpid (crt, (pid, ref, bld)) = if cpid == pid then
                li (attr CLASS "menlisel"
                 ## attr ID (mid ++ "li" ++ [crt])
                 ## img (attr CLASS "menupic"
                      ## attr SRC bld
                      )
                 )
              else
                li (attr CLASS "menli"
                 ## attr ID (mid ++ "li" ++ [crt])
                 ## a (attr CLASS "menua"
                    ## attr HREF ref
                    ## img (attr CLASS "menupic"
                         ## attr SRC bld
                         )
                    )
                 )

thumbs :: AddTo a DIV => Bool -> String -> String -> String -> [String] -> ELT a -> ELT a
thumbs tst tdir bdir bid fils =
    div (attr ID "thumbs"
      ## foldl1 (##) (P.map (maketh tst tdir bdir bid) fils)
      )
    where maketh tst tdir bdir bid fil =
              let nme = fil ++ ".jpg"
                  attr_title = if tst then attr TITLE fil else id
              in a (attr CLASS "thumb"
                 ## img (attr CLASS "thumb"
                      ## attr_title
                      ## attr SRC (tdir ++ "/" ++ nme)
                      ## attr ONCLICK ("javascript:replacePic('" ++ bid ++ "','" ++ bdir ++ "/" ++ nme ++ "')")
                      )
                  )

mknews :: AddTo a DIV => String -> ELT a -> ELT a
mknews ntxt = div (attr ID "ralign"
                ## rawtext ntxt
                )

bigpic :: AddTo a DIV => String -> String -> String -> String -> (ELT a -> ELT a)
bigpic did iid src txt =
    div (attr ID did
      ## img (attr ALT "Bild"
           ## attr ID iid
           ## attr SRC src
           )
      ## div (attr ID "text"
           ## text txt
           )
      ## div (attr ID "textshad"
           ## text txt
           )
      )

--mpoint lang s = (s, "../" ++ lang ++ "/" ++ s ++ ".html", "../Bilder/Style/" ++ s ++ ".gif")
mpoint lang s = (s, s ++ ".html", "../Bilder/Style/" ++ s ++ ".gif")

menugrp1 pagid lang = mkmenugrp pagid "menu1" (P.map (mpoint lang) ["home", "vita", "links", "kontakt"])
menugrp2 pagid lang = mkmenugrp pagid "menu2" (P.map (mpoint lang) ["gal"++[c] | c <- ['1'..'5']])
menugrp3 pagid lang = mkmenugrp pagid "menu3" (P.map (mpoint lang) ["gal"++[c] | c <- ['6'..'9']++['a']])

basic_struct :: String -> String -> Bool -> (ELT DIV -> ELT DIV) -> (ELT DIV -> ELT DIV) -> String
basic_struct pagid lang tst cont cont_navi = show_html $
    build_document (myhead "../Styles/default.css" "../Styles/style-" "../Scripts/basic.js"
                 ## body (div (attr ID "container"
                            ## mylogo "../Bilder/Style/front.gif"
                            ## div (attr ID "navi"
                                 ## div (attr ID "menu"
                                      ## menugrp1 pagid lang
                                      ## menugrp2 pagid lang
                                      ## menugrp3 pagid lang
                                      )
                                 ## cont_navi
                                 ## div (attr ID "copy"
                                      ## rawtext "&copy;Mariana Ionita 2007-2009"
                                      )
                                 )
                            ## cont
                            )
                 )
               )

page_gal :: String -> String -> Bool -> String -> String -> [String] -> String -> String
page_gal pagid lang tst tdir bdir fils txt = basic_struct pagid lang tst bpic th
    where bpic = bigpic "bigpic" "bild" ("../" ++ bdir ++ "/" ++ P.head fils ++ ".jpg") txt
          th = thumbs tst ("../" ++ tdir) ("../" ++ bdir) "bild" fils

page_txt :: String -> String -> Bool -> String -> String -> String
page_txt pagid lang tst htxt ntxt = basic_struct pagid lang tst ctext news
    where ctext = rawtext htxt
          news = mknews ntxt