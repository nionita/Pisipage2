module Common (basicStruct) where
import Text.XHtml
import Base
import Helpers
import Navigation

-- This function renders a page in a standard way, given the language, the
-- render type, the page id, the content and the content navi
-- the last two parameters are html fragments and should be div elements
basicStruct :: Lang -> RenderType -> PageIdent -> Html -> Html -> Html
basicStruct la rt pid cont cnavi
    = myhead "../Styles/default.css" "../Styles/style-" "../Scripts/basic.js"
  +++ body << (
          thediv `hid` "container" << (
                  mylogo "../Bilder/Style/front.gif"
              +++ navidiv la rt pid cnavi
              +++ cont
          )
      )

navidiv la rt pid cnavi
    = thediv `hid` "navi" << (
          menuDiv la rt pid
      +++ cnavi
      +++ copyr
      )

copyr = thediv `hid` "copy" << (
            copyright
        +++ myStringToHtml "Mariana Ionita 2007-2017"
        )

mylogo gif = thediv `hid` "logo"
             << image `hid` "logop" `hsrc` gif

myhead def_css css_pref js_file
    = header << (
          thetitle (myStringToHtml "Mariana Ionita")
      +++ meta ! [ strAttr "HTTP_EQUIV" "Content-type",
                  strAttr "CONTENT" "text/html; charset=ISO-8859-1" ]
                  -- strAttr "CONTENT" "text/html; charset=UTF-8" ]
      +++ meta ! [ strAttr "NAME" "description",
                  strAttr "LANGUAGE" "de",
                  strAttr "CONTENT" "Die Internetseite von Mariana Ionita" ]
      +++ meta ! [ strAttr "NAME" "keywords",
                  strAttr "LANGUAGE" "de",
                  strAttr "CONTENT" "Malerei, Portraet, Zeichnen" ]
      +++ thelink noHtml ! [ href def_css,
                     thetype "text/css",
                     rel "stylesheet" ]
      +++ (script noHtml ! [ lang "javascript" ]) `hsrc` js_file
      +++ script ! [ lang "javascript" ]
         << myStringToHtml ("generateCSSRef('" ++ css_pref ++ "')")
      )
