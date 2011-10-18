module ParseHtml where

import Control.Monad
import qualified Data.Map as M
import Text.ParserCombinators.Parsec hiding (label)
import Text.XHtml.Strict

type Func b = (forall a . a) -> b

xhtmlTags :: [(String, Func Html)]
xhtmlTags = [
    ("abbr",        \x -> abbr x),
    ("acronym",     \x -> acronym x),
    ("address",     \x -> address x),
    ("anchor",      \x -> anchor x),
    ("area",        \_ -> area),
    ("bdo",         \x -> bdo x),
    ("big",         \x -> big x),
    ("blockquote",  \x -> blockquote x),
    ("body",        \x -> body x),
    ("bold",        \x -> bold x),
    ("br",          \_ -> br),
    ("button",      \x -> button x),
    ("caption",     \x -> caption x),
    ("cite",        \x -> cite x),
    ("col",         \x -> col x),
    ("colgroup",    \x -> colgroup x),
    ("del",         \x -> del x),
    ("ddef",        \x -> ddef x),
    ("define",      \x -> define x),
    ("dlist",       \x -> dlist x),
    ("dterm",       \x -> dterm x),
    ("emphasize",   \x -> emphasize x),
    ("fieldset",    \x -> fieldset x),
    ("form",        \x -> form x),
    ("h1",          \x -> h1 x),
    ("h2",          \x -> h2 x),
    ("h3",          \x -> h3 x),
    ("h4",          \x -> h4 x),
    ("h5",          \x -> h5 x),
    ("h6",          \x -> h6 x),
    ("header",      \x -> header x),
    ("hr",          \_ -> hr),
    ("image",       \_ -> image),
    ("input",       \_ -> input),
    ("ins",         \x -> ins x),
    ("italics",     \x -> italics x),
    ("keyboard",    \x -> keyboard x),
    ("label",       \x -> label x),
    ("legend",      \x -> legend x),
    ("li",          \x -> li x),
    ("meta",        \_ -> meta),
    ("noscript",    \x -> noscript x),
    ("object",      \x -> object x),
    ("olist",       \x -> olist x),
    ("optgroup",    \x -> optgroup x),
    --("option",      \x -> option x),
    ("paragraph",   \x -> paragraph x),
    ("param",       \_ -> param),
    ("pre",         \x -> pre x),
    ("quote",       \x -> quote x),
    ("sample",      \x -> sample x),
    ("script",      \x -> script x),
    ("select",      \x -> select x),
    ("small",       \x -> small x),
    ("strong",      \x -> strong x),
    ("style",       \x -> style x),
    ("sub",         \x -> sub x),
    ("sup",         \x -> sup x),
    ("table",       \x -> table x),
    ("tbody",       \x -> tbody x),
    ("td",          \x -> td x),
    ("textarea",    \x -> textarea x),
    ("tfoot",       \x -> tfoot x),
    ("th",          \x -> th x),
    ("thead",       \x -> thead x),
    ("base",        \_ -> thebase),
    ("code",        \x -> thecode x),
    ("div",         \x -> thediv x),
    ("html",        \x -> thehtml x),
    ("link",        \x -> thelink x),
    ("map",         \x -> themap x),
    ("span",        \x -> thespan x),
    ("title",       \x -> thetitle x),
    ("tr",          \x -> tr x),
    ("tt",          \x -> tt x),
    ("ulist",       \x -> ulist x),
    ("variable",    \x -> variable x)
    ]

tagsMap = M.fromList xhtmlTags

resWord :: Eq a => M.Map String a -> Parser String
resWord m = do t <- many1 letter
               when (M.lookup t m == Nothing) (fail ("unknown tag: " ++ t))
               return t

--parseRes :: M.Map String (a -> b) -> String -> Parser (a -> b)
parseRes m w = do t <- many1 letter
                  case M.lookup t m of
                      Nothing -> fail ("unknown " ++ w ++ ": " ++ t)
                      Just f  -> return f

parseTag :: Parser (Func Html)
parseTag = parseRes tagsMap "tag"

-- Help function: parse a string up to one of the given chars
upTo :: [Char] -> Parser [Char]
upTo ds = many1 (noneOf ds)
{--
upTo ds = do c <- noneOf ds
             cs <- upTo ds
             return (c:cs)
--}

parseHtml :: Parser Html
parseHtml = do many space
               choice [parseElem, parseText]
            <?> "html"

parseElem :: Parser Html
parseElem = do en <- parseElTag
               many1 space
               (ats, cnt) <- restElem en
               return $ compElem en cnt ! ats
            <?> "html element"

-- Compose a html element from tag name and content
compElem en cnt = if isNoHtml cnt then itag en else tag en cnt

parseElTag :: Parser String
parseElTag = do char '<'
                en <- elemName
                return en
             <?> "element tag"

elemName :: Parser String
elemName = many1 lower <?> "element name"

restElem :: String -> Parser ([HtmlAttr], Html)
restElem nm = do ats <- parseAttList
                 ht <- (restElNoCont <|> restElCont nm)
                 return (ats, ht)
              <?> ("> or /> to close the tag " ++ nm)

restElNoCont = do char '/'
                  char '>'
                  return noHtml
               <?> "/>"

restElCont nm = do char '>'
                   many space
                   els <- parseProperCont nm
                   return $ concatHtml els
                <?> "proper element content"

parseProperCont :: String -> Parser [Html]
parseProperCont nm = try (do closing nm
                             return []
                          )
                     <|> (do h <- parseHtml
                             hs <- parseProperCont nm
                             return (h:hs)
                          )
                     -- <|> return [stringToHtml ("Fehler hier: closing " ++ nm)]
                     -- <|> return []
                     <?> "html element content"

closing nm = do char '<'
                char '/'
                nm1 <- elemName
                char '>'
{--
                if nm1 == nm
                   then return ()
                   else fail $ nm ++ ", encountered " ++ nm1
             <?> ("closing of " ++ nm)
--}

-- Parse a html attribute
parseAttr :: Parser HtmlAttr
parseAttr = do at <- many1 lower
               char '='
               va <- parseQuote
               many space
               return $ strAttr at va
            <?> "Attribut"
parseAttList = many1 parseAttr <|> return [] <?> "attribute list"

-- Parse a quoted string
parseQuote :: Parser String
parseQuote = do char '"'
                cs <- upTo ['"']
                char '"'
                return cs

-- Parse a text element
parseText :: Parser Html
parseText = do s <- upTo "<"
               return (stringToHtml s)
            <?> "some text"

--parseHtml = do parseElem
-- For tests:
myp p inp = parse p "" inp
pf p file = parseFromFile p file