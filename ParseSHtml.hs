module ParseSHtml where

import Text.ParserCombinators.Parsec hiding (label)
import Text.XHtml.Strict

-- Helper function: parse a string up to one of the given chars
upTo :: [Char] -> Parser [Char]
upTo ds = many1 (noneOf ds)

parseHtmlFrg :: Parser Html
parseHtmlFrg = do many space
                  choice [parseElem, parseText]
               <?> "html fragment"

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

-- Rest element with no content
restElNoCont = do char '/'
                  char '>'
                  return noHtml
               <?> "/>"

-- Rest element with content
restElCont nm = do char '>'
                   many space
                   els <- parseProperCont nm
                   return $ concatHtml els
                <?> "element with content"

-- Parse closing tag or proper content(s)
parseProperCont :: String -> Parser [Html]
parseProperCont nm = try (do closing nm
                             return []
                          )
                     <|> (do h <- parseHtmlFrg
                             hs <- parseProperCont nm
                             return (h:hs)
                          )
                     -- <|> return []
                     <?> "proper element content"

closing nm = do char '<'
                char '/'
                nm1 <- elemName
                char '>'
                if nm1 == nm
                   then return ()
                   else fail $ nm ++ ", encountered " ++ nm1
             <?> ("closing of " ++ nm)

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