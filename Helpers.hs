module Helpers where
import Data.Char
import Text.XHtml

-- Often used attributes for html elements
hclass e c = e ! [theclass c]
hid    e i = e ! [identifier i]
hsrc   e s = e ! [src s]

-- Break a list in a maximal number of sublists separated by a separator
-- expressed as a predicate
mbreak :: Int -> (a -> Bool) -> [a] -> [[a]]
mbreak 0 _ as = [as]
mbreak n p as
    | null as2  = [as1]
    | otherwise = as1 : mbreak (n-1) p (tail as2)
    where (as1, as2) = break p as

myStringToHtml = stringToHtml

{--
-- Irgendwie passt der Unicode von Haskell nicht mit dem von Win 7 (aber alle Viewer,
-- wie Firefox, Vim, Safari, etc geben Win 7 Recht!)
myStringToHtml = stringToHtml . map umlaute
    where umlaute x = case ord x of
               245  -> chr 228;	-- ä
               247  -> chr 246;	-- ö
               179  -> chr 252;	-- ü
               9472 -> chr 196;	-- Ä
               205  -> chr 214;	-- Ö
               9604 -> chr 220;	-- Ü
               9600 -> chr 223;	-- ß
               _    -> x
--}
