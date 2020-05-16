module Translator (module Translator) where

import FormulaParser
import Parsing
import Pos


lex2code :: LexToken -> String

lex2code (LexToken str (Pos t l f n)) = 
    "<div style=\"position: absolute; top:"++show (round t)++"px; left:"++show (round l)++"px;\"><span style=\"font-size:"++show (round f)++"px; font-style:"++(if n then "normal" else "oblique")++"; lineheight:100%;\">"++str++"</span></div>"

lexes2code :: [LexToken] -> String 
lexes2code lexes = "<html>\n<head>\n<META content=\"text/html; charset=utf-8\">\n</head>\n<body>\n"
    ++ concatMap ((++"\n") . lex2code) lexes
    ++ "</body>\n</html>\n"

parseResult2code :: ParseResult -> String
parseResult2code (ParseResult lexes _) = lexes2code lexes

translate :: String -> String
translate str = case parse (s startPos) str of 
    [] -> ""
    [(pr, _)] -> parseResult2code pr

-- t1 = translate "$a^{2}$"
-- t2 = translate "$a_^{c2}{b}$"
-- t3 = translate "$\\sum{a^{2}}{b^{2}}{(c\\int{1}{2}{dt})}$"
-- t4 = translate "$"

-- do { putStrLn t1}