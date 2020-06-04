-- FormulaParser.hs
-- 按照给定的语法规则，基于Parsing.hs进行parse，并设计相应的制导规则，融合在parse过程中

module FormulaParser (module FormulaParser) where

import Parsing
import Pos

-- Grammar rules:
--   S -> $B$
--   B -> B1B | B1
--   B1 -> B2Bm | B2
--     in which Bm -> B'Bm | B'
--              B' -> _^{B} | ^{B} | _{B}
--   B2 -> \int{B}{B}{B} | \sum{B}{B}{B} | id | num | \blank | (B)
-- Note: tolerant all spaces

-- LexToken

data LexToken = LexToken {
    str :: String,
    pos :: Pos
} 

instance Show LexToken where
    -- show :: Show a => a -> String
    show (LexToken str pos) = "\n\tLexToken: \n\t\tString: "++str++"\n\t\t"++(show pos)

data ParseResult = ParseResult {
    lexes :: [LexToken],
    nextPos :: Pos
} 

instance Show ParseResult where
    -- show :: Show a => a -> String
    show (ParseResult lexes nextPos) = let lexStr = "["++(concatMap show lexes)++"\t]\n"
        in "ParseResult: "++lexStr++"\tnextPos: \n\t\t"++(show nextPos)


-- Parsers

-- s: S -> $B$

s :: Pos -> Parser ParseResult
s pos = do {
    symbol "$";
    pr <- b pos;
    symbol "$";
    return pr
}

-- b: B -> B1B | B1

b :: Pos -> Parser ParseResult
b pos = b_1 pos <|> b_2 pos

-- b_1: B -> B1B

b_1 :: Pos -> Parser ParseResult
b_1 pos = do { 
    ParseResult lexes1 nextPos1 <- b1 pos; 
    ParseResult lexes nextPos <- b nextPos1;
    return $ ParseResult (lexes1++lexes) nextPos
}

-- b_2: B -> B1

b_2 :: Pos -> Parser ParseResult
b_2 pos = do { 
    pr1 <- (b1 pos); 
    return pr1
}

-- b1: B1 -> B2Bm | B2

b1 :: Pos -> Parser ParseResult
b1 pos =  b1_1 pos <|> b1_2 pos

-- b1_1: B1 -> B2Bm

b1_1 :: Pos -> Parser ParseResult
b1_1 pos = do {
    ParseResult lexes2 nextPos2 <- b2 pos;
    ParseResult lexesm nextPosm <- bm nextPos2;
    return $ ParseResult (lexes2++lexesm) nextPosm
}

-- b1_2: B1 -> B2

b1_2 :: Pos -> Parser ParseResult
b1_2 pos = do {
    pr2 <- b2 pos;
    return pr2
}

-- bm: Bm -> B'Bm | B'

bm :: Pos -> Parser ParseResult
bm pos = bm_1 pos <|> bm_2 pos

-- bm_1: Bm -> B'Bm

bm_1 :: Pos -> Parser ParseResult
bm_1 pos = do {
    ParseResult lexes' nextPos' <- b' pos;
    ParseResult lexesm nextPosm <- bm nextPos';
    return $ ParseResult (lexes'++lexesm) nextPosm
}

-- bm_2: Bm -> B'

bm_2 :: Pos -> Parser ParseResult
bm_2 pos = do {
    pr' <- b' pos;
    return pr'
}

-- b': B' -> _^{B} | ^{B} | _{B}

b' :: Pos -> Parser ParseResult
b' pos = b'_1 pos <|> b'_2 pos <|> b'_3 pos

-- b'_1: B' -> _^{B}

b'_1 :: Pos -> Parser ParseResult
b'_1 pos = do {
    symbol "_^";
    symbol "{";
    ParseResult lexes1 nextPos1 <- b (toScript subScriptTopShift pos);
    symbol "}";
    symbol "{";
    ParseResult lexes2 nextPos2 <- b (toScript superScriptTopShift pos);
    symbol "}";
    let nextPos = selectRightPos (backScript subScriptTopShift nextPos1) $ backScript superScriptTopShift nextPos2
    in return $ ParseResult (lexes1++lexes2) $ nextPos
}

-- b'_2: B' -> ^{B}

b'_2 :: Pos -> Parser ParseResult
b'_2 pos = do {
    symbol "^";
    symbol "{";
    ParseResult lexes nextPos <- b (toScript superScriptTopShift pos);
    symbol "}";
    return $ ParseResult lexes (backScript superScriptTopShift nextPos)
}

-- b'_3: B' -> _{B}

b'_3 :: Pos -> Parser ParseResult
b'_3 pos = do {
    symbol "_";
    symbol "{";
    ParseResult lexes nextPos <- b (toScript subScriptTopShift pos);
    symbol "}";
    return $ ParseResult lexes (backScript subScriptTopShift nextPos)
}

-- b2: B2 -> \int{B}{B}{B} | \sum{B}{B}{B} | id | num | \blank | (B)
-- B2 is the only non-terminal that allows empty(failed) match (of formula strings).

b2 :: Pos -> Parser ParseResult
b2 pos = b2_1 pos <|> b2_2 pos <|> b2_3 pos <|> b2_4 pos <|> b2_5 pos <|> b2_6 pos <|> empty

-- b2_1: B2 -> \int{B}{B}{B}
b2_1 :: Pos -> Parser ParseResult
b2_1 pos = do {
    symbol "\\int";
    symbol "{";
    ParseResult lexes1 nextPos1 <- b $ toScript subScriptTopShift $ bigRight intRightParam $ pos;
    symbol "}";
    symbol "{";
    ParseResult lexes2 nextPos2 <- b $ toScript superScriptTopShift $ bigRight intRightParam $ pos;
    symbol "}";
    symbol "{";
    let {nextPos' = selectRightPos (backScript subScriptTopShift nextPos1) $ backScript superScriptTopShift nextPos2};
    ParseResult lexes nextPos <- b nextPos';
    symbol "}";
    return $ ParseResult ([LexToken "∫" $ bigLeft intLeftParam $ pos]++lexes1++lexes2++lexes) nextPos
}

-- b2_2: B2 -> \sum{B}{B}{B}
b2_2 :: Pos -> Parser ParseResult
b2_2 pos = do {
    symbol "\\sum";
    symbol "{";
    ParseResult lexes1 nextPos1 <- b $ toScript subScriptTopShift $ bigRight sumRightParam $ pos;
    symbol "}";
    symbol "{";
    ParseResult lexes2 nextPos2 <- b $ toScript superScriptTopShift $ bigRight sumRightParam $ pos;
    symbol "}";
    symbol "{";
    let {nextPos' = selectRightPos (backScript subScriptTopShift nextPos1) $ backScript superScriptTopShift nextPos2};
    ParseResult lexes nextPos <- b nextPos';
    symbol "}";
    return $ ParseResult ([LexToken "∑" $ bigLeft sumLeftParam $ pos]++lexes1++lexes2++lexes) nextPos
}

-- b2_3: B2 -> id
b2_3 :: Pos -> Parser ParseResult
b2_3 pos = do {
    id <- identifier;
    return $ ParseResult [LexToken id $ toItalic pos] $ (rightShift $ length id) pos
}

-- b2_4: B2 -> num
b2_4 :: Pos -> Parser ParseResult
b2_4 pos = do {
    num <- natural;
    return $ ParseResult [(LexToken $ show num) pos] $ (rightShift $ length $ show num) pos
}

-- b2_5: B2 -> \blank
b2_5 :: Pos -> Parser ParseResult
b2_5 pos = do {
    symbol "\\blank";
    return $ ParseResult [LexToken " " pos] $ rightShift 1 pos
}

-- b2_6: B2 -> (B)
b2_6 :: Pos -> Parser ParseResult
b2_6 pos = do {
    symbol "(";
    ParseResult lexes nextPos <- b (rightShift 1 pos);
    symbol ")";
    return $ ParseResult ([LexToken "(" pos]++lexes++[LexToken ")" nextPos]) $ rightShift 1 nextPos
}


-- Testing

{-

tb2 = parse (b2 startPos) "hello"
tb1 = parse (b1 startPos) "hello"
tbm1 = parse (bm startPos) ""
tbm2 = parse (bm startPos) "_{22}^{33}_^{45}{67}"

test1 = parse (s startPos) "$a^{2}$"
test2 = parse (s startPos) "$a_^{c2}{b}$"
test3 = parse (s startPos) "$\\sum{a^{2}}{b^{2}}{(c\\int{1}{2}{dt})}$"

-}