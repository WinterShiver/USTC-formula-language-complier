-- Pos.hs
-- Position representation: 对于parse时字符串的位置标记，提出Pos类型进行记录，并给出一系列相关方法

module Pos (module Pos) where

-- Definition

data Pos = Pos { 
    top :: Float, 
    left :: Float, 
    fontsize :: Float, 
    normal :: Bool  -- True: normal, False: oblique
} 

instance Show Pos where
    -- show :: Show a => a -> String
    show (Pos t l f n) = "Pos: "++(show t)++"\t"++(show l)++"\t"++(show f)++"\t"++(show n)++"\t\n"

-- test = Pos {top = 1, left = 2, fontsize = 3, normal = True} 
-- test2 = Pos 2 3 4 False


-- Initial state

startTop = 175.0   --起始顶位置
startLeft = 200.0   --起始左位置
normalFontsize = 50.0   --起始字号，正常字体大小的字号

startPos = Pos startTop startLeft normalFontsize True 


-- Super scripts and sub scripts

scriptFontsizeScale = 0.6 :: Float   --上下标字体缩小比例

-- toScript: 给定上标/下标参数和基准位置，计算上标/下标的起始位置
-- topShift: 移动到上标/下标位置时，top相对基准位置的偏移量与字体高度的比例
toScript :: Float -> Pos -> Pos 
toScript topShift (Pos t l f n) = Pos (t+topShift*f) l (f*scriptFontsizeScale) n

-- backScript: 给定上标/下标参数和上标/下标的结束位置，计算返回基准配置之后的位置
backScript :: Float -> Pos -> Pos
backScript topShift (Pos t l f n) = let f' = f/scriptFontsizeScale in
    Pos (t-topShift*f') l f' n

-- 上标和下标的具体参数
subScriptTopShift = 0.7 :: Float   
superScriptTopShift = -0.3 :: Float

-- Super sub scripts

-- selectRightPos: 给定两个位置，返回两个位置中较靠右的位置。
-- 作为参数的两个位置通常是
selectRightPos :: Pos -> Pos -> Pos
selectRightPos pos1 pos2 = 
    let Pos _ l1 _ _ = pos1 
        Pos _ l2 _ _ = pos2 
    in if l1 > l2 then pos1 else pos2


-- Big symbols: id and sum

-- bigLeft: 给定相应参数和基准位置，计算放置大符号的位置
bigLeft :: (Float, Float, Float) -> Pos -> Pos
bigLeft (topShift, leftShift, fontsizeScale) (Pos t l f n) =
    Pos (t+topShift*f) (l+leftShift*f) (f*fontsizeScale) n

-- bigRight: 给定相应参数和基准位置，计算放置大符号后，后续符号的基准位置
bigRight :: Float -> Pos -> Pos
bigRight rightShift (Pos t l f n) = Pos t (l+rightShift*f) f n

-- int符号在bigLeft和bigRight的参数

intLeftParam = (-0.3, 0.0, 1.6) :: (Float, Float, Float)
intRightParam = 0.7 :: Float

-- sum符号在bigLeft和bigRight的参数

sumLeftParam = (-0.4, 0.0, 1.6) :: (Float, Float, Float)
sumRightParam = 1.3 :: Float


-- Normal symbols: id, num, ()

aspectRadio = 2.0   -- 正常字符的长宽比，长：宽

rightShift :: Int -> Pos -> Pos -- 给定i和基准位置，计算按照标准宽度右移i个字符的位置
rightShift i (Pos t l f n) = 
    let i' = fromIntegral i
        fontWidth = f / aspectRadio -- 当前字体大小下，字符的宽度
    in Pos t (l+i'*fontWidth) f n

-- id

toItalic :: Pos -> Pos -- 把属性值normal改成False
toItalic (Pos t l f n) = Pos t l f False