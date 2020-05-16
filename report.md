# 数学公式的语法解析与排版

[TOC]

## 库

本项目基于Graham Hutton在Programming in Haskell一书中提出的函数式parser库Parsing.hs.

## 修改文法

函数式编程进行语法分析的原理是模式匹配，而不是基于自动机，所以只需要消除原始文法的二义性和左递归即可。将原始的文法修改如下：

```
S -> $B$
B -> B1B | B1
B1 -> B2Bm | B2
  Bm -> B'Bm | e
  B' -> _^{B} | ^{B} | _{B}
B2 -> \int{B}{B}{B} | \sum{B}{B}{B} | id | num | \blank | (B)
```

注：我们在实现上述文法时，忽略输入公式字符串中所有的空格，只将其视为分隔符。

## 设计语法记号

目标HTML文件中，和数学公式排版有关的语句是这样的：

```html
<div style="position: absolute; top:175px; left:500px;"><span style="font-size:50px; font-style:oblique; lineheight:100%;">a</span></div>
<div style="position: absolute; top:160px; left:525px;"><span style="font-size:30px; font-style:normal; lineheight:100%;">2</span></div>
```

实质上是把公式解析成了一系列字符串，每个字符串具有自己的位置、字号和样式，放在对应的位置之后就实现了公式排版效果。

我们定义如下的结构描述一个解析出来的语法记号：

```haskell
-- Pos.hs
data Pos = Pos { 
    top :: Float, 
    left :: Float, 
    fontsize :: Float, 
    normal :: Bool  -- True: normal, False: oblique
} 

-- FormulaParser.hs
data LexToken = LexToken {
    str :: String,
    pos :: Pos
} 
```

类型Pos用来给定全部的排版配置（top, left, font-size, font-style)，类型LexToken定义字符串和排版配置组成一个语法记号。

## Parser的类型

在Parsing.hs中，Parser的类型定义是`newtype Parser a = P (String -> [(a,String)])`，解析函数的类型是`parse :: Parser a -> String -> [(a,String)]`。我们需要确定`a`是谁。

对于Parser，最终解析的解析结果应该是一系列的LexToken，类型是[LexToken]. 但是考虑到如下两点：

* Parser需要一个排版配置参数的输入，来确定输出的LexToken的放置位置

* 非终结符对应的Parser，只对应一部分文本的匹配，在匹配结束之后还要给出已给位置，作为继续放置下面排版记号的初始位置

所以确定Parser的类型

```haskell
-- FormulaParser.hs
data ParseResult = ParseResult {
    lexes :: [LexToken],
    nextPos :: Pos
} 

s :: Pos -> Parser ParseResult
```

## 非终结符S和B

这两个非终结符没有理解难度，S的作用是处理公式开头和结尾的标识符$，B的作用是处理连续多个的“基本公式单元”。

S匹配整个公式。按照语法规则，S依次匹配一个\$，一个B，一个\$，并直接返回B的解析结果作为自己的解析结果。

```haskell
-- FormulaParser.hs
s :: Pos -> Parser ParseResult
s pos = do {
    symbol "$";
    pr <- b pos;
    symbol "$";
    return pr
}
```

B匹配至少一个B1，语法规则被递归地写成依次匹配一个B1和一个B，或匹配B1. 

按照语法规则写，要么依次匹配一个B1和一个B，以B1的输出位置为B的输入位置，结合两者的解析结果进行输出，要么匹配一个B1，输出B1的结果。

```haskell
-- FormulaParser.hs
b :: Pos -> Parser ParseResult
b pos = do { 
    ParseResult lexes1 nextPos1 <- b1 pos; 
    ParseResult lexes nextPos <- b nextPos1;
    return $ ParseResult (lexes1++lexes) nextPos
} <|> do { 
    pr1 <- (b1 pos); 
    return pr1
}
```

## 非终结符B1：上标、下标、上下标的处理

B1的作用是匹配“带有上下标的公式”，而B2的作用是匹配“不带上下标的公式”。按照语法规则，Bm匹配“零个或多个上下标”，B1要么匹配一个B2和一个Bm，要么匹配一个B2. 

B1的实现细节和B很相似，没什么难的。

```haskell
-- FormulaParser.hs
b1 :: Pos -> Parser ParseResult
b1 pos = do {
    ParseResult lexes2 nextPos2 <- b2 pos;
    ParseResult lexesm nextPosm <- bm nextPos2;
    return $ ParseResult (lexes2++lexesm) nextPosm
} <|> do {
    pr2 <- b2 pos;
    return pr2
}
```

Bm匹配“零个或多个上下标”，递归地写成“匹配一个B'后匹配Bm，或匹配一个空串”。如果是前者，后匹配的Bm以前面B'的输出位置为起始位置即可，后匹配Bm的输出位置为最终的输出位置；如果是后者，ParseResult中语法记号列表为空，把输入位置作为输出位置即可。

```haskell
-- FormulaParser.hs
bm :: Pos -> Parser ParseResult
bm pos = do {
    ParseResult lexes' nextPos' <- b' pos;
    ParseResult lexesm nextPosm <- bm nextPos';
    return $ ParseResult (lexes'++lexesm) nextPosm
} <|> (return $ ParseResult [] pos)
```

### 非终结符B'：上标下标的位置变换

把标准位置变成上标（下标）位置，变化在于：位置升高（降低）了，字号变小了。用如下的函数实现标准位置到上下标位置的变换：

```haskell
-- Pos.hs
scriptFontsizeScale = 0.6   --上下标字体缩小比例

toScript :: Float -> Pos -> Pos 
toScript topShift (Pos t l f n) = Pos (t+topShift*f) l (f*scriptFontsizeScale) n

subScriptTopShift = 0.7   
superScriptTopShift = -0.3 
```

backScript是toScript的逆变换：

```haskell
-- Pos.hs
backScript :: Float -> Pos -> Pos
backScript topShift (Pos t l f n) = let f' = f/scriptFontsizeScale in
    Pos (t-topShift*f') l f' n
```

在这种情况下就可以定义匹配`^{B}`的parser：

```haskell
-- ^{B}
do {
    symbol "^";
    symbol "{";
    ParseResult lexes nextPos <- b (toScript superScriptTopShift pos);
    symbol "}";
    return $ ParseResult lexes (backScript superScriptTopShift nextPos)
}
```

上标的输入位置由基准位置变换而来，上标的输出位置再变换回去，成为最终的输出位置。

![script_pos_illu](https://gitee.com//WinterShiver/myimgs/raw/master/img/20200516192213.png)

下标也类似。但是上下标要更复杂一些。

上下标需要：确定下标输入位置-接收下标-确定下标后续位置-确定上标输入位置-接收上标-确定上标后续位置，然后在下标后续位置和上标后续位置中，选择较靠右的位置作为随后接收输入的位置。

在实现上下标的parser中，借助selectRightPos函数选取两个位置中较靠右的，然后返回这个位置为下一步的位置。

```haskell
-- Pos.hs
selectRightPos :: Pos -> Pos -> Pos
selectRightPos pos1 pos2 = 
    let Pos _ l1 _ _ = pos1 
        Pos _ l2 _ _ = pos2 
    in if l1 > l2 then pos1 else pos2
    
-- FormulaParser.hs
do {
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
```

B'的定义对上下标，上标和下标的parser做了一下sum. 如下所示：

```haskell
-- FormulaParser.hs
b' :: Pos -> Parser ParseResult
b' pos = do {
    symbol "_^";
    symbol "{";
    ParseResult lexes1 nextPos1 <- b (toScript subScriptTopShift pos);
    symbol "}";
    symbol "{";
    ParseResult lexes2 nextPos2 <- b (toScript superScriptTopShift pos);
    symbol "}";
    let nextPos = selectRightPos (backScript subScriptTopShift nextPos1) $ backScript superScriptTopShift nextPos2
    in return $ ParseResult (lexes1++lexes2) $ nextPos
} <|> do {
    symbol "^";
    symbol "{";
    ParseResult lexes nextPos <- b (toScript superScriptTopShift pos);
    symbol "}";
    return $ ParseResult lexes (backScript superScriptTopShift nextPos)
} <|> do {
    symbol "_";
    symbol "{";
    ParseResult lexes nextPos <- b (toScript subScriptTopShift pos);
    symbol "}";
    return $ ParseResult lexes (backScript subScriptTopShift nextPos)
}
```

## 非终结符B2

所有First字符是非终结符的东西都塞进了B2里面。B2匹配的东西主要分两类，一类是积分号、求和号这样的大公式（以下称“大符号”），另一类是尺寸普通的符号（id，num，括号）

### 大符号的处理

确定大符号的插入位置，特殊之处在于：

* 大符号的fontsize需要变大
* 大符号的插入位置，相对于基准位置，需要进行一点偏移
* 大符号之后的符号的插入位置，需要单独进行计算

所以需要这些东西（以积分号为例）：

```haskell
-- Pos.hs
bigLeft :: (Float, Float, Float) -> Pos -> Pos
bigLeft (topShift, leftShift, fontsizeScale) (Pos t l f n) =
    Pos (t+topShift*f) (l+leftShift*f) (f*fontsizeScale) n

bigRight :: Float -> Pos -> Pos
bigRight rightShift (Pos t l f n) = Pos t (l+rightShift*f) f n

intLeftParam = (-0.3, 0.0, 1.6) :: (Float, Float, Float)
intRightParam = 0.7 :: Float
```

为`bigLeft intLeftParam`输入基准位置，输出放积分号（大符号）的位置；为`bigLeft intRightParam`输入基准位置，输出大符号之后的基准位置。这个基准位置会被大符号的上下标利用，获得它们的对应位置。

积分号的parser代码如下：

```haskell
-- FormulaParser.hs
do {
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
```

放大符号、下标、上标、符号内部文字的位置都要单独计算，返回位置取符号内部文字的返回位置。

对于其他大符号，parser完全相同，只是使用的参数需要调整。

```haskell
-- Pos.hs
sumLeftParam = (-0.4, 0.0, 1.6) :: (Float, Float, Float)
sumRightParam = 1.3 :: Float
```

### 基本符号的处理

基本符号包括id，num和括号。

id的匹配可以直接利用Parsing.hs的identifier，但是要注意输出位置的计算。考虑到，对于id，内部每个字符的宽度是一致的。id的输出位置=输入位置右移字符个数*字符宽度（num和括号也用的上），我们将这个逻辑封装成一个函数：

```haskell
-- Pos.hs
aspectRadio = 2.0   -- 正常字符的长宽比，长：宽

rightShift :: Int -> Pos -> Pos -- 给定i和基准位置，计算按照标准宽度右移i个字符的位置
rightShift i (Pos t l f n) = 
    let i' = fromIntegral i
        fontWidth = f / aspectRadio -- 当前字体大小下，字符的宽度
    in Pos t (l+i'*fontWidth) f n
```

id的parser定义如下（num类似）：

```haskell
-- FormulaParser.hs
do {
    id <- identifier;
    return $ ParseResult [LexToken id $ toItalic pos] $ (rightShift $ length id) pos
}
```

注意定义里面的toItalic函数，这个函数是为了使id输出为斜体。如有疑惑可以看Pos.hs中此函数的定义。

括号的parser也类似，只是额外处理的左右括号的parse和位置计算：

```haskell
-- FormulaParser.hs
do {
    symbol "(";
    ParseResult lexes nextPos <- b (rightShift 1 pos);
    symbol ")";
    return $ ParseResult ([LexToken "(" pos]++lexes++[LexToken ")" nextPos]) $ rightShift 1 nextPos
}
```

把这些parser都alter起来，获得b2的定义：

```haskell
-- FormulaParser.hs
b2 :: Pos -> Parser ParseResult
b2 pos = do {
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
} <|> do {
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
} <|> do {
    id <- identifier;
    return $ ParseResult [LexToken id $ toItalic pos] $ (rightShift $ length id) pos
} <|> do {
    num <- natural;
    return $ ParseResult [(LexToken $ show num) pos] $ (rightShift $ length $ show num) pos
} <|> do {
    symbol "\\blank";
    return $ ParseResult [LexToken " " pos] $ rightShift 1 pos
} <|> do {
    symbol "(";
    ParseResult lexes nextPos <- b (rightShift 1 pos);
    symbol ")";
    return $ ParseResult ([LexToken "(" pos]++lexes++[LexToken ")" nextPos]) $ rightShift 1 nextPos
} <|> empty
```

注意最后的empty，b2允许匹配失败，但不允许匹配空串为valid。

## 获得parse结果

### 执行parse获得结果

执行FormulaParser.hs里面的test1（`test1 = parse (s startPos) "$a^{2}$"`）获得如下结果：

![parse_succ](https://gitee.com//WinterShiver/myimgs/raw/master/img/20200516215218.png)

FormulaParser.hs已经可以把公式字符串翻译成ParseResult了（如果执行失败会得到空的列表）。

### 翻译parse结果

Translator.hs把ParseResult的信息提取出来，翻译成HTML代码。具体的做法是，把ParseResult里面的[LexToken]逐元素翻译成代码语句，然后拼接起来并加上头尾。具体可见Translator.hs.

![trans_succ](https://gitee.com//WinterShiver/myimgs/raw/master/img/20200516221219.png)

获得的效果如上图所示（如果parse失败，空的列表传入translate函数，会得到空的HTML文件）。

## 完成所有文件的翻译

display函数读取一个文件内的公式字符串，翻译成HTML并输出到指定文件：

```haskell
display :: String -> String -> IO ()
display srcPath tgtPath = do {
    src <- readFile srcPath;
    let tgt = translate src 
    in writeFile tgtPath tgt
}
```

displays可以执行多个文件的翻译过程。main调用displays，应用在文件列表，得到所有文件的翻译结果。

## 结果展示

![image-20200516221722624](https://gitee.com//WinterShiver/myimgs/raw/master/img/20200516221724.png)

![image-20200516221745557](https://gitee.com//WinterShiver/myimgs/raw/master/img/20200516221747.png)

![image-20200516221803439](https://gitee.com//WinterShiver/myimgs/raw/master/img/20200516221804.png)

![image-20200516221821207](https://gitee.com//WinterShiver/myimgs/raw/master/img/20200516221822.png)

![image-20200516221839951](https://gitee.com//WinterShiver/myimgs/raw/master/img/20200516221841.png)

![image-20200516221854355](https://gitee.com//WinterShiver/myimgs/raw/master/img/20200516221855.png)

![image-20200516221912310](https://gitee.com//WinterShiver/myimgs/raw/master/img/20200516221913.png)