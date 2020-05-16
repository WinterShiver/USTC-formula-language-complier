module Main where

import Translator

srcPaths = [
    "../test/test01.txt"
    , "../test/test02.txt"
    , "../test/test03.txt"
    , "../test/test04.txt"
    , "../test/test05.txt"
    , "../test/test06.txt"
    , "../test/test07.txt"
    , "../test/test08.txt"
    , "../test/test09.txt"
    , "../test/test10.txt"
    , "../test/test_challenge.txt"]

tgtPaths = [
    "../result/test01.html"
    , "../result/test02.html"
    , "../result/test03.html"
    , "../result/test04.html"
    , "../result/test05.html"
    , "../result/test06.html"
    , "../result/test07.html"
    , "../result/test08.html"
    , "../result/test09.html"
    , "../result/test10.html"
    , "../result/test_challenge.html"]

display :: String -> String -> IO ()
display srcPath tgtPath = do {
    src <- readFile srcPath;
    let tgt = translate src 
    in writeFile tgtPath tgt
}

displays :: [(String, String)] -> IO ()
displays [] = putStrLn "Translation Done."
displays (st:sts) = let (s, t) = st in do {
    putStrLn ("Translating "++s++" into "++t);
    display s t;
    displays sts
}

main :: IO ()
main = displays $ zip srcPaths tgtPaths