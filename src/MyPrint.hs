module MyPrint (printLine, showNumber, emphasis) where

import Text.Printf
import qualified Data.Text as T

printLine :: String -> T.Text -> IO ()
printLine a b = printf "[%-12s] %s\n" a (T.unpack b)

showNumber :: Double -> T.Text
showNumber n = T.pack $ showNumber' n
  where
    showNumber' :: Double -> String
    showNumber' n'
      | n' > 0 = "+ " ++ show n'
      | n' < 0 = "- " ++ show (abs n')
      | otherwise = show (abs n')

startEmphasis :: T.Text
startEmphasis = T.pack "\ESC[01;31m"

endEmphasis :: T.Text
endEmphasis = T.pack "\ESC[39;49m\ESC[0m"

emphasis :: T.Text -> (Int, Int) -> T.Text
emphasis str range = T.concat [subBody, T.pack "\n", mainBody]
  where
    pre = T.take (fst range) str
    target = T.take (snd range - fst range) (T.drop (fst range) str)
    post = T.drop (snd range) str
    mainBody = T.concat [pre, startEmphasis, target, endEmphasis, post]

    preSpc = T.concat [T.pack " " | _ <- [1 .. tWidth pre]]
    targetHat = T.concat [T.pack "v" | _ <- [1 .. tWidth target]]
    subBody = T.concat [preSpc, startEmphasis, targetHat, endEmphasis]

    tWidth :: T.Text -> Int
    tWidth txt = sum . map tCharWidth $ T.unpack txt

    tCharWidth :: Char -> Int
    tCharWidth c = case c of
      c' | c' <= '\x7F' -> 1
      _ -> 2

