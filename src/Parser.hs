module Parser (parseLine, AST (..)) where

import Exception
import qualified Data.Text as T
import AST (AST (..))
import Lexer (Token (..), tokenRange)
import MyPrint
import Debug.Trace (trace)

data LineType = Assignment | Evaluation | SysCommand
instance Show LineType where
    show Assignment = "Assignment"
    show Evaluation = "Evaluation"
    show SysCommand = "SysCommand"

data ParseContext = ParseContext
  { expression :: T.Text,
    tokens :: [Token],
    idx :: Int,
    lineType :: LineType
  }

data Command = Command
  { commandAst :: AST,
    commandType :: LineType
  } deriving (Show)


nc :: ParseContext -> Int -> ParseContext
nc ctx n = let i = idx ctx in ctx {idx = i + n}

st :: ParseContext -> LineType -> ParseContext
st ctx t = ctx {lineType = t}

sayError :: ParseContext -> String -> ExceptTT a
sayError ctx msg =
  throwError $ T.concat [T.pack "ParseError: ", combinedMsg]
  where
    combineMsg :: ParseContext -> String -> T.Text
    combineMsg ctx' msg' = T.concat [T.pack msg', T.pack "\n", emphasized, T.pack "\n"]
      where
        ts = tokens ctx'
        i = idx ctx' - 1
        t = ts !! i
        range = tokenRange t
        emphasized = MyPrint.emphasis (expression ctx') range
    combinedMsg =
      if idx ctx == 0
        then T.pack msg
        else combineMsg ctx msg

-- パーサー
parseLine :: T.Text -> [Token] -> ExceptTT Command
parseLine expr ts = do
  let ctx = ParseContext {expression = expr, tokens = ts, idx = 0, lineType = SysCommand}
  (ctx', ast, rest) <- parseAddSub ctx ts
  if null rest
    then return Parser.Command {commandAst = ast, commandType = lineType ctx'}
    else do
      (ctx'', ast', rest') <- parseLineLHS ctx' ast rest
      case rest' of
        []       -> return Parser.Command {commandAst = ast', commandType = lineType ctx''}
        [TokQ _] -> return Parser.Command {commandAst = ast', commandType = lineType (st ctx'' Evaluation) }
        _        -> sayError ctx "[parseLine] Extra Tokens"

-- "右辺" の解析
parseLineLHS :: ParseContext -> AST -> [Token] -> ExceptTT (ParseContext, AST, [Token])
parseLineLHS ctx _ [] = sayError ctx "[parseLineLHS] no tokens"
parseLineLHS ctx lhs ([TokEqual _, TokQ _]) = do
  let ctx_ = nc (st ctx Evaluation) 2
  return (ctx_, lhs, [])
parseLineLHS ctx lhs (TokEqual _ : ts) = do
  let ctx_ = nc (st ctx Assignment) 1
  (ctx', rhs, rest) <- parseAddSub ctx_ ts
  if null rest
    then return (ctx', Equal lhs rhs, rest)
    else parseLineRHS ctx' (Equal lhs rhs) rest
parseLineLHS ctx ast ts = return (ctx, ast, ts)

-- "左辺" の解析
parseLineRHS :: ParseContext -> AST -> [Token] -> ExceptTT (ParseContext, AST, [Token])
parseLineRHS ctx _ []               = sayError ctx "[parseLineRHS] no tokens"
parseLineRHS ctx _ (TokEqual _ : _) = sayError (nc ctx 1) "[parseLineRHS] multiple \"=\""
parseLineRHS ctx ast ts             = do
  let ctx_ = nc (st ctx Evaluation) 1
  return (ctx_, ast, ts)


-- 加算・減算を解析
parseAddSub :: ParseContext -> [Token] -> ExceptTT (ParseContext, AST, [Token])
parseAddSub ctx ts = do
  (ctx', term, rest) <- parseMulDiv ctx ts
  parseAddSub' ctx' term rest

-- 二項+, 二項-を解析
parseAddSub' :: ParseContext -> AST -> [Token] -> ExceptTT (ParseContext, AST, [Token])
parseAddSub' ctx ast (TokPlus _ : ts) = do
  (ctx', term, rest) <- parseMulDiv (nc ctx 1) ts
  parseAddSub' ctx' (Add ast term) rest
parseAddSub' ctx ast (TokMinus _ : ts) = do
  (ctx', term, rest) <- parseMulDiv (nc ctx 1) ts
  parseAddSub' ctx' (Sub ast term) rest
parseAddSub' ctx ast ts = return (ctx, ast, ts)

-- 乗算を解析
parseMulDiv :: ParseContext -> [Token] -> ExceptTT (ParseContext, AST, [Token])
parseMulDiv ctx ts = do
  (ctx', factor, rest) <- parsePow ctx ts
  parseMulDiv' ctx' factor rest

parseMulDiv' :: ParseContext -> AST -> [Token] -> ExceptTT (ParseContext, AST, [Token])
parseMulDiv' ctx ast (TokMul _ : ts) = do
  (ctx', factor, rest) <- parsePow (nc ctx 1) ts
  parseMulDiv' ctx' (Mul ast factor) rest
parseMulDiv' ctx ast (TokDiv _ : ts) = do
  (ctx', factor, rest) <- parsePow (nc ctx 1) ts
  parseMulDiv' ctx' (Div ast factor) rest
parseMulDiv' ctx ast ts = return (ctx, ast, ts)

-- 累乗を解析
parsePow :: ParseContext -> [Token] -> ExceptTT (ParseContext, AST, [Token])
parsePow ctx ts = do
  (ctx', base, rest) <- parseUnary ctx ts
  parsePow' ctx' base rest

parsePow' :: ParseContext -> AST -> [Token] -> ExceptTT (ParseContext, AST, [Token])
parsePow' ctx base (TokPow _ : n@(TokNum _ _) : rest) = do
  (ctx', expo, _) <- parseTerm (nc ctx 1) [n]
  return (ctx', Pow base expo, rest)
parsePow' ctx _ (TokPow _ : _) = sayError (nc ctx 1) "[parsePow'] Power(^) requires Number as Right Operand"
parsePow' ctx base totsens = return (ctx, base, totsens)

parseUnary :: ParseContext -> [Token] -> ExceptTT (ParseContext, AST, [Token])
-- 単項プラス
parseUnary ctx (TokPlus _ : ts) = parseParen (nc ctx 1) ts
-- 単項マイナス
parseUnary ctx (TokMinus _ : ts) = do
  (ctx', term, rest) <- parseParen (nc ctx 1) ts
  return (ctx', Mul (Num (-1.0)) term, rest)
-- フォールスルー
parseUnary ctx ts = parseParen ctx ts

-- 括弧を解析
parseParen :: ParseContext -> [Token] -> ExceptTT (ParseContext, AST, [Token])
parseParen ctx (TokLParen _ : ts) = do
  let ctx' = nc ctx 1
  (ctx'', subexpr, rest) <- parseAddSub ctx' ts
  case rest of
    (TokRParen _ : ts') -> return (nc ctx'' 1, subexpr, ts')
    _ -> sayError ctx' "[parseParen] Missing Closing Parenthesis(')') in Right Position"
parseParen ctx ts = parseTerm ctx ts

-- 単項（数値や変数）を解析
parseTerm :: ParseContext -> [Token] -> ExceptTT (ParseContext, AST, [Token])
-- 数値
parseTerm ctx (TokNum n _ : ts) = return (nc ctx 1, Num n, ts)
-- 変数（X ^ n）
parseTerm ctx (TokIdent var _ : TokPow _ : TokNum expo _ : ts) = do
  if not (isInteger expo)
    then sayError (nc ctx 3) "[parseTerm] Exponent must be an Integer"
    else do
      if expo == 0
        then return (nc ctx 3, Num 1, ts)
        else return (nc ctx 3, Var var (round expo), ts)
  where
    isInteger :: Double -> Bool
    isInteger x = fromIntegral (floor x :: Integer) == x

-- 変数（X）
parseTerm ctx (TokIdent var _ : ts) =
  return (nc ctx 1, Var var 1, ts)
  
-- まずいパターン
parseTerm ctx (TokRParen _ : _) = sayError (nc ctx 1) "[parseTerm] Unexpected Closing Parenthesis(')')"
parseTerm ctx (TokPow _ : _)    = sayError (nc ctx 1) "[parseTerm] Power(^) requires Left Operand"
parseTerm ctx (t : _)           = sayError ctx      $ "[parseTerm] Unexpected Token in this ParseContext: " ++ show t
parseTerm ctx []                = sayError ctx        "[parseTerm] Neither Number nor Variable Token"
