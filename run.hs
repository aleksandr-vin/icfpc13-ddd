

import Data
import Gen
import Opener
import Compiler
import Text.JSON
import System.IO
import System.Environment
import Data.Word

-- sample input
sss = "[{\"inputs\":[\"0\",\"1\",\"2\"]},{\"size\":[\"3\"]},{\"operators\":[\"not\"]}]"
dss = decode sss :: Result [JSObject JSValue]

data DInput = DInput { inputs :: [String], size :: [String], operators :: [String] }
              deriving (Show)

getInputs :: Result [JSObject JSValue] -> Result DInput
getInputs (Ok objs) = let (!) = flip valFromObj 
                          obj1 = objs !! 0
                          obj2 = objs !! 1
                          obj3 = objs !! 2
                      in do
  inputs    <- obj1 ! "inputs"
  size      <- obj2 ! "size"
  operators <- obj3 ! "operators"
  return DInput { inputs = inputs, size = size, operators = operators }


run n o = genP (n-1) $ readOps o

orun n o = openP $ run n o

crun xs n o = let progs     = openP $ run n o
                  funs      = map (compile) progs 
                  compute f = map (f) xs
                  results   = map (compute) funs
              in (progs, results, xs)

main = do
  [inf, outf] <- getArgs
  s <- readFile inf
  writeFile outf (foo s) -- decode s :: Result [JSObject JSValue])
  
foo x = toJson (crun xs n o)
  where objs = decode x :: Result [JSObject JSValue]
        din = (\(Ok x) -> x) $ getInputs objs
        xs = map (\x -> read x :: Word64) $ inputs din
        n = read ((size din) !! 0) :: Int
        o = operators din

json progs =
    encode $ [toJSObject [(name, map (JSString . toJSString . show) progs)]]
    where name = "solutions"

toJson (p, r, x) =
  encode $ [toJSObject [("solutions", map (JSString . toJSString . show) p)],
            toJSObject [("results",   map (JSString . toJSString . show) r)],
            toJSObject [("inputs",    map (JSString . toJSString . show) x)]]
