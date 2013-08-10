

import Data
import Gen
import qualified Data.List as L


run n o =
    let progs = genP (n-1) $ readOps o
    in filter (\p -> opsInProg (remapTFold o) p) progs

opsInProg o p = L.all (\x -> L.isInfixOf x (show p)) o

remapTFold o = map (\x -> if x == "tfold" then "fold" else x) o