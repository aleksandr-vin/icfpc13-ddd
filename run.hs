

import Data
import Gen
import qualified Data.List as L


run n o = genP (n-1) $ readOps o
