

import Data
import Gen
import Opener


run n o = genP (n-1) $ readOps o

orun n o = openP $ run n o