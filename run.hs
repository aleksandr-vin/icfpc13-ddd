

import Data
import Gen
import Opener
import Text.JSON


run n o = genP (n-1) $ readOps o

orun n o = openP $ run n o

json progs =
    encode $ toJSObject [(name, map (JSString . toJSString. show) progs)]
    where name = "solutions"
