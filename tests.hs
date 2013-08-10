

import DataTests
import GenTests
import OpenerTests

run =
    do DataTests.testIt
       GenTests.testIt
       OpenerTests.testIt
