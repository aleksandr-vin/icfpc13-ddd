

import DataTests
import GenTests
import OpenerTests
import CompilerTests

run =
    do DataTests.testIt
       GenTests.testIt
       OpenerTests.testIt
       CompilerTests.testIt
