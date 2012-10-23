library('RUnit')
 
source('sample.r')
 
test.suite <- defineTestSuite("example",
                              dirs = file.path("tests"),
                              testFileRegexp = '^.+\\.r$')
 
test.result <- runTestSuite(test.suite)
 
printTextProtocol(test.result)

