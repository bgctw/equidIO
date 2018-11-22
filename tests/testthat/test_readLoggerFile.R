.tmp.f <- function(){
  require(testthat) 
  require(rlang)
}
context("readEquidistantCsv")

testData <- tribble(
  ~Date, ~Time, ~minute
  ,"2018-10-03", "06:00", 0
  ,"2018-10-03", "06:15", 15
  ,"2018-10-03", "06:30", 30
  ,"2018-10-03", "06:45", 45
  ,"2018-10-03", "07:00", 60
  ,"2018-10-03", "07:15", 75
)

tmpDir <- tempdir()
tmpFile <- file.path(tmpDir, "testData.csv")
write_csv(testData, tmpFile)

# generate csvFile with additional header lines not matching column types
lines <- read_lines(tmpFile)
linesWithUnits <- c(
  lines[1]
  ,"-,-,min"
  ,"number,number,number"
  ,lines[-1]
)
tmpUnitsFile <- file.path(tmpDir, "testDataUnits.csv")
write_lines(linesWithUnits, tmpUnitsFile)

linesWithHeader <- c(
  " Some Header information"
  , "across"
  , "three lines"
  , linesWithUnits
)
tmpHeaderFile <- file.path(tmpDir, "testDataHeader.csv")
write_lines(linesWithHeader, tmpHeaderFile)

createTimestampDateTime <- function(
  ### add column timestamp from columns date and time
  data  ##<< data.frame to mutate
  , timezone ##<< ignored here
){
  data %>%  mutate(
    timestamp = as.POSIXct(
      paste(!!sym("Date"), !!sym("Time"))
      , format = "%Y-%m-%d %H:%M", tz = "GMT")
  )
}

test_that("readEquidistantCsv",{
  ans <- readEquidistantCsv(tmpFile, fCreateTimestamp = createTimestampDateTime )
  expect_equal( ans$minute, testData$minute)
  expect_equal( ans$timestamp[1], ISOdatetime(2018,10,3,6,0,0, tz = "GMT") )
})

testDataRe <- readEquidistantCsv(tmpFile, fCreateTimestamp = createTimestampDateTime )

test_that("readEquidistantCsv startTime",{
  startTime <- testDataRe$timestamp[3]
  ans <- readEquidistantCsv(
    tmpFile, fCreateTimestamp = createTimestampDateTime 
    , startTime = startTime
    )
  expect_equal( ans$minute, testData$minute[-(1:2)])
})

test_that("readEquidistantCsv startTime and endTime",{
  startTime <- testDataRe$timestamp[3]
  endTime <- testDataRe$timestamp[5]
  ans <- readEquidistantCsv(
    tmpFile, fCreateTimestamp = createTimestampDateTime 
    , startTime = startTime
    , endTime = endTime
  )
  expect_equal( ans$minute, testData$minute[3:5])
})

test_that("readEquidistantCsv startTime before file and endTime",{
  startTime <- testDataRe$timestamp[1] - 3600
  endTime <- testDataRe$timestamp[5]
  ans <- readEquidistantCsv(
    tmpFile, fCreateTimestamp = createTimestampDateTime 
    , startTime = startTime
    , endTime = endTime
  )
  expect_equal( ans$minute, testData$minute[1:5])
})


test_that("readEquidistantCsv endTime",{
  endTime <- testDataRe$timestamp[5]
  ans <- readEquidistantCsv(
    tmpFile, fCreateTimestamp = createTimestampDateTime 
    , endTime = endTime
  )
  expect_equal( ans$minute, testData$minute[1:5])
})


test_that("readEquidistantCsv endTime after file",{
  endTime <- testDataRe$timestamp[nrow(testDataRe)] + 3600
  ans <- readEquidistantCsv(
    tmpFile, fCreateTimestamp = createTimestampDateTime 
    , endTime = endTime
  )
  expect_equal( ans$minute, testData$minute)
})

test_that("readEquidistantCsv error missing time steps",{
  testData2 <- testData %>% slice(-3)
  tmpFile2 <- file.path(tmpDir,"testData2.csv")
  write_csv(testData2, tmpFile2)
  startTime <- testDataRe$timestamp[4] # after missing step
  expect_error(
    ans <- readEquidistantCsv(
      tmpFile2, fCreateTimestamp = createTimestampDateTime 
      , startTime = startTime
    )
    ,"equidistant"
    
  )
})

test_that("readEquidistantCsv misaligned startTime",{
  startTime <- testDataRe$timestamp[3] + 60*7
  expect_error(
    ans <- readEquidistantCsv(
      tmpFile, fCreateTimestamp = createTimestampDateTime 
      , startTime = startTime
    )
    ,"startTime must be a multiple"
    
  )
})

test_that("readEquidistantCsv misaligned endTime",{
  endTime <- testDataRe$timestamp[3] + 60*7
  expect_error(
    ans <- readEquidistantCsv(
      tmpFile, fCreateTimestamp = createTimestampDateTime 
      , endTime = endTime
    )
    ,"multiple"
  )
})


test_that("readEquidistantCsv colHeader startTime and endTime",{
  startTime <- testDataRe$timestamp[3]
  endTime <- testDataRe$timestamp[5]
  ans <- readEquidistantCsv(
    tmpUnitsFile, nRowsColumnHeader = 2
    , fCreateTimestamp = createTimestampDateTime 
    , startTime = startTime
    , endTime = endTime
  )
  expect_equal( ans$minute, testData$minute[3:5])
  dfHeader <- attr(ans, "columnHeader")
  expect_true("minute" %in% names(dfHeader))
  expect_equal( dfHeader$minute, c("min","number"))
})

test_that("readEquidistantCsv initial Header startTime and endTime",{
  startTime <- testDataRe$timestamp[3]
  endTime <- testDataRe$timestamp[5]
  ans <- readEquidistantCsv(
    tmpHeaderFile, nRowsColumnHeader = 2, nRowsHeader = 3
    , fCreateTimestamp = createTimestampDateTime 
    , startTime = startTime
    , endTime = endTime
  )
  expect_equal( ans$minute, testData$minute[3:5])
  dfHeader <- attr(ans, "columnHeader")
  expect_true("minute" %in% names(dfHeader))
  expect_equal( dfHeader$minute, c("min","number"))
  headerLines <- attr(ans, "header")
  expect_equal( headerLines, linesWithHeader[1:3])
})

test_that("readEquidistantCsv initial Header startTime and endTime colTypes",{
  startTime <- testDataRe$timestamp[3]
  endTime <- testDataRe$timestamp[5]
  ans <- readEquidistantCsv(
    tmpHeaderFile, nRowsColumnHeader = 2, nRowsHeader = 3
    , fCreateTimestamp = createTimestampDateTime 
    , startTime = startTime
    , endTime = endTime
    , col_types = cols(Date = col_character(), Time = col_character())
  )
  expect_equal( ans$minute, testData$minute[3:5])
  dfHeader <- attr(ans, "columnHeader")
  expect_true("minute" %in% names(dfHeader))
  expect_equal( dfHeader$minute, c("min","number"))
  headerLines <- attr(ans, "header")
  expect_equal( headerLines, linesWithHeader[1:3])
})




