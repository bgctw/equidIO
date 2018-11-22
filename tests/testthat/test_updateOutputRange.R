.tmp.f <- function(){
  library(testthat)
  library(dplyr)
  library(purrr)
  library(tidyr)
}

#require(testthat)
context("updateOutputRange")

dsTarget0 <- tibble(
  date = seq(ISOdatetime(2010,1,1,0,0,30, tz = "UTC"), by = "30 min", length.out = 10)
  , resp = as.numeric(1:10)
  , sdResp = 10.0 + 1:10
  , canopyPosition = "treeCovered"
  , treatment = "Nadd"
)
dsTarget <- rbind( dsTarget0, mutate(dsTarget0, canopyPosition = "openLand")) %>%
  arrange(canopyPosition, date) %>%
  mutate(canopyPosition = factor(canopyPosition))
indexColumns <- c("canopyPosition","treatment")

test_that("removeLastIncompleteRecord", {
  ds <- dsTarget %>% group_by_at(vars(one_of(indexColumns)))
  ans <- removeLastIncompleteRecord(ds, "date")
  expect_equal( count(ans)$n, c(10,10))
  #
  ds <- dsTarget %>% 
    mutate(
      date = as.POSIXct(
        ifelse(row_number() == n(), date - 10, date), origin = '1970-01-01')
    ) %>% 
    group_by_at(vars(one_of(indexColumns))) 
  ans <- removeLastIncompleteRecord(ds, "date")
  expect_equal( count(ans)$n, c(10,9))
})

test_that("updateOutputRange normal case",{
  dsNew <- dsTarget %>% slice(3:8) %>% mutate(resp = resp + 20.0)
  dsUp <- updateOutputRange(dsTarget, dsNew, indexColumns = indexColumns) %>%
    arrange(canopyPosition, date)
  expect_equal( nrow(dsUp), nrow(dsTarget))
  #problems with POSIXct: expect_equal( slice(dsUp, -(3:8)), slice(dsTarget, -(3:8)))
  expect_equal( slice(dsUp, -(3:8))$date, slice(dsTarget, -(3:8))$date)
  expect_equal( select(slice(dsUp, -(3:8)), -date), select(slice(dsTarget, -(3:8)), -date))
  #expect_equal( slice(dsUp, (3:8)), slice(dsNew))
  expect_equal( slice(dsUp, (3:8))$date, slice(dsNew)$date)
  expect_equal( select(slice(dsUp, (3:8)), -date), select(slice(dsNew), -date))
})

test_that("updateOutputRange additional times",{
  dsTarget1 <- slice(dsTarget, 1:18)
  dsNew <- slice(dsTarget, 17:20)
  iRowsT <- 1:16
  iRowsN <- 17:20
  dsUp <- updateOutputRange(dsTarget1, dsNew, indexColumns = indexColumns) %>%
    arrange(canopyPosition, date)
  expect_equal( nrow(dsUp), nrow(dsTarget))
  #problems with POSIXct: expect_equal( slice(dsUp, -(3:8)), slice(dsTarget, -(3:8)))
  expect_equal( slice(dsUp, iRowsT)$date, slice(dsTarget, iRowsT)$date)
  expect_equal( select(slice(dsUp, iRowsT), -date), select(slice(dsTarget, iRowsT), -date))
  #expect_equal( slice(dsUp, (3:8)), slice(dsNew))
  expect_equal( slice(dsUp, iRowsN)$date, slice(dsNew)$date)
  expect_equal( select(slice(dsUp, iRowsN), -date), select(slice(dsNew), -date))
})

test_that("updateOutputRange additional times with gap after target",{
  iRowsT <- 1:14
  iRowsN <- 17:20
  dsTarget1 <- slice(dsTarget, iRowsT)
  dsNew <- slice(dsTarget, iRowsN)
  dsUp <- updateOutputRange(dsTarget1, dsNew, indexColumns = indexColumns) %>%
    arrange(canopyPosition, date)
  expect_equal( nrow(dsUp), nrow(dsTarget))
  dsUp %>% group_by(!!!rlang::syms(indexColumns)) %>% nest() %>%
    mutate(ans = map_lgl(data, function(dss){
      expect_true( all(diff(as.numeric(dss$date)) == 1800))
    }))
  expect_equal( dsUp$resp[15], NA_real_ )
})

test_that("updateOutputRange additional times with gap before target",{
  iRowsT <- 4:20
  iRowsN <- 1:2
  dsTarget1 <- slice(dsTarget, iRowsT)
  dsNew <- slice(dsTarget, iRowsN)
  dsUp <- updateOutputRange(dsTarget1, dsNew, indexColumns = indexColumns) %>%
    arrange(canopyPosition, date)
  expect_equal( nrow(dsUp), nrow(dsTarget))
  dsUp %>% group_by(!!!rlang::syms(indexColumns)) %>% nest() %>%
    mutate(ans = map_lgl(data, function(dss){
      expect_true( all(diff(as.numeric(dss$date)) == 1800))
    }))
  expect_equal( dsUp$resp[3], NA_real_ )
})



test_that("updateOutputRange several groups",{
  iRowsN <- c(9,10,17:20)
  dsTarget1 <- slice(dsTarget, 1:18)
  dsNew <- slice(dsTarget, iRowsN)
  iRowsT <- setdiff(1:20, iRowsN)
  dsUp <- updateOutputRange(dsTarget1, dsNew, indexColumns = indexColumns) %>%
    arrange(canopyPosition, date)
  expect_equal( nrow(dsUp), nrow(dsTarget))
  #problems with POSIXct: expect_equal( slice(dsUp, -(3:8)), slice(dsTarget, -(3:8)))
  expect_equal( slice(dsUp, iRowsT)$date, slice(dsTarget, iRowsT)$date)
  expect_equal( select(slice(dsUp, iRowsT), -date), select(slice(dsTarget, iRowsT), -date))
  #expect_equal( slice(dsUp, (3:8)), slice(dsNew))
  expect_equal( slice(dsUp, iRowsN)$date, slice(dsNew)$date)
  expect_equal( select(slice(dsUp, iRowsN), -date), select(slice(dsNew), -date))
})

test_that("updateOutputRange additional factor level",{
  # If the new data.frame contains factors, with new levels. The target factor 
  # is re-leveled with a warning.
  #  If the new data contains new levels of the index group, then records are added.
  iRowsN <- c(8:20)
  dsTarget1 <- slice(dsTarget, 1:10) %>% droplevels()
  expect_equal(length(levels(dsTarget1$canopyPosition)), 1L)
  dsNew <- slice(dsTarget, iRowsN) %>%  droplevels()
  iRowsT <- setdiff(1:20, iRowsN)
  expect_warning(
    dsUp <- updateOutputRange(dsTarget1, dsNew, indexColumns = indexColumns) %>%
      arrange(canopyPosition, date)
  )
  expect_equal( nrow(dsUp), nrow(dsTarget))
  #problems with POSIXct: expect_equal( slice(dsUp, -(3:8)), slice(dsTarget, -(3:8)))
  expect_equal( slice(dsUp, iRowsT)$date, slice(dsTarget, iRowsT)$date)
  expect_equal( select(slice(dsUp, iRowsT), -date), select(slice(dsTarget, iRowsT), -date))
  #expect_equal( slice(dsUp, (3:8)), slice(dsNew))
  expect_equal( slice(dsUp, iRowsN)$date, slice(dsNew)$date)
  expect_equal( select(slice(dsUp, iRowsN), -date), select(slice(dsNew), -date))
})

test_that("no index column",{
  # test if it also works if no index columns are present
  # use only the first 10 records of openLand
  iRowsN <- c(8:10)
  iRowsT <- 1:9
  iRowsTKept <- setdiff(iRowsT, iRowsN)
  dsTarget1 <- slice(dsTarget, iRowsT) %>%
    select(setdiff(names(dsTarget),indexColumns))
  dsNew <- slice(dsTarget, iRowsN) %>%
    select(setdiff(names(dsTarget),indexColumns))
  dsUp <- updateOutputRange(dsTarget1, dsNew) %>%
    arrange( date)
  expect_equal( nrow(dsUp), length(iRowsTKept) + length(iRowsN))
  # kept rows corresponds to target
  #problems with POSIXct: expect_equal( slice(dsUp, -(3:8)), slice(dsTarget, -(3:8)))
  expect_equal( slice(dsUp, iRowsTKept)$date, slice(dsTarget1, iRowsTKept)$date)
  expect_equal( select(slice(dsUp, iRowsTKept), -date)
                , select(slice(dsTarget1, iRowsTKept), -date))
  # new rows correspond to new
  #expect_equal( slice(dsUp, (3:8)), slice(dsNew))
  expect_equal( slice(dsUp, iRowsN)$date, slice(dsNew)$date)
  expect_equal( select(slice(dsUp, iRowsN), -date), select(slice(dsNew), -date))
})

test_that("bind distant timestamps",{
  skip("TODO")
})



