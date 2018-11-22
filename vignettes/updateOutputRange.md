<!--
rmarkdown::render("vignettes/updateOututRange.Rmd", output_format = "md_document")
-->
Updating parts in an equidistant grouped data.frame
===================================================

Example setup
-------------

In order to demonstrate updating, we setup some example data.

The timestamp is equidistant by 30 minutes within each group. There are
two groups: canopyPosition (10 records each) and treatment (only one
level). There are two fake data columns: resp and original row number.

    require(equidIO, quietly = TRUE)
    require(dplyr, quietly = TRUE)
    dsTarget1 <- tibble(
      date = seq(ISOdatetime(2010,1,1,0,0,30, tz = "UTC"), by = "30 min", length.out = 10)
      , resp = as.numeric(1:10)
      , origRow = 1:10
      , canopyPosition = "openLand"
      , treatment = "Nadd"
    )
    dsTarget2 <- dsTarget1 %>% mutate(
      canopyPosition = "treeCovered"
      , origRow = 1:10 + nrow(dsTarget1)
    )
    indexColumns <- c("canopyPosition","treatment")
    dsTarget <- rbind( dsTarget1, dsTarget2) %>%
      arrange(canopyPosition, date) %>%
      mutate(canopyPosition = factor(canopyPosition)) %>% 
      group_by_at(vars(indexColumns))
    dsTarget

    ## # A tibble: 20 x 5
    ## # Groups:   canopyPosition, treatment [2]
    ##    date                 resp origRow canopyPosition treatment
    ##    <dttm>              <dbl>   <int> <fct>          <chr>    
    ##  1 2010-01-01 00:00:30    1.       1 openLand       Nadd     
    ##  2 2010-01-01 00:30:30    2.       2 openLand       Nadd     
    ##  3 2010-01-01 01:00:30    3.       3 openLand       Nadd     
    ##  4 2010-01-01 01:30:30    4.       4 openLand       Nadd     
    ##  5 2010-01-01 02:00:30    5.       5 openLand       Nadd     
    ##  6 2010-01-01 02:30:30    6.       6 openLand       Nadd     
    ##  7 2010-01-01 03:00:30    7.       7 openLand       Nadd     
    ##  8 2010-01-01 03:30:30    8.       8 openLand       Nadd     
    ##  9 2010-01-01 04:00:30    9.       9 openLand       Nadd     
    ## 10 2010-01-01 04:30:30   10.      10 openLand       Nadd     
    ## 11 2010-01-01 00:00:30    1.      11 treeCovered    Nadd     
    ## 12 2010-01-01 00:30:30    2.      12 treeCovered    Nadd     
    ## 13 2010-01-01 01:00:30    3.      13 treeCovered    Nadd     
    ## 14 2010-01-01 01:30:30    4.      14 treeCovered    Nadd     
    ## 15 2010-01-01 02:00:30    5.      15 treeCovered    Nadd     
    ## 16 2010-01-01 02:30:30    6.      16 treeCovered    Nadd     
    ## 17 2010-01-01 03:00:30    7.      17 treeCovered    Nadd     
    ## 18 2010-01-01 03:30:30    8.      18 treeCovered    Nadd     
    ## 19 2010-01-01 04:00:30    9.      19 treeCovered    Nadd     
    ## 20 2010-01-01 04:30:30   10.      20 treeCovered    Nadd

Updating target by new data
---------------------------

Now we have some new data `dsNew` and want to replace the corresponding
rows `dsTarget`. The indexColumns by default correspond to the grouping
variables in the new data.

    dsNew <- dsNew0 <- ungroup(dsTarget) %>%  #ungroup for slicing across groups
      slice(3:8) %>%
      mutate(resp = resp + 1000) %>% 
      group_by_at(vars(group_vars(dsTarget))) # restore grouping
    dsNew

    ## # A tibble: 6 x 5
    ## # Groups:   canopyPosition, treatment [1]
    ##   date                 resp origRow canopyPosition treatment
    ##   <dttm>              <dbl>   <int> <fct>          <chr>    
    ## 1 2010-01-01 01:00:30 1003.       3 openLand       Nadd     
    ## 2 2010-01-01 01:30:30 1004.       4 openLand       Nadd     
    ## 3 2010-01-01 02:00:30 1005.       5 openLand       Nadd     
    ## 4 2010-01-01 02:30:30 1006.       6 openLand       Nadd     
    ## 5 2010-01-01 03:00:30 1007.       7 openLand       Nadd     
    ## 6 2010-01-01 03:30:30 1008.       8 openLand       Nadd

    dsUp <- updateOutputRange(dsTarget, dsNew) %>%
      arrange(canopyPosition, date)
    dsUp %>% ungroup() %>% slice(2:12)

    ## # A tibble: 11 x 5
    ##    date                 resp origRow canopyPosition treatment
    ##    <dttm>              <dbl>   <int> <fct>          <chr>    
    ##  1 2010-01-01 00:30:30    2.       2 openLand       Nadd     
    ##  2 2010-01-01 01:00:30 1003.       3 openLand       Nadd     
    ##  3 2010-01-01 01:30:30 1004.       4 openLand       Nadd     
    ##  4 2010-01-01 02:00:30 1005.       5 openLand       Nadd     
    ##  5 2010-01-01 02:30:30 1006.       6 openLand       Nadd     
    ##  6 2010-01-01 03:00:30 1007.       7 openLand       Nadd     
    ##  7 2010-01-01 03:30:30 1008.       8 openLand       Nadd     
    ##  8 2010-01-01 04:00:30    9.       9 openLand       Nadd     
    ##  9 2010-01-01 04:30:30   10.      10 openLand       Nadd     
    ## 10 2010-01-01 00:00:30    1.      11 treeCovered    Nadd     
    ## 11 2010-01-01 00:30:30    2.      12 treeCovered    Nadd

From the respiration column it can be seen that the corresponding rows
have been updated by an amount of +1000. The rows of the other groups
have not been modified.

Ensuring equidistant records
----------------------------

If there are additional rows, they are appended.

Moreover, if there are missing records between the times of the new data
and the target, those rows are added with missing values.

In the following example, rows 17:20 of the original target are added to
a data.frame that only consists of rows 1:14. The updated data has
additional rows 15 and 16 with missing values.

    iRowsT <- 1:14
    iRowsN <- 17:20
    dsTarget1 <- slice(ungroup(dsTarget), iRowsT)
    dsNew <- slice(ungroup(dsTarget), iRowsN) %>% mutate(resp = resp + 1000)
    dsUp <- updateOutputRange(dsTarget1, dsNew, indexColumns = indexColumns) %>%
      arrange(canopyPosition, date)
    dsUp %>% ungroup() %>% slice(14:18)

    ## # A tibble: 5 x 5
    ##   date                 resp origRow canopyPosition treatment
    ##   <dttm>              <dbl>   <int> <fct>          <chr>    
    ## 1 2010-01-01 01:30:30    4.      14 treeCovered    Nadd     
    ## 2 2010-01-01 02:00:30   NA       NA treeCovered    Nadd     
    ## 3 2010-01-01 02:30:30   NA       NA treeCovered    Nadd     
    ## 4 2010-01-01 03:00:30 1007.      17 treeCovered    Nadd     
    ## 5 2010-01-01 03:30:30 1008.      18 treeCovered    Nadd

Adding missing factor levels
----------------------------

If the new data.frame contains factors, with new levels. The target
factor is re-leveled with a warning.

If the new data contains new levels of the index group, then records are
added.

    iRowsN <- c(8:20)
    # drop canopyPosition level "treeCovered"
    dsTarget1 <- slice(ungroup(dsTarget), 1:10) %>% droplevels() 
    dsNew <- slice(ungroup(dsTarget), iRowsN) %>%  
      mutate(resp = resp + 1000) %>% 
      droplevels()
    dsUp <- updateOutputRange(dsTarget1, dsNew, indexColumns = indexColumns) %>%
      arrange(canopyPosition, date)

    ## Warning in expandAllInconsistentFactorLevels(list(dsTarget, dsNew),
    ## noWarningCols = ".group"): releveling factors canopyPosition

    dsUp %>% ungroup() %>% slice(6:14)

    ## # A tibble: 9 x 5
    ##   date                 resp origRow canopyPosition treatment
    ##   <dttm>              <dbl>   <int> <fct>          <chr>    
    ## 1 2010-01-01 02:30:30    6.       6 openLand       Nadd     
    ## 2 2010-01-01 03:00:30    7.       7 openLand       Nadd     
    ## 3 2010-01-01 03:30:30 1008.       8 openLand       Nadd     
    ## 4 2010-01-01 04:00:30 1009.       9 openLand       Nadd     
    ## 5 2010-01-01 04:30:30 1010.      10 openLand       Nadd     
    ## 6 2010-01-01 00:00:30 1001.      11 treeCovered    Nadd     
    ## 7 2010-01-01 00:30:30 1002.      12 treeCovered    Nadd     
    ## 8 2010-01-01 01:00:30 1003.      13 treeCovered    Nadd     
    ## 9 2010-01-01 01:30:30 1004.      14 treeCovered    Nadd

Updating data in a file
-----------------------

Convenience, function `updateRData` executes `updateOutputRange` to a
data.frame stored in .RData format, as written by `save`.

    fileName <- file.path(tempdir(),"updateRDataExample.RData")
    save(dsTarget, file = fileName)
    updateRData(dsNew0, fileName)

    ## updated updateRDataExample in file /tmp/Rtmp1c8Wlr/updateRDataExample.RData

    load(fileName)
    updateRDataExample %>% 
      arrange(canopyPosition, date) %>% ungroup() %>% slice(2:12)

    ## # A tibble: 11 x 5
    ##    date                 resp origRow canopyPosition treatment
    ##    <dttm>              <dbl>   <int> <fct>          <chr>    
    ##  1 2010-01-01 00:30:30    2.       2 openLand       Nadd     
    ##  2 2010-01-01 01:00:30 1003.       3 openLand       Nadd     
    ##  3 2010-01-01 01:30:30 1004.       4 openLand       Nadd     
    ##  4 2010-01-01 02:00:30 1005.       5 openLand       Nadd     
    ##  5 2010-01-01 02:30:30 1006.       6 openLand       Nadd     
    ##  6 2010-01-01 03:00:30 1007.       7 openLand       Nadd     
    ##  7 2010-01-01 03:30:30 1008.       8 openLand       Nadd     
    ##  8 2010-01-01 04:00:30    9.       9 openLand       Nadd     
    ##  9 2010-01-01 04:30:30   10.      10 openLand       Nadd     
    ## 10 2010-01-01 00:00:30    1.      11 treeCovered    Nadd     
    ## 11 2010-01-01 00:30:30    2.      12 treeCovered    Nadd
