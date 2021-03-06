<!--
rmarkdown::render("vignettes/readEquidistantCsv.Rmd", output_format = "md_document")
-->
Reading parts of a logger file with equidistant data
====================================================

Example setup
-------------

In order to demonstrate reading, we setup some example data and write it
to a file `testData.csv` located in temporary directory.

The example data contains two columns that describe the time stamp, and
one data column, here just reporting the minute after start.

    require(equidIO, quietly = TRUE)
    require(tibble, quietly = TRUE)
    require(dplyr, quietly = TRUE)
    require(readr, quietly = TRUE)
    tmpDir <- tempdir()
    timezone = "Etc/GMT-1"
    testData <- tribble(
      ~Date, ~Time, ~minute
      ,"2018-10-03", "06:00", 0
      ,"2018-10-03", "06:15", 15
      ,"2018-10-03", "06:30", 30
      ,"2018-10-03", "06:45", 45
      ,"2018-10-03", "07:00", 60
      ,"2018-10-03", "07:15", 75
    )  %>% 
      mutate( timestamp = as.POSIXct(
       paste(Date, Time), format = "%Y-%m-%d %H:%M", tz = timezone))
    tmpFile <- file.path(tmpDir, "testData.csv")
    write_csv(select(testData, timestamp, everything()), tmpFile)

Reading
-------

Without further arguments, `readEquidistantCsv` reads the entire file.

    (datAll <- readEquidistantCsv(tmpFile, timezone = timezone))

    ## # A tibble: 6 x 4
    ##   timestamp           Date       Time     minute
    ##   <dttm>              <date>     <time>    <int>
    ## 1 2018-10-03 06:00:00 2018-10-03 06:00:00      0
    ## 2 2018-10-03 06:15:00 2018-10-03 06:15:00     15
    ## 3 2018-10-03 06:30:00 2018-10-03 06:30:00     30
    ## 4 2018-10-03 06:45:00 2018-10-03 06:45:00     45
    ## 5 2018-10-03 07:00:00 2018-10-03 07:00:00     60
    ## 6 2018-10-03 07:15:00 2018-10-03 07:15:00     75

Note, that `write_csv` and `read_csv` by default use timezone UTC when
parsing timestamps. `readEquidistantCsv` allows to specify the timezone
for the timestamp column with the `timezone` argument.

Specify start and end times
---------------------------

We can specify a start and/or and time to read only parts. The
corresponding line numbers are computed, and only the relevant part of
the file is parsed. This can be much faster than reading the entire file
and then filter for the corresponding times.

    (datPart <- readEquidistantCsv(
      tmpFile, timezone = timezone
      , startTime = ISOdatetime(2018,10,3,6,30,0, tz = timezone)
      , endTime = ISOdatetime(2018,10,3,6,45,0, tz = timezone)))

    ## # A tibble: 2 x 4
    ##   timestamp           Date       Time     minute
    ##   <dttm>              <date>     <time>    <int>
    ## 1 2018-10-03 06:30:00 2018-10-03 06:30:00     30
    ## 2 2018-10-03 06:45:00 2018-10-03 06:45:00     45

Make sure to specify those times with the same time zone.

Start times before the dataset and end times after the dataset are
accepted. This corresponds to omitting the constraint. Otherwise, the
times must match a timestamp of the equidistant dataset.

If the dataset is not truly equidistant, i.e. due to missing timesteps,
the start and end row numbers cannot be comupted and
`readEquidistantCsv` will fail with an error messsage.

Customize reading times
-----------------------

The default of `readEquidistantCsv` assumes column `timestamp` to be
present to be formatted in a way so that it is properly recognized and
parsed as timestamp by `read_csv`.

However, often the file to read holds the time in a different format. We
can supply a function with argument `fCreateTimestamp` that creates the
timestamp column in a customized way.

The next example creates the timestamp from the information in the Date
and Time column.

    stampByDateAndTime <- function(
      ### add column timestamp from columns date and time
      data  ##<< data.frame to mutate
      , timezone ##<< ignored here
    ){
      data %>%  mutate( timestamp = as.POSIXct(
        paste(Date, Time), format = "%Y-%m-%d %H:%M", tz = "Etc/GMT-1")) 
    }
    (ansCust <- readEquidistantCsv(tmpFile, fCreateTimestamp = stampByDateAndTime))

    ## # A tibble: 6 x 4
    ##   timestamp           Date       Time     minute
    ##   <dttm>              <date>     <time>    <int>
    ## 1 2018-10-03 06:00:00 2018-10-03 06:00:00      0
    ## 2 2018-10-03 06:15:00 2018-10-03 06:15:00     15
    ## 3 2018-10-03 06:30:00 2018-10-03 06:30:00     30
    ## 4 2018-10-03 06:45:00 2018-10-03 06:45:00     45
    ## 5 2018-10-03 07:00:00 2018-10-03 07:00:00     60
    ## 6 2018-10-03 07:15:00 2018-10-03 07:15:00     75

Note, that this custom function is also reponsible to set the propor
time zone of the timestamp column. If it uses the passed-down timezone,
it should handle zero-length values.

Read meta-information
---------------------

Sometimes, logger file report units or other meta information in rows
after the column names. This poses difficulties for `read_csv`, but can
be taken care of by `readEquidistantCsv`.

First we generate an example file that has three freetext lines before
the column names, and two lines of meta information after the colum
names.

    lines <- read_lines(tmpFile)
    linesWithHeader <- c(
      " Some Header information", "across", "three lines"
      , lines[1]
      , "-,-,-,min"
      , "number,number,number,number"
      , lines[-1]
    )
    tmpHeaderFile <- file.path(tmpDir, "testDataHeader.csv")
    write_lines(linesWithHeader, tmpHeaderFile)

Next, we read this file. We must supply the number of lines before and
after the columns names.

    (ansMeta <- readEquidistantCsv(
      tmpHeaderFile, timezone = timezone
      , nRowsHeader = 3, nRowsColumnHeader = 2
    ))

    ## # A tibble: 6 x 4
    ##   timestamp           Date       Time     minute
    ##   <dttm>              <date>     <time>    <int>
    ## 1 2018-10-03 06:00:00 2018-10-03 06:00:00      0
    ## 2 2018-10-03 06:15:00 2018-10-03 06:15:00     15
    ## 3 2018-10-03 06:30:00 2018-10-03 06:30:00     30
    ## 4 2018-10-03 06:45:00 2018-10-03 06:45:00     45
    ## 5 2018-10-03 07:00:00 2018-10-03 07:00:00     60
    ## 6 2018-10-03 07:15:00 2018-10-03 07:15:00     75

The additional information is stored in two attributes of the result.

    attr(ansMeta, "header")

    ## [1] " Some Header information" "across"                  
    ## [3] "three lines"

    attr(ansMeta, "columnHeader")

    ## # A tibble: 2 x 4
    ##   timestamp Date   Time   minute
    ##   <chr>     <chr>  <chr>  <chr> 
    ## 1 -         -      -      min   
    ## 2 number    number number number
