Project 1 - Vignette for Reading and Summarizing Data from the NHL API
================
Lucy Yin
6/20/2021

-   [Introduction](#introduction)
-   [Install and load required
    packages](#install-and-load-required-packages)
-   [Write functions to contact the NHL records and stats
    API](#write-functions-to-contact-the-nhl-records-and-stats-api)
    -   [NHL records API](#nhl-records-api)
        -   [Try out NHL records API](#try-out-nhl-records-api)
    -   [NHL stats API](#nhl-stats-api)
        -   [Try out NHL stats API](#try-out-nhl-stats-api)
    -   [Wrapper Function](#wrapper-function)
        -   [Try out wrapper function](#try-out-wrapper-function)
-   [Exporatory Data Analysis](#exporatory-data-analysis)
    -   [Grabbing data and combine](#grabbing-data-and-combine)
    -   [Create new variables](#create-new-variables)
    -   [Read data from 2 different endpoints and
        combine](#read-data-from-2-different-endpoints-and-combine)
    -   [Contingency Tables](#contingency-tables)
    -   [Numerical Summaries for Categorical
        Variables](#numerical-summaries-for-categorical-variables)
    -   [Plots](#plots)
        -   [Bar plot](#bar-plot)
        -   [Histogram](#histogram)
        -   [Box Plot](#box-plot)
        -   [Scatter Plot](#scatter-plot)
        -   [Scatter Plot using Group](#scatter-plot-using-group)
-   [Outro](#outro)

# Introduction

This vignette introduces my way for reading and summarizing data from
the National Hockey League’s (NHL) API. To access the NHL Stats API
Documentation, please visit this [GitLab
page](https://gitlab.com/dword4/nhlapi/-/blob/master/stats-api.md).

# Install and load required packages

First we need to install and load in the needed packages. To install the
packages, simply do `install.packages("name of package")`. After
successful installation, read in the following packages.

``` r
library(httr)
library(jsonlite)
library(tidyverse)
library(knitr)
library(haven)
library(qwraps2)
library(rmarkdown)
library(data.table)
library(kableExtra)
library(xml2)
```

# Write functions to contact the NHL records and stats API

## NHL records API

We will need to contact the records and stats API for this vignette.
First is the NHL records API. The different endpoints we will
incorporate include:

-   franchise  
-   franchise-team-totals  
-   franchise-season-records  
-   franchise-goalie-records  
-   franchise-skater records  
-   franchise-detail

The function `get.records` below contacts the NHL records API and
returns well-formatted, parsed data in tibble format. Users have the
option to specify the franchise of choice by team’s short name (such as
Hurricanes, Eagles, etc.) or by the team ID number. To make this
possible, we have to first map each franchise’s team short name to its
team ID number.

``` r
# map id, team names and most recent id
franchise.url <- GET("https://records.nhl.com/site/api/franchise")
franchise.txt <- content(franchise.url, "text", encoding = "UTF-8")
franchise.json <- fromJSON(franchise.txt, flatten=TRUE)
franchise.list <- franchise.json$data %>% as.data.frame()
franchise.tbl <- tibble(franchise.list$id, franchise.list$teamCommonName, franchise.list$fullName, franchise.list$mostRecentTeamId)
franchise.tbl
```

    ## # A tibble: 39 x 4
    ##    `franchise.list$id` `franchise.list$teamCommonName` `franchise.list$fullName` `franchise.list$mostRecentTeamId`
    ##                  <int> <chr>                           <chr>                                                 <int>
    ##  1                   1 Canadiens                       Montréal Canadiens                                        8
    ##  2                   2 Wanderers                       Montreal Wanderers                                       41
    ##  3                   3 Eagles                          St. Louis Eagles                                         45
    ##  4                   4 Tigers                          Hamilton Tigers                                          37
    ##  5                   5 Maple Leafs                     Toronto Maple Leafs                                      10
    ##  6                   6 Bruins                          Boston Bruins                                             6
    ##  7                   7 Maroons                         Montreal Maroons                                         43
    ##  8                   8 Americans                       Brooklyn Americans                                       51
    ##  9                   9 Quakers                         Philadelphia Quakers                                     39
    ## 10                  10 Rangers                         New York Rangers                                          3
    ## # … with 29 more rows

``` r
# pull out the common name vector (index can serve as the id)
name.id.list <- franchise.tbl$`franchise.list$teamCommonName`
# pull out the most recent id vector (index can serve as the id)
id.most.recent.id.list <- franchise.tbl$`franchise.list$mostRecentTeamId`
```

``` r
get.records <- function(table.name, id=NULL, team.common.name=NULL, ...) {
  base.url <- "https://records.nhl.com/site/api"
  
  ## table name is given
  if (!is.null(table.name)) {
    
    ## check if user input the team.common.name, if yes then convert it to the corresponding id
    if (!is.null(team.common.name)) {
      for (i in 1:length(name.id.list)) {
        if (team.common.name == name.id.list[i]) {
          id <- (i)
        }
      }
    }
    
    ## change user inputted id to most.recent.id to be used for the franchise-details table
    if (is.numeric(id)) {
      for (i in 1:length(id.most.recent.id.list)) {
        if (i == id) {
          most.recent.id <- id.most.recent.id.list[i]
        }
      }
    } 

    ## table name is this one and yes id
    if (table.name %in% ("franchise") & (is.numeric(id))) {
      return ("The table selected cannot be returned with the specified ID or team common name input.")
    }  
    
    ## yes table name, no id
    else if (is.null(id)) {
      full.url <- paste0(base.url, "/", table.name)
      
      nfl.records <- GET(full.url)
      nfl.records.txt <- content(nfl.records, "text", encoding = "UTF-8")  ## convert to JSON text form
      nfl.records.json <- fromJSON(nfl.records.txt, flatten=TRUE)  ## convert to list
      return (nfl.records.json$data %>% as_tibble())
    }
    
    ## table name is one of the 4, yes id
    else if (table.name %in% c("franchise-team-totals",
                               "franchise-season-records",
                               "franchise-goalie-records",
                               "franchise-skater-records") & 
             (is.numeric(id))) {
      full.url <- paste0(base.url, "/", table.name, "?cayenneExp=franchiseId=", id)
      nfl.records <- GET(full.url)
      nfl.records.txt <- content(nfl.records, "text", encoding = "UTF-8")  ## convert to JSON text form
      nfl.records.json <- fromJSON(nfl.records.txt, flatten=TRUE)  ## convert to list
      return (nfl.records.json$data %>% as_tibble())
    }
    
    ## table name is this, id is one of the 8 (because these results don't return html content)
    else if (table.name %in% ("franchise-detail") & 
             (id %in% c(2:4, 7:9, 13, 39))) {
      nfl.records.details <- GET(paste0(base.url, "/", table.name, "?cayenneExp=mostRecentTeamId=", most.recent.id))
      nfl.records.details.txt <- content(nfl.records.details, "text", encoding = "UTF-8")  ## convert to JSON text form
      nfl.records.details.json <- fromJSON(nfl.records.details.txt, flatten=TRUE)  ## convert to list  
      return (nfl.records.details.json$data %>% as_tibble())
    }
    
    ## table name is this, id is one of the 31 (these tibbles contain html content, further parse out html)
    else if (table.name %in% ("franchise-detail") & 
             (is.numeric(id) %in% c(1, 5 ,6, 10:12, 14:38))) {
      nfl.records.details <- GET(paste0(base.url, "/", table.name, "?cayenneExp=mostRecentTeamId=", most.recent.id))
      nfl.records.details.txt <- content(nfl.records.details, "text", encoding = "UTF-8")  ## convert to JSON text form
      nfl.records.details.json <- fromJSON(nfl.records.details.txt, flatten=TRUE)  ## convert to list
      fran.details <- nfl.records.details.json$data
      
      ## these columns contain html content, save as separate vectors
      txt.string1 <- fran.details$captainHistory
      txt.string2 <- fran.details$coachingHistory
      txt.string3 <- fran.details$generalManagerHistory
      txt.string4 <- fran.details$retiredNumbersSummary
      
      ## delete the original columns of html content
      fran.details <- fran.details %>% select(-c(3,4,8,11))

      ## use gsub function to remove all enter breaks from our character string 
      txt.string1.1 <- gsub("[\r\n\t]", "", txt.string1)
      txt.string2.1 <- gsub("[\r\n\t]", "", txt.string2)
      txt.string3.1 <- gsub("[\r\n\t]", "", txt.string3)
      txt.string4.1 <- gsub("[\r\n\t]", "", txt.string4)
      
      ## add parsed html content back into table
      fran.details$captainHistory <- read_html(txt.string1.1) %>% xml_text()
      fran.details$coachingHistory <- read_html(txt.string2.1) %>% xml_text()
      fran.details$generalManagerHistory <- read_html(txt.string3.1) %>% xml_text()
      fran.details$retiredNumbersSummary <- read_html(txt.string4.1) %>% xml_text()
      return (fran.details)
    }
  }
  
  ## table name not given
  else {
    return ("Must provide a table name to get results.")
  }
}
```

### Try out NHL records API

We can test out the `get.records` function with a few different
combination of inputs to make sure the function works.

``` r
get.records("franchise-goalie-records")
```

    ## # A tibble: 1,078 x 29
    ##       id activePlayer firstName franchiseId franchiseName  gameTypeId gamesPlayed lastName losses mostGoalsAgainstDa… mostGoalsAgainst…
    ##    <int> <lgl>        <chr>           <int> <chr>               <int>       <int> <chr>     <int> <chr>                           <int>
    ##  1   235 FALSE        Don                15 Dallas Stars            2         315 Beaupre     125 1983-10-07                         10
    ##  2   236 FALSE        Bob                28 Arizona Coyot…          2         281 Essensa     114 1992-12-11, 1992-1…                 8
    ##  3   237 FALSE        Tony               11 Chicago Black…          2         873 Esposito    302 1983-10-15, 1980-1…                10
    ##  4   238 FALSE        Grant              25 Edmonton Oile…          2         423 Fuhr        117 1984-02-05, 1982-1…                 9
    ##  5   239 FALSE        Ron                16 Philadelphia …          2         489 Hextall     172 1987-04-05                          9
    ##  6   240 FALSE        Curtis             18 St. Louis Blu…          2         280 Joseph       96 1992-11-25, 1990-0…                 8
    ##  7   241 FALSE        Olie               24 Washington Ca…          2         711 Kolzig      293 2006-01-25, 2005-1…                 8
    ##  8   242 FALSE        Mike               18 St. Louis Blu…          2         347 Liut        133 1982-02-25                          9
    ##  9   243 FALSE        Kirk               20 Vancouver Can…          2         516 McLean      228 1996-10-19                          9
    ## 10   244 FALSE        Gilles             13 Cleveland Bar…          2         250 Meloche     140 1973-10-21                         11
    ## # … with 1,068 more rows, and 18 more variables: mostSavesDates <chr>, mostSavesOneGame <int>, mostShotsAgainstDates <chr>,
    ## #   mostShotsAgainstOneGame <int>, mostShutoutsOneSeason <int>, mostShutoutsSeasonIds <chr>, mostWinsOneSeason <int>,
    ## #   mostWinsSeasonIds <chr>, overtimeLosses <int>, playerId <int>, positionCode <chr>, rookieGamesPlayed <int>, rookieShutouts <int>,
    ## #   rookieWins <int>, seasons <int>, shutouts <int>, ties <int>, wins <int>

``` r
get.records("franchise-goalie-records", team.common.name = "Eagles")
```

    ## # A tibble: 4 x 29
    ##      id activePlayer firstName franchiseId franchiseName   gameTypeId gamesPlayed lastName  losses mostGoalsAgainst… mostGoalsAgainstO…
    ##   <int> <lgl>        <chr>           <int> <chr>                <int>       <int> <chr>      <int> <chr>                          <int>
    ## 1  1231 FALSE        Clint               3 St. Louis Eagl…          2         158 Benedict      58 1917-12-22                        11
    ## 2   538 FALSE        Bill                3 St. Louis Eagl…          2          90 Beveridge     56 1934-12-13                        11
    ## 3   248 FALSE        Alec                3 St. Louis Eagl…          2         294 Connell      106 1925-02-11                        10
    ## 4   600 FALSE        Sammy               3 St. Louis Eagl…          2           2 Hebert         1 1924-03-01                         5
    ## # … with 18 more variables: mostSavesDates <lgl>, mostSavesOneGame <lgl>, mostShotsAgainstDates <lgl>, mostShotsAgainstOneGame <lgl>,
    ## #   mostShutoutsOneSeason <int>, mostShutoutsSeasonIds <chr>, mostWinsOneSeason <int>, mostWinsSeasonIds <chr>, overtimeLosses <lgl>,
    ## #   playerId <int>, positionCode <chr>, rookieGamesPlayed <int>, rookieShutouts <int>, rookieWins <int>, seasons <int>,
    ## #   shutouts <int>, ties <int>, wins <int>

``` r
get.records("franchise-detail")
```

    ## # A tibble: 39 x 13
    ##       id active captainHistory  coachingHistory  dateAwarded  directoryUrl firstSeasonId generalManagerH… heroImageUrl mostRecentTeamId
    ##    <int> <lgl>  <chr>           <chr>            <chr>        <chr>                <int> <chr>            <chr>                   <int>
    ##  1     1 TRUE   "<ul class=\"s… "<ul class=\"st… 1917-11-26T… https://www…      19171918 "<ul class=\"st… https://rec…                8
    ##  2     2 FALSE   <NA>            <NA>            1917-11-26T… <NA>              19171918  <NA>            https://rec…               41
    ##  3     3 FALSE   <NA>            <NA>            1917-11-26T… <NA>              19171918  <NA>            https://rec…               45
    ##  4     4 FALSE   <NA>            <NA>            1917-11-26T… <NA>              19191920  <NA>            https://rec…               37
    ##  5     5 TRUE   "<ul class=\"s… "<ul class=\"st… 1917-11-26T… https://www…      19171918 "<ul class=\"st… https://rec…               10
    ##  6     6 TRUE   "<ul class=\"s… "<ul class=\"st… 1924-11-01T… https://www…      19241925 "<ul class=\"st… https://rec…                6
    ##  7     7 FALSE   <NA>            <NA>            1924-11-01T… <NA>              19241925  <NA>            https://rec…               43
    ##  8     8 FALSE   <NA>            <NA>            1925-09-22T… <NA>              19251926  <NA>            https://rec…               51
    ##  9     9 FALSE   <NA>            <NA>            1925-11-07T… <NA>              19251926  <NA>            https://rec…               39
    ## 10    10 TRUE   "<ul class=\"s… "<ul class=\"st… 1926-05-15T… https://www…      19261927 "<ul class=\"st… https://rec…                3
    ## # … with 29 more rows, and 3 more variables: retiredNumbersSummary <chr>, teamAbbrev <chr>, teamFullName <chr>

``` r
get.records("franchise-detail", team.common.name = "Hurricanes")
```

    ##   id active         dateAwarded                              directoryUrl firstSeasonId
    ## 1 26   TRUE 1979-06-22T00:00:00 https://www.nhl.com/hurricanes/team/staff      19791980
    ##                                                            heroImageUrl mostRecentTeamId teamAbbrev        teamFullName
    ## 1 https://records.nhl.com/site/asset/public/ext/hero/Team Pages/Aho.jpg               12        CAR Carolina Hurricanes
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       captainHistory
    ## 1 Jordan Staal: 2019-20 – PresentJustin Williams: 2018-19Justin Faulk and Jordan Staal: 2017-18(No Captain): 2016-17Eric Staal: 2010-11 – 2015-16Rod Brind’Amour and Eric Staal: 2009-10Rod Brind’Amour: 2005-06 – 2008-09Ron Francis: 2000-01 – 2003-04Keith Primeau and Ron Francis: 1999-00Keith Primeau: 1998-99Kevin Dineen: 1996-97 – 1997-98Brendan Shanahan: 1995-96Pat Verbeek: 1992-93 – 1994-95Randy Ladouceur: 1991-92Ron Francis: 1985-86 – 1990-91Mark Johnson and Ron Francis: 1984-85Mark Johnson: 1983-84Russ Anderson: 1982-83Dave Keon: 1981-82Rick Ley and Mike Rogers: 1980-81Rick Ley: 1979-80
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     coachingHistory
    ## 1 Rod Brind’Amour: Oct. 4, 2018 – PresentBill Peters: Oct. 10, 2014 – April 7, 2018Kirk Muller: Nov. 29, 2011 – April 13, 2014Paul Maurice: Dec. 4, 2008 – Nov. 27, 2011Peter Laviolette: Dec. 18, 2003 – Nov. 30, 2008Paul Maurice: Nov. 7, 1995 – Dec. 14, 2003Paul Holmgren: Jan. 21 – Nov. 5, 1995Pierre McGuire: Nov. 17, 1993 – April 14, 1994Paul Holmgren: Oct. 6, 1992 – Nov. 13, 1993Jim Roberts:  Oct. 5, 1991 – May 1, 1992Rick Ley: Oct. 5, 1989 – April 13, 1991Larry Pleau: Feb. 7, 1988 – April 9, 1989Jack Evans: Oct. 5, 1983 – Feb. 6, 1988John Cunniff: March 8 – April 3, 1983Larry Pleau: Jan. 27 – March 6, 1983Larry Kish: Oct. 6, 1982 – Jan. 23, 1983Larry Pleau: Feb. 22, 1981 – April 4, 1982Don Blackburn: Oct. 11, 1979 – Feb. 19, 1981* Date range indicates first and last games coached during tenure (regular season or playoffs)
    ##                                                                                                                                                                                                                                                                                                                                                                                                                      generalManagerHistory
    ## 1 Don Waddell: May 8, 2018 – PresentRon Francis: April 28, 2014 – March 7, 2018Jim Rutherford: June 28, 1994 – April 28, 2014Paul Holmgren: Sept. 8, 1993 – June 28, 1994Brian Burke: May 26, 1992 – Sept. 1, 1993Eddie Johnston: May 11, 1989 – May 12, 1992Emile Francis: May 2, 1983 – May 11, 1989Larry Pleau: April 2, 1981 – May 2, 1983Jack Kelley: May 6, 1977 – April 2, 1981* Date range indicates first and last days of tenure
    ##                                                                          retiredNumbersSummary
    ## 1 2 – Glen Wesley (1994-08)10 – Ron Francis (1981-91, 1998-04)17 – Rod Brind’Amour (2000-10)  

``` r
get.records("franchise-detail", id=12)
```

    ##   id active         dateAwarded                                          directoryUrl firstSeasonId
    ## 1 12   TRUE 1926-09-25T00:00:00 https://www.nhl.com/redwings/team/business-operations      19261927
    ##                                                                     heroImageUrl mostRecentTeamId teamAbbrev      teamFullName
    ## 1 https://records.nhl.com/site/asset/public/ext/hero/Team Pages/DET/WingsWin.jpg               17        DET Detroit Red Wings
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           captainHistory
    ## 1 Dylan Larkin: 2020-21 – Present(No Captain): 2018-19 – 2019-20Henrik Zetterberg: 2012-13 – 2017-18Nicklas Lidstrom: 2006-07 – 2011-12Steve Yzerman: 1986-87 – 2005-06Danny Gare: 1982-83 – 1985-86Reed Larson: 1981-82Errol Thompson and Reed Larson: 1980-81Dale McCourt: 1979-80Dennis Hextall, Nick Libett and Paul Woods: 1978-79Dan Maloney and Dennis Hextall: 1977-78Danny Grant and Dennis Polonich: 1976-77Danny Grant and Terry Harper: 1975-76Marcel Dionne: 1974-75Alex Delvecchio, Nick Libett, Red Berenson, Gary Bergman, Ted Harris, Mickey Redmond and Larry Johnston: 1973-74Alex Delvecchio: 1962-63 – 1972-73Gordie Howe: 1958-59 – 1961-62Red Kelly: 1956-57 – 1957-58Ted Lindsay: 1952-53 – 1955-56Sid Abel: 1946-47 – 1951-52Flash Hollett and Sid Abel: 1945-46Flash Hollett: 1944-45Mud Bruneteau and Flash Hollett: 1943-44Sid Abel: 1942-43Ebbie Goodfellow and Syd Howe: 1941-42Ebbie Goodfellow: 1938-39 – 1940-41Doug Young: 1935-36 – 1937-38Ebbie Goodfellow: 1934-35Herbie Lewis: 1933-34Larry Aurie: 1932-33Carson Cooper: 1931-32George Hay: 1930-31Reg Noble: 1927-28 – 1929-30Art Duncan: 1926-27
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                coachingHistory
    ## 1 Jeff Blashill: Oct. 9, 2015 – PresentMike Babcock: Oct. 5, 2005 – April 29, 2015Dave Lewis: Oct. 10, 2002 – May 3, 2004Scotty Bowman: Oct. 23, 1998 – June 13, 2002Dave Lewis and Barry Smith (Co-Coaches): Oct. 10-21, 1998Scotty Bowman: Oct. 5, 1993 – June 16, 1998Bryan Murray: Oct. 4, 1990 – May 1, 1993Jacques Demers: Oct. 9, 1986 – April 1, 1990Brad Park: Dec. 31, 1985 – April 6, 1986Harry Neale: Oct 10 – Dec. 29, 1985Nick Polano: Oct. 6, 1982 – April 13, 1985Billy Dea: March 11 – April 4, 1982Wayne Maxner: Nov. 26, 1980 – March 8, 1982Ted Lindsay: March 21 – Nov. 22, 1980Bobby Kromm: Oct. 13, 1977 – March 19, 1980Larry Wilson: Jan. 20 – April 3, 1977Alex Delvecchio: Dec. 5, 1975 – Jan. 15, 1977Doug Barkley: Oct. 8 – Dec. 3, 1975Alex Delvecchio: Nov. 7, 1973 – April 6, 1975Ted Garvin: Oct. 10 – Nov. 4, 1973Johnny Wilson: Nov. 1, 1971 – April 1, 1973Doug Barkley: Jan. 9 – Oct. 31, 1971Ned Harkness: Oct. 10, 1970 – Jan. 7, 1971Sid Abel: Oct. 16, 1969 – April 12, 1970Bill Gadsby: Oct. 11, 1968 – Oct. 15, 1969Sid Abel: Jan. 4, 1958 – March 31, 1968Jimmy Skinner: Oct. 7, 1954 – Jan. 1, 1958Tommy Ivan: Oct. 15, 1947 – April 16, 1954Jack Adams: Nov. 15, 1927 – April 5, 1947Duke Keats: Feb. 24 – March 26, 1927Art Duncan: Nov. 18, 1926 – Feb. 22, 1927* Date range indicates first and last games coached during tenure (regular season or playoffs)
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 generalManagerHistory
    ## 1 Steve Yzerman: April 19, 2019 – PresentKen Holland: July 18, 1997 – April 19, 2019Jim Devellano: June 3, 1994 – July 18, 1997Bryan Murray: July 13, 1990 – June 3, 1994Jim Devellano: July 12, 1982 – July 11, 1990Jimmy Skinner: April 11, 1980 – July 12, 1982Ted Lindsay: March 16, 1977 – April 11, 1980Alex Delvecchio: May 21, 1974 – March 16, 1977Jimmy Skinner: Feb. 6 – May 21, 1974Ned Harkness: Jan. 8, 1971 – Feb. 6, 1974Sid Abel: April 26, 1962 – Jan. 6, 1971Jack Adams: May 14, 1927 – April 26, 1962Art Duncan: Oct. 18, 1926 – May 14, 1927* Date range indicates first and last days of tenure
    ##                                                                                                                                                                                                                                     retiredNumbersSummary
    ## 1 1 – Terry Sawchuk (1949-55, 1957-64, 1968-69)4 – Red Kelly (1947-60)5 – Nicklas Lidstrom (1991-12)7 – Ted Lindsay (1947-57, 1964-65)9 – Gordie Howe (1946-71)10 – Alex Delvecchio (1951-73)12 – Sid Abel (1938-43, 1945-52)19 – Steve Yzerman (1983-06)

``` r
get.records("franchise-team-totals", 26)
```

    ## # A tibble: 4 x 30
    ##      id activeFranchise firstSeasonId franchiseId gameTypeId gamesPlayed goalsAgainst goalsFor homeLosses homeOvertimeLosses homeTies
    ##   <int>           <int>         <int>       <int>      <int>       <int>        <int>    <int>      <int>              <int>    <int>
    ## 1    24               1      19971998          26          2        1812         5140     4914        323                 77       52
    ## 2    67               1      19791980          26          2        1420         5345     4704        297                 NA       95
    ## 3    23               1      19971998          26          3         112          282      272         24                  0       NA
    ## 4    68               1      19791980          26          3          49          177      143         10                 NA       NA
    ## # … with 19 more variables: homeWins <int>, lastSeasonId <int>, losses <int>, overtimeLosses <int>, penaltyMinutes <int>,
    ## #   pointPctg <dbl>, points <int>, roadLosses <int>, roadOvertimeLosses <int>, roadTies <int>, roadWins <int>, shootoutLosses <int>,
    ## #   shootoutWins <int>, shutouts <int>, teamId <int>, teamName <chr>, ties <int>, triCode <chr>, wins <int>

``` r
get.records("franchise", id=23)
```

    ## [1] "The table selected cannot be returned with the specified ID or team common name input."

``` r
get.records("franchise")
```

    ## # A tibble: 39 x 8
    ##       id firstSeasonId fullName             lastSeasonId mostRecentTeamId teamAbbrev teamCommonName teamPlaceName
    ##    <int>         <int> <chr>                       <int>            <int> <chr>      <chr>          <chr>        
    ##  1     1      19171918 Montréal Canadiens             NA                8 MTL        Canadiens      Montréal     
    ##  2     2      19171918 Montreal Wanderers       19171918               41 MWN        Wanderers      Montreal     
    ##  3     3      19171918 St. Louis Eagles         19341935               45 SLE        Eagles         St. Louis    
    ##  4     4      19191920 Hamilton Tigers          19241925               37 HAM        Tigers         Hamilton     
    ##  5     5      19171918 Toronto Maple Leafs            NA               10 TOR        Maple Leafs    Toronto      
    ##  6     6      19241925 Boston Bruins                  NA                6 BOS        Bruins         Boston       
    ##  7     7      19241925 Montreal Maroons         19371938               43 MMR        Maroons        Montreal     
    ##  8     8      19251926 Brooklyn Americans       19411942               51 BRK        Americans      Brooklyn     
    ##  9     9      19251926 Philadelphia Quakers     19301931               39 QUA        Quakers        Philadelphia 
    ## 10    10      19261927 New York Rangers               NA                3 NYR        Rangers        New York     
    ## # … with 29 more rows

## NHL stats API

Next we write a function to contact the NHL stats API for the different
modifiers:

-   team.roster  
-   person.names  
-   team.schedule.next  
-   team.schedule.previous  
-   team.stats  
-   team.roster&season  
-   teamId  
-   statsSingleSeasonPlayoffs

Depending on the modifier, user will need to input different variables
into the function to retrieve meaningful data out. For this vignette we
will only work with the `team.stats` modifier.

``` r
get.stat2 <- function(modifier, id=NULL, season=NULL, id2=NULL, id3=NULL, id4=NULL, ...) {
  base.url3 <- "https://statsapi.web.nhl.com/api/v1/teams"
  
  ## change the id inputs to most.recent.id to be used in this stats api section
  if (is.numeric(id)) {
    for (i in 1:length(id.most.recent.id.list)) {
      if (i == id) {
        most.recent.id <- id.most.recent.id.list[i]
      }
    }
  }
  
  if (is.numeric(id2)) {
    for (i in 1:length(id.most.recent.id.list)) {
      if (i == id2) {
        most.recent.id2 <- id.most.recent.id.list[i]
      }
    }
  }  
  
  if (is.numeric(id3)) {
    for (i in 1:length(id.most.recent.id.list)) {
      if (i == id3) {
        most.recent.id3 <- id.most.recent.id.list[i]
      }
    }
  }   

  if (is.numeric(id4)) {
    for (i in 1:length(id.most.recent.id.list)) {
      if (i == id4) {
        most.recent.id4 <- id.most.recent.id.list[i]
      }
    }
  }      
  
  ## if modifier is 1 of the 5, no id (most.recent.id)
  if (modifier %in% c("team.roster",
                      "person.names",
                      "team.schedule.next", 
                      "team.schedule.previous",  
                      "team.stats") & 
      is.null(id)) {
    full.url2 <- paste0(base.url3, "?expand=", modifier)
  }
  ## if modifier is 1 of the 5, yes id (most.recent.id)
  else if (modifier %in% c("team.roster",
                           "person.names",
                           "team.schedule.next",
                           "team.schedule.previous",
                           "team.stats") & 
           is.numeric(most.recent.id)) {
    full.url2 <- paste0(base.url3, "/", most.recent.id, "?expand=", modifier)
  }
  
  ## if modifier is this, no id (most.recent.id), yes season
  else if (modifier %in% ("team.roster&season") & 
           is.null(id) & 
           is.numeric(season)) {
    full.url2 <- paste0(base.url3, "?expand=", modifier, "=", season)
  }
  
  ## if modifier is this, yes id (most.recent.id), yes season
  else if (modifier %in% ("team.roster&season") & 
           is.numeric(most.recent.id) & 
           is.numeric(season)) {
    full.url2 <- paste0(base.url3, "/", most.recent.id, "?expand=", modifier, "=", season)
  }  
  
  ## if modifier is this, no season
  else if (modifier %in% ("team.roster&season") & is.null(season)) {
    return ("Must provide a season input to get the team roster & season stats.")
  }    

  ## if modifier is this, multiple id's (most recent id's) given
  else if (modifier %in% ("teamId") & 
           is.numeric(c(most.recent.id, most.recent.id2, most.recent.id3, most.recent.id4))) {
    full.url2 <- paste0(base.url3, "?", modifier, "=",
                        paste(most.recent.id, most.recent.id2, most.recent.id3, most.recent.id4, sep = ","))
  }
  ## if modifier is this, no id (most recent id)
  else if (modifier %in% ("statsSingleSeasonPlayoffs") & is.null(id)) {
    full.url2 <- paste0(base.url3, "?stats=", modifier)
  }
  
  ## if modifier is this, yes id (most.recent.id)
  else if (modifier %in% ("statsSingleSeasonPlayoffs") & is.numeric(most.recent.id)) {
    full.url2 <- paste0(base.url3, "/", most.recent.id, "?stats=", modifier)
  }
  
  ## all other combinations
  else {
    return ("Invalid input, please try again.")  
  }
      
  nfl.stats3 <- GET(full.url2)
  nfl.stats3.txt <- content(nfl.stats3, "text", encoding = "UTF-8")
  nfl.stats3.json<- fromJSON(nfl.stats3.txt, flatten=TRUE)
  
  ## because returned tibble has data frames within, need to unnest twice to parse out everything  
  return (nfl.stats3.json$teams %>% unnest(everything()) %>% unnest(everything()) %>% as_tibble())

}
```

### Try out NHL stats API

We use the `get.stats2` function above and retrieved the `team.stats`
table. We see for each team there are 2 rows, first row has all the raw
data, the second row has the ranking information. So we can separate
this into two different tables by accessing the odd or even rows.

``` r
get.stat2("team.stats", 26)
```

    ## # A tibble: 2 x 65
    ##      id name       link      abbreviation teamName locationName firstYearOfPlay stat.gamesPlayed stat.wins stat.losses stat.ot stat.pts
    ##   <int> <chr>      <chr>     <chr>        <chr>    <chr>        <chr>                      <int> <chr>     <chr>       <chr>   <chr>   
    ## 1    12 Carolina … /api/v1/… CAR          Hurrica… Carolina     1979                          56 36        12          8       80      
    ## 2    12 Carolina … /api/v1/… CAR          Hurrica… Carolina     1979                          NA 5th       1st         7th     3rd     
    ## # … with 53 more variables: stat.ptPctg <chr>, stat.goalsPerGame <chr>, stat.goalsAgainstPerGame <chr>, stat.evGGARatio <chr>,
    ## #   stat.powerPlayPercentage <chr>, stat.powerPlayGoals <chr>, stat.powerPlayGoalsAgainst <chr>, stat.powerPlayOpportunities <chr>,
    ## #   stat.penaltyKillPercentage <chr>, stat.shotsPerGame <chr>, stat.shotsAllowed <chr>, stat.winScoreFirst <chr>,
    ## #   stat.winOppScoreFirst <chr>, stat.winLeadFirstPer <chr>, stat.winLeadSecondPer <chr>, stat.winOutshootOpp <chr>,
    ## #   stat.winOutshotByOpp <chr>, stat.faceOffsTaken <chr>, stat.faceOffsWon <chr>, stat.faceOffsLost <chr>,
    ## #   stat.faceOffWinPercentage <chr>, stat.shootingPctg <dbl>, stat.savePctg <dbl>, stat.penaltyKillOpportunities <chr>,
    ## #   stat.savePctRank <chr>, stat.shootingPctRank <chr>, team.id <int>, team.name <chr>, team.link <chr>, type.displayName <chr>,
    ## #   type.gameType.id <chr>, type.gameType.description <chr>, type.gameType.postseason <lgl>, shortName <chr>, officialSiteUrl <chr>,
    ## #   franchiseId <int>, active <lgl>, venue.id <int>, venue.name <chr>, venue.link <chr>, venue.city <chr>, venue.timeZone.id <chr>,
    ## #   venue.timeZone.offset <int>, venue.timeZone.tz <chr>, division.id <int>, division.name <chr>, division.link <chr>,
    ## #   conference.id <int>, conference.name <chr>, conference.link <chr>, franchise.franchiseId <int>, franchise.teamName <chr>,
    ## #   franchise.link <chr>

``` r
get.stats.data <- get.stat2("team.stats")
row.odd <- seq_len(nrow(get.stats.data)) %% 2  ## Create row indicator
row.odd ## Print row indicator
```

    ##  [1] 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1

``` r
# subset odd rows where numbers are given in raw
data.row.odd <- get.stats.data[row.odd == 1, ]
data.row.odd
```

    ## # A tibble: 32 x 65
    ##       id name      link      abbreviation teamName locationName firstYearOfPlay stat.gamesPlayed stat.wins stat.losses stat.ot stat.pts
    ##    <int> <chr>     <chr>     <chr>        <chr>    <chr>        <chr>                      <int> <chr>     <chr>       <chr>   <chr>   
    ##  1     1 New Jers… /api/v1/… NJD          Devils   New Jersey   1982                          56 19        30          7       45      
    ##  2     2 New York… /api/v1/… NYI          Islande… New York     1972                          56 32        17          7       71      
    ##  3     3 New York… /api/v1/… NYR          Rangers  New York     1926                          56 27        23          6       60      
    ##  4     4 Philadel… /api/v1/… PHI          Flyers   Philadelphia 1967                          56 25        23          8       58      
    ##  5     5 Pittsbur… /api/v1/… PIT          Penguins Pittsburgh   1967                          56 37        16          3       77      
    ##  6     6 Boston B… /api/v1/… BOS          Bruins   Boston       1924                          56 33        16          7       73      
    ##  7     7 Buffalo … /api/v1/… BUF          Sabres   Buffalo      1970                          56 15        34          7       37      
    ##  8     8 Montréal… /api/v1/… MTL          Canadie… Montréal     1909                          56 24        21          11      59      
    ##  9     9 Ottawa S… /api/v1/… OTT          Senators Ottawa       1990                          56 23        28          5       51      
    ## 10    10 Toronto … /api/v1/… TOR          Maple L… Toronto      1917                          56 35        14          7       77      
    ## # … with 22 more rows, and 53 more variables: stat.ptPctg <chr>, stat.goalsPerGame <chr>, stat.goalsAgainstPerGame <chr>,
    ## #   stat.evGGARatio <chr>, stat.powerPlayPercentage <chr>, stat.powerPlayGoals <chr>, stat.powerPlayGoalsAgainst <chr>,
    ## #   stat.powerPlayOpportunities <chr>, stat.penaltyKillPercentage <chr>, stat.shotsPerGame <chr>, stat.shotsAllowed <chr>,
    ## #   stat.winScoreFirst <chr>, stat.winOppScoreFirst <chr>, stat.winLeadFirstPer <chr>, stat.winLeadSecondPer <chr>,
    ## #   stat.winOutshootOpp <chr>, stat.winOutshotByOpp <chr>, stat.faceOffsTaken <chr>, stat.faceOffsWon <chr>, stat.faceOffsLost <chr>,
    ## #   stat.faceOffWinPercentage <chr>, stat.shootingPctg <dbl>, stat.savePctg <dbl>, stat.penaltyKillOpportunities <chr>,
    ## #   stat.savePctRank <chr>, stat.shootingPctRank <chr>, team.id <int>, team.name <chr>, team.link <chr>, type.displayName <chr>,
    ## #   type.gameType.id <chr>, type.gameType.description <chr>, type.gameType.postseason <lgl>, shortName <chr>, officialSiteUrl <chr>,
    ## #   franchiseId <int>, active <lgl>, venue.name <chr>, venue.link <chr>, venue.city <chr>, venue.id <int>, venue.timeZone.id <chr>,
    ## #   venue.timeZone.offset <int>, venue.timeZone.tz <chr>, division.id <int>, division.name <chr>, division.link <chr>,
    ## #   conference.id <int>, conference.name <chr>, conference.link <chr>, franchise.franchiseId <int>, franchise.teamName <chr>,
    ## #   franchise.link <chr>

``` r
# subset even rows where ranks are given instead of raw number
data.row.even.ranks <- get.stats.data[row.odd == 0, ]
data.row.even.ranks
```

    ## # A tibble: 31 x 65
    ##       id name      link      abbreviation teamName locationName firstYearOfPlay stat.gamesPlayed stat.wins stat.losses stat.ot stat.pts
    ##    <int> <chr>     <chr>     <chr>        <chr>    <chr>        <chr>                      <int> <chr>     <chr>       <chr>   <chr>   
    ##  1     1 New Jers… /api/v1/… NJD          Devils   New Jersey   1982                          NA 28th      29th        15th    29th    
    ##  2     2 New York… /api/v1/… NYI          Islande… New York     1972                          NA 12th      11th        11th    12th    
    ##  3     3 New York… /api/v1/… NYR          Rangers  New York     1926                          NA 16th      18th        17th    17th    
    ##  4     4 Philadel… /api/v1/… PHI          Flyers   Philadelphia 1967                          NA 18th      19th        8th     19th    
    ##  5     5 Pittsbur… /api/v1/… PIT          Penguins Pittsburgh   1967                          NA 4th       7th         25th    7th     
    ##  6     6 Boston B… /api/v1/… BOS          Bruins   Boston       1924                          NA 11th      9th         10th    10th    
    ##  7     7 Buffalo … /api/v1/… BUF          Sabres   Buffalo      1970                          NA 31st      31st        16th    31st    
    ##  8     8 Montréal… /api/v1/… MTL          Canadie… Montréal     1909                          NA 19th      15th        3rd     18th    
    ##  9     9 Ottawa S… /api/v1/… OTT          Senators Ottawa       1990                          NA 23rd      25th        22nd    23rd    
    ## 10    10 Toronto … /api/v1/… TOR          Maple L… Toronto      1917                          NA 8th       5th         9th     5th     
    ## # … with 21 more rows, and 53 more variables: stat.ptPctg <chr>, stat.goalsPerGame <chr>, stat.goalsAgainstPerGame <chr>,
    ## #   stat.evGGARatio <chr>, stat.powerPlayPercentage <chr>, stat.powerPlayGoals <chr>, stat.powerPlayGoalsAgainst <chr>,
    ## #   stat.powerPlayOpportunities <chr>, stat.penaltyKillPercentage <chr>, stat.shotsPerGame <chr>, stat.shotsAllowed <chr>,
    ## #   stat.winScoreFirst <chr>, stat.winOppScoreFirst <chr>, stat.winLeadFirstPer <chr>, stat.winLeadSecondPer <chr>,
    ## #   stat.winOutshootOpp <chr>, stat.winOutshotByOpp <chr>, stat.faceOffsTaken <chr>, stat.faceOffsWon <chr>, stat.faceOffsLost <chr>,
    ## #   stat.faceOffWinPercentage <chr>, stat.shootingPctg <dbl>, stat.savePctg <dbl>, stat.penaltyKillOpportunities <chr>,
    ## #   stat.savePctRank <chr>, stat.shootingPctRank <chr>, team.id <int>, team.name <chr>, team.link <chr>, type.displayName <chr>,
    ## #   type.gameType.id <chr>, type.gameType.description <chr>, type.gameType.postseason <lgl>, shortName <chr>, officialSiteUrl <chr>,
    ## #   franchiseId <int>, active <lgl>, venue.name <chr>, venue.link <chr>, venue.city <chr>, venue.id <int>, venue.timeZone.id <chr>,
    ## #   venue.timeZone.offset <int>, venue.timeZone.tz <chr>, division.id <int>, division.name <chr>, division.link <chr>,
    ## #   conference.id <int>, conference.name <chr>, conference.link <chr>, franchise.franchiseId <int>, franchise.teamName <chr>,
    ## #   franchise.link <chr>

## Wrapper Function

We write a wrapper function as the one-stop-shop for the user to access
any of the API endpoints above. This wrapper function can let user
access both the `get.records` and `get.stats2` functions using any
modifiers, teamID’s, team names, etc.

``` r
get.nhl.data <- function (table.name=NULL, 
                          modifier=NULL, 
                          id=NULL, 
                          team.common.name=NULL, 
                          season=NULL, 
                          id2=NULL, id3=NULL, id4=NULL, ...) {

  ## yes table name, yes modifier
  if (!is.null(table.name) & !is.null(modifier)) {
    stop ("Cannot use both table name and modifier to get back information.")
  }
  
  ## yes table name
  else if (!is.null(table.name) & is.null(team.common.name) & is.null(id) & is.null(modifier)) {
    output <- get.records(table.name)
  }  
  
  ## yes table name, yes id or team.common.name, no modifier
  else if (!is.null(table.name) & (is.numeric(id) | !is.null(team.common.name)) & is.null(modifier)) {
    output <- get.records(table.name, id, team.common.name)
  }
  
  ## yes modifier
  else if (!is.null(modifier) & is.null(table.name) & is.null(id) & is.null(team.common.name)) {
    output <- get.stat2(modifier)
  }
  
  ## yes modifier, yes id
  else if (!is.null(modifier) & is.null(table.name) & is.numeric(id)) {
    output <- get.stat2(modifier, id)
  }
  
  ## yes modifier, yes season
  else if (!is.null(modifier) & is.null(table.name) & !is.null(season)) {
    output <- get.stat2(modifier, season)
  }
  
  ## yes modifier, yes multiple id
  else if (!is.null(modifier) & is.null(table.name) & is.numeric(id) & is.numeric(id2)) {
    output <- get.stat2(modifier, id, id2, id3, id4)
  }
  
  ## all other combinations
  else {
    return ("Invalid combination of inputs, please try again.")
  }
  
  return(output)
}
```

### Try out wrapper function

We can try a few different combinations to make sure the `get.nhl.data`
wrapper function works properly.

``` r
get.nhl.data(table.name = "franchise-detail", id=12)
```

    ##   id active         dateAwarded                                          directoryUrl firstSeasonId
    ## 1 12   TRUE 1926-09-25T00:00:00 https://www.nhl.com/redwings/team/business-operations      19261927
    ##                                                                     heroImageUrl mostRecentTeamId teamAbbrev      teamFullName
    ## 1 https://records.nhl.com/site/asset/public/ext/hero/Team Pages/DET/WingsWin.jpg               17        DET Detroit Red Wings
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           captainHistory
    ## 1 Dylan Larkin: 2020-21 – Present(No Captain): 2018-19 – 2019-20Henrik Zetterberg: 2012-13 – 2017-18Nicklas Lidstrom: 2006-07 – 2011-12Steve Yzerman: 1986-87 – 2005-06Danny Gare: 1982-83 – 1985-86Reed Larson: 1981-82Errol Thompson and Reed Larson: 1980-81Dale McCourt: 1979-80Dennis Hextall, Nick Libett and Paul Woods: 1978-79Dan Maloney and Dennis Hextall: 1977-78Danny Grant and Dennis Polonich: 1976-77Danny Grant and Terry Harper: 1975-76Marcel Dionne: 1974-75Alex Delvecchio, Nick Libett, Red Berenson, Gary Bergman, Ted Harris, Mickey Redmond and Larry Johnston: 1973-74Alex Delvecchio: 1962-63 – 1972-73Gordie Howe: 1958-59 – 1961-62Red Kelly: 1956-57 – 1957-58Ted Lindsay: 1952-53 – 1955-56Sid Abel: 1946-47 – 1951-52Flash Hollett and Sid Abel: 1945-46Flash Hollett: 1944-45Mud Bruneteau and Flash Hollett: 1943-44Sid Abel: 1942-43Ebbie Goodfellow and Syd Howe: 1941-42Ebbie Goodfellow: 1938-39 – 1940-41Doug Young: 1935-36 – 1937-38Ebbie Goodfellow: 1934-35Herbie Lewis: 1933-34Larry Aurie: 1932-33Carson Cooper: 1931-32George Hay: 1930-31Reg Noble: 1927-28 – 1929-30Art Duncan: 1926-27
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                coachingHistory
    ## 1 Jeff Blashill: Oct. 9, 2015 – PresentMike Babcock: Oct. 5, 2005 – April 29, 2015Dave Lewis: Oct. 10, 2002 – May 3, 2004Scotty Bowman: Oct. 23, 1998 – June 13, 2002Dave Lewis and Barry Smith (Co-Coaches): Oct. 10-21, 1998Scotty Bowman: Oct. 5, 1993 – June 16, 1998Bryan Murray: Oct. 4, 1990 – May 1, 1993Jacques Demers: Oct. 9, 1986 – April 1, 1990Brad Park: Dec. 31, 1985 – April 6, 1986Harry Neale: Oct 10 – Dec. 29, 1985Nick Polano: Oct. 6, 1982 – April 13, 1985Billy Dea: March 11 – April 4, 1982Wayne Maxner: Nov. 26, 1980 – March 8, 1982Ted Lindsay: March 21 – Nov. 22, 1980Bobby Kromm: Oct. 13, 1977 – March 19, 1980Larry Wilson: Jan. 20 – April 3, 1977Alex Delvecchio: Dec. 5, 1975 – Jan. 15, 1977Doug Barkley: Oct. 8 – Dec. 3, 1975Alex Delvecchio: Nov. 7, 1973 – April 6, 1975Ted Garvin: Oct. 10 – Nov. 4, 1973Johnny Wilson: Nov. 1, 1971 – April 1, 1973Doug Barkley: Jan. 9 – Oct. 31, 1971Ned Harkness: Oct. 10, 1970 – Jan. 7, 1971Sid Abel: Oct. 16, 1969 – April 12, 1970Bill Gadsby: Oct. 11, 1968 – Oct. 15, 1969Sid Abel: Jan. 4, 1958 – March 31, 1968Jimmy Skinner: Oct. 7, 1954 – Jan. 1, 1958Tommy Ivan: Oct. 15, 1947 – April 16, 1954Jack Adams: Nov. 15, 1927 – April 5, 1947Duke Keats: Feb. 24 – March 26, 1927Art Duncan: Nov. 18, 1926 – Feb. 22, 1927* Date range indicates first and last games coached during tenure (regular season or playoffs)
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 generalManagerHistory
    ## 1 Steve Yzerman: April 19, 2019 – PresentKen Holland: July 18, 1997 – April 19, 2019Jim Devellano: June 3, 1994 – July 18, 1997Bryan Murray: July 13, 1990 – June 3, 1994Jim Devellano: July 12, 1982 – July 11, 1990Jimmy Skinner: April 11, 1980 – July 12, 1982Ted Lindsay: March 16, 1977 – April 11, 1980Alex Delvecchio: May 21, 1974 – March 16, 1977Jimmy Skinner: Feb. 6 – May 21, 1974Ned Harkness: Jan. 8, 1971 – Feb. 6, 1974Sid Abel: April 26, 1962 – Jan. 6, 1971Jack Adams: May 14, 1927 – April 26, 1962Art Duncan: Oct. 18, 1926 – May 14, 1927* Date range indicates first and last days of tenure
    ##                                                                                                                                                                                                                                     retiredNumbersSummary
    ## 1 1 – Terry Sawchuk (1949-55, 1957-64, 1968-69)4 – Red Kelly (1947-60)5 – Nicklas Lidstrom (1991-12)7 – Ted Lindsay (1947-57, 1964-65)9 – Gordie Howe (1946-71)10 – Alex Delvecchio (1951-73)12 – Sid Abel (1938-43, 1945-52)19 – Steve Yzerman (1983-06)

``` r
get.nhl.data(table.name = "franchise-team-totals")
```

    ## # A tibble: 105 x 30
    ##       id activeFranchise firstSeasonId franchiseId gameTypeId gamesPlayed goalsAgainst goalsFor homeLosses homeOvertimeLosses homeTies
    ##    <int>           <int>         <int>       <int>      <int>       <int>        <int>    <int>      <int>              <int>    <int>
    ##  1     1               1      19821983          23          2        2993         8902     8792        525                 85       96
    ##  2     2               1      19821983          23          3         257          634      697         53                  0       NA
    ##  3     3               1      19721973          22          2        3788        11907    12045        678                 84      170
    ##  4     4               1      19721973          22          3         310          899      986         53                  1       NA
    ##  5     5               1      19261927          10          2        6560        20020    20041       1143                 76      448
    ##  6     6               1      19261927          10          3         518         1447     1404        104                  0        1
    ##  7     7               1      19671968          16          3         449         1332     1335         97                  0       NA
    ##  8     8               1      19671968          16          2        4171        12255    13690        584                 93      193
    ##  9     9               1      19671968          17          2        4171        14049    13874        683                 60      205
    ## 10    10               1      19671968          17          3         391         1131     1190         85                  0       NA
    ## # … with 95 more rows, and 19 more variables: homeWins <int>, lastSeasonId <int>, losses <int>, overtimeLosses <int>,
    ## #   penaltyMinutes <int>, pointPctg <dbl>, points <int>, roadLosses <int>, roadOvertimeLosses <int>, roadTies <int>, roadWins <int>,
    ## #   shootoutLosses <int>, shootoutWins <int>, shutouts <int>, teamId <int>, teamName <chr>, ties <int>, triCode <chr>, wins <int>

``` r
get.nhl.data(modifier = "person.names", id = 26, season = 20142015)
```

    ## # A tibble: 1 x 27
    ##      id name  link  abbreviation teamName locationName firstYearOfPlay shortName officialSiteUrl franchiseId active venue.id venue.name
    ##   <int> <chr> <chr> <chr>        <chr>    <chr>        <chr>           <chr>     <chr>                 <int> <lgl>     <int> <chr>     
    ## 1    12 Caro… /api… CAR          Hurrica… Carolina     1979            Carolina  http://www.car…          26 TRUE       5066 PNC Arena 
    ## # … with 14 more variables: venue.link <chr>, venue.city <chr>, venue.timeZone.id <chr>, venue.timeZone.offset <int>,
    ## #   venue.timeZone.tz <chr>, division.id <int>, division.name <chr>, division.link <chr>, conference.id <int>, conference.name <chr>,
    ## #   conference.link <chr>, franchise.franchiseId <int>, franchise.teamName <chr>, franchise.link <chr>

``` r
get.nhl.data(modifier = "team.stats", id = 26)
```

    ## # A tibble: 2 x 65
    ##      id name       link      abbreviation teamName locationName firstYearOfPlay stat.gamesPlayed stat.wins stat.losses stat.ot stat.pts
    ##   <int> <chr>      <chr>     <chr>        <chr>    <chr>        <chr>                      <int> <chr>     <chr>       <chr>   <chr>   
    ## 1    12 Carolina … /api/v1/… CAR          Hurrica… Carolina     1979                          56 36        12          8       80      
    ## 2    12 Carolina … /api/v1/… CAR          Hurrica… Carolina     1979                          NA 5th       1st         7th     3rd     
    ## # … with 53 more variables: stat.ptPctg <chr>, stat.goalsPerGame <chr>, stat.goalsAgainstPerGame <chr>, stat.evGGARatio <chr>,
    ## #   stat.powerPlayPercentage <chr>, stat.powerPlayGoals <chr>, stat.powerPlayGoalsAgainst <chr>, stat.powerPlayOpportunities <chr>,
    ## #   stat.penaltyKillPercentage <chr>, stat.shotsPerGame <chr>, stat.shotsAllowed <chr>, stat.winScoreFirst <chr>,
    ## #   stat.winOppScoreFirst <chr>, stat.winLeadFirstPer <chr>, stat.winLeadSecondPer <chr>, stat.winOutshootOpp <chr>,
    ## #   stat.winOutshotByOpp <chr>, stat.faceOffsTaken <chr>, stat.faceOffsWon <chr>, stat.faceOffsLost <chr>,
    ## #   stat.faceOffWinPercentage <chr>, stat.shootingPctg <dbl>, stat.savePctg <dbl>, stat.penaltyKillOpportunities <chr>,
    ## #   stat.savePctRank <chr>, stat.shootingPctRank <chr>, team.id <int>, team.name <chr>, team.link <chr>, type.displayName <chr>,
    ## #   type.gameType.id <chr>, type.gameType.description <chr>, type.gameType.postseason <lgl>, shortName <chr>, officialSiteUrl <chr>,
    ## #   franchiseId <int>, active <lgl>, venue.id <int>, venue.name <chr>, venue.link <chr>, venue.city <chr>, venue.timeZone.id <chr>,
    ## #   venue.timeZone.offset <int>, venue.timeZone.tz <chr>, division.id <int>, division.name <chr>, division.link <chr>,
    ## #   conference.id <int>, conference.name <chr>, conference.link <chr>, franchise.franchiseId <int>, franchise.teamName <chr>,
    ## #   franchise.link <chr>

# Exporatory Data Analysis

After setting up the functions to retrieve data from the NHL API, we can
grab data to specifically do exploratory data analysis.

## Grabbing data and combine

First we can use the wrapper function to retrieve the
`franchise-team-totals` data, filter by specific fields to only retain
information for the 31 current active teams. Then we can retrieve the
`team.stats` data and only keep the odd rows (which are the raw data
without ranking).

``` r
# get all information from "franchise-team-totals"
team.total.raw <- get.nhl.data(table.name = "franchise-team-totals") 

# filter to get only the 31 active teams
team.total <- team.total.raw %>% rename(abbreviation = triCode) %>% 
  select(activeFranchise, gameTypeId, lastSeasonId, everything()) %>%
  filter(activeFranchise==1 & gameTypeId==2 & is.na(lastSeasonId))
team.total
```

    ## # A tibble: 31 x 30
    ##    activeFranchise gameTypeId lastSeasonId    id firstSeasonId franchiseId gamesPlayed goalsAgainst goalsFor homeLosses
    ##              <int>      <int>        <int> <int>         <int>       <int>       <int>        <int>    <int>      <int>
    ##  1               1          2           NA     1      19821983          23        2993         8902     8792        525
    ##  2               1          2           NA     3      19721973          22        3788        11907    12045        678
    ##  3               1          2           NA     5      19261927          10        6560        20020    20041       1143
    ##  4               1          2           NA     8      19671968          16        4171        12255    13690        584
    ##  5               1          2           NA     9      19671968          17        4171        14049    13874        683
    ##  6               1          2           NA    11      19241925           6        6626        19137    21112        960
    ##  7               1          2           NA    13      19701971          19        3945        11966    12471        639
    ##  8               1          2           NA    16      19171918           1        6787        18260    21791        881
    ##  9               1          2           NA    17      19921993          30        2195         6580     6250        413
    ## 10               1          2           NA    19      19271928           5        6516        19953    19980       1082
    ## # … with 21 more rows, and 20 more variables: homeOvertimeLosses <int>, homeTies <int>, homeWins <int>, losses <int>,
    ## #   overtimeLosses <int>, penaltyMinutes <int>, pointPctg <dbl>, points <int>, roadLosses <int>, roadOvertimeLosses <int>,
    ## #   roadTies <int>, roadWins <int>, shootoutLosses <int>, shootoutWins <int>, shutouts <int>, teamId <int>, teamName <chr>,
    ## #   ties <int>, abbreviation <chr>, wins <int>

``` r
# get division data for different teams from "team.stats"
division.raw <- get.nhl.data(modifier = "team.stats")

# select only odd rows (does not include ranks), then select useful columns
row.odd2 <- seq_len(nrow(division.raw)) %% 2  ## Create row indicator

division <- division.raw[row.odd2 == 1, ] %>%
  select(franchiseId, venue.name, venue.city, 
         division.id, division.name, conference.id, conference.name)
division
```

    ## # A tibble: 32 x 7
    ##    franchiseId venue.name                        venue.city   division.id division.name   conference.id conference.name
    ##          <int> <chr>                             <chr>              <int> <chr>                   <int> <chr>          
    ##  1          23 Prudential Center                 Newark                25 MassMutual East             6 Eastern        
    ##  2          22 Nassau Veterans Memorial Coliseum Uniondale             25 MassMutual East             6 Eastern        
    ##  3          10 Madison Square Garden             New York              25 MassMutual East             6 Eastern        
    ##  4          16 Wells Fargo Center                Philadelphia          25 MassMutual East             6 Eastern        
    ##  5          17 PPG Paints Arena                  Pittsburgh            25 MassMutual East             6 Eastern        
    ##  6           6 TD Garden                         Boston                25 MassMutual East             6 Eastern        
    ##  7          19 KeyBank Center                    Buffalo               25 MassMutual East             6 Eastern        
    ##  8           1 Bell Centre                       Montréal              28 Scotia North                6 Eastern        
    ##  9          30 Canadian Tire Centre              Ottawa                28 Scotia North                6 Eastern        
    ## 10           5 Scotiabank Arena                  Toronto               28 Scotia North                6 Eastern        
    ## # … with 22 more rows

We find out what columns are present in both of these tables then left
join using a common index (in this case we use the franchiseId) to
create a new combined data named `new.data`.

``` r
# find common columns between the 2 data frames
common.names.dv <- intersect(names(team.total), names(division))
common.names.dv
```

    ## [1] "franchiseId"

``` r
# combine the 2 tables (join matching rows from division to team.total) together using "franchiseId" as index
new.data <- left_join(team.total, division, by="franchiseId")
new.data
```

    ## # A tibble: 31 x 36
    ##    activeFranchise gameTypeId lastSeasonId    id firstSeasonId franchiseId gamesPlayed goalsAgainst goalsFor homeLosses
    ##              <int>      <int>        <int> <int>         <int>       <int>       <int>        <int>    <int>      <int>
    ##  1               1          2           NA     1      19821983          23        2993         8902     8792        525
    ##  2               1          2           NA     3      19721973          22        3788        11907    12045        678
    ##  3               1          2           NA     5      19261927          10        6560        20020    20041       1143
    ##  4               1          2           NA     8      19671968          16        4171        12255    13690        584
    ##  5               1          2           NA     9      19671968          17        4171        14049    13874        683
    ##  6               1          2           NA    11      19241925           6        6626        19137    21112        960
    ##  7               1          2           NA    13      19701971          19        3945        11966    12471        639
    ##  8               1          2           NA    16      19171918           1        6787        18260    21791        881
    ##  9               1          2           NA    17      19921993          30        2195         6580     6250        413
    ## 10               1          2           NA    19      19271928           5        6516        19953    19980       1082
    ## # … with 21 more rows, and 26 more variables: homeOvertimeLosses <int>, homeTies <int>, homeWins <int>, losses <int>,
    ## #   overtimeLosses <int>, penaltyMinutes <int>, pointPctg <dbl>, points <int>, roadLosses <int>, roadOvertimeLosses <int>,
    ## #   roadTies <int>, roadWins <int>, shootoutLosses <int>, shootoutWins <int>, shutouts <int>, teamId <int>, teamName <chr>,
    ## #   ties <int>, abbreviation <chr>, wins <int>, venue.name <chr>, venue.city <chr>, division.id <int>, division.name <chr>,
    ## #   conference.id <int>, conference.name <chr>

## Create new variables

After the new table `new.data` is created, we can add in new variables
that are functions of the existing variables. We append these new
variables into the `new.data` table as new columns.

``` r
# % of wins from all games played
new.data$perc.total.games.win <- (new.data$wins / new.data$gamesPlayed * 100) %>% round(2)

# % of losses from all games played
new.data$perc.total.games.loss <- (new.data$losses / new.data$gamesPlayed * 100) %>% round(2)

# % of wins from home games
new.data$perc.home.win <- (new.data$homeWins / (new.data$homeLosses + new.data$homeOvertimeLosses + new.data$homeTies + new.data$homeWins) *100) %>% round(2)

# % of losses from home games
new.data$perc.home.loss <- (new.data$homeLosses / (new.data$homeLosses + new.data$homeOvertimeLosses + new.data$homeTies + new.data$homeWins) *100) %>% round(2)

# % of wins from road games
new.data$perc.road.win <- (new.data$roadWins / (new.data$roadLosses + new.data$roadOvertimeLosses + new.data$roadTies + new.data$roadWins) *100) %>% round(2)

# % of losses from road games
new.data$perc.road.loss <- (new.data$roadLosses / (new.data$roadLosses + new.data$roadOvertimeLosses + new.data$roadTies + new.data$roadWins) *100) %>% round(2)

new.data %>% 
  select(perc.total.games.win, perc.total.games.loss, perc.home.win, perc.home.loss, perc.road.win, perc.road.loss, 
         everything())
```

    ## # A tibble: 31 x 42
    ##    perc.total.games… perc.total.game… perc.home.win perc.home.loss perc.road.win perc.road.loss activeFranchise gameTypeId lastSeasonId
    ##                <dbl>            <dbl>         <dbl>          <dbl>         <dbl>          <dbl>           <int>      <int>        <int>
    ##  1              46.6             40.5          52.8           35.1          40.4           45.8               1          2           NA
    ##  2              44.6             41.9          50.8           35.8          38.3           48.0               1          2           NA
    ##  3              44.0             41.4          49.2           34.8          38.7           48.0               1          2           NA
    ##  4              49.8             34.8          58.3           28            41.4           41.6               1          2           NA
    ##  5              45.6             41.6          54.6           32.7          36.7           50.4               1          2           NA
    ##  6              48.9             36.3          56.9           29.0          40.9           43.6               1          2           NA
    ##  7              45.8             39.6          53.4           32.4          38.1           46.9               1          2           NA
    ##  8              51.2             33.9          60.0           26.0          42.3           41.9               1          2           NA
    ##  9              44.2             42.8          48.5           37.6          40.0           48.1               1          2           NA
    ## 10              44.1             41.4          52.3           33.2          35.9           49.5               1          2           NA
    ## # … with 21 more rows, and 33 more variables: id <int>, firstSeasonId <int>, franchiseId <int>, gamesPlayed <int>, goalsAgainst <int>,
    ## #   goalsFor <int>, homeLosses <int>, homeOvertimeLosses <int>, homeTies <int>, homeWins <int>, losses <int>, overtimeLosses <int>,
    ## #   penaltyMinutes <int>, pointPctg <dbl>, points <int>, roadLosses <int>, roadOvertimeLosses <int>, roadTies <int>, roadWins <int>,
    ## #   shootoutLosses <int>, shootoutWins <int>, shutouts <int>, teamId <int>, teamName <chr>, ties <int>, abbreviation <chr>,
    ## #   wins <int>, venue.name <chr>, venue.city <chr>, division.id <int>, division.name <chr>, conference.id <int>, conference.name <chr>

## Read data from 2 different endpoints and combine

Another good example of reading data from 2 different endpoints and
combining them together is finding common columns within the
`franchise-goalie-records` and `franchise-skater-records` tables, select
only the common columns for each table then row bind them together. This
way we end up with a complete list of player information for all player
positions.

Below is how we combine player information for the Hurricanes franchise
(teamID=26).

``` r
# read from goalie records endpoint for hurricanes
goalie.records <- get.nhl.data(table.name = "franchise-goalie-records", id = 26)
goalie.records
```

    ## # A tibble: 38 x 29
    ##       id activePlayer firstName franchiseId franchiseName  gameTypeId gamesPlayed lastName  losses mostGoalsAgainstDa… mostGoalsAgains…
    ##    <int> <lgl>        <chr>           <int> <chr>               <int>       <int> <chr>      <int> <chr>                          <int>
    ##  1   336 FALSE        Tom                26 Carolina Hurr…          2          34 Barrasso      12 2001-12-30, 2001-1…                5
    ##  2   363 FALSE        Richard            26 Carolina Hurr…          2           6 Brodeur        2 1988-03-09                         4
    ##  3   369 FALSE        Sean               26 Carolina Hurr…          2         256 Burke        120 1992-12-11                         9
    ##  4   411 FALSE        Mark               26 Carolina Hurr…          2           3 Fitzpatr…      2 2000-02-15                         5
    ##  5   425 FALSE        John               26 Carolina Hurr…          2         122 Garrett       57 1982-01-02, 1980-1…                9
    ##  6   430 FALSE        Mario              26 Carolina Hurr…          2          23 Gosselin      13 1993-04-10, 1993-0…                6
    ##  7   470 FALSE        Pat                26 Carolina Hurr…          2           5 Jablonski      4 1998-01-03                         6
    ##  8   490 FALSE        Mike               26 Carolina Hurr…          2         252 Liut         111 1985-10-23                         9
    ##  9   508 FALSE        Kirk               26 Carolina Hurr…          2           8 McLean         2 1998-03-06, 1998-0…                4
    ## 10   525 FALSE        Greg               26 Carolina Hurr…          2         219 Millen       120 1983-02-23, 1982-1…               11
    ## # … with 28 more rows, and 18 more variables: mostSavesDates <chr>, mostSavesOneGame <int>, mostShotsAgainstDates <chr>,
    ## #   mostShotsAgainstOneGame <int>, mostShutoutsOneSeason <int>, mostShutoutsSeasonIds <chr>, mostWinsOneSeason <int>,
    ## #   mostWinsSeasonIds <chr>, overtimeLosses <int>, playerId <int>, positionCode <chr>, rookieGamesPlayed <int>, rookieShutouts <int>,
    ## #   rookieWins <int>, seasons <int>, shutouts <int>, ties <int>, wins <int>

``` r
# read from skater records endpoint for hurricanes
skater.records <- get.nhl.data(table.name = "franchise-skater-records", id = 26)
skater.records
```

    ## # A tibble: 487 x 31
    ##       id activePlayer assists firstName franchiseId franchiseName  gameTypeId gamesPlayed goals lastName  mostAssistsGameDates         
    ##    <int> <lgl>          <int> <chr>           <int> <chr>               <int>       <int> <int> <chr>     <chr>                        
    ##  1 17239 FALSE              0 Jim                26 Carolina Hurr…          2          16     0 Agnew     1992-10-06, 1992-10-08, 1992…
    ##  2 17418 FALSE              1 Mike               26 Carolina Hurr…          2           5     0 Antonovi… 1979-10-13                   
    ##  3 17543 FALSE              0 Fred               26 Carolina Hurr…          2           3     0 Arthur    1980-10-09, 1980-10-11, 1980…
    ##  4 17703 FALSE              2 Jergus             26 Carolina Hurr…          2          10     0 Baca      1990-10-27, 1991-02-24       
    ##  5 17728 FALSE              0 Reid               26 Carolina Hurr…          2          12     0 Bailey    1983-10-05, 1983-10-08, 1983…
    ##  6 18169 FALSE              0 Bob                26 Carolina Hurr…          2           1     0 Bodak     1989-12-14, 1989-12-16, 1989…
    ##  7 18233 FALSE              0 Charlie            26 Carolina Hurr…          2           1     0 Bourgeois 1988-03-15, 1988-03-19, 1988…
    ##  8 18288 FALSE              0 Greg               26 Carolina Hurr…          2           1     0 Britz     1986-10-11, 1986-10-12, 1986…
    ##  9 18328 FALSE              1 Jeff               26 Carolina Hurr…          2           7     0 Brownsch… 1982-04-03                   
    ## 10 18799 FALSE              1 Shane              26 Carolina Hurr…          2          22     0 Churla    1987-02-01                   
    ## # … with 477 more rows, and 20 more variables: mostAssistsOneGame <int>, mostAssistsOneSeason <int>, mostAssistsSeasonIds <chr>,
    ## #   mostGoalsGameDates <chr>, mostGoalsOneGame <int>, mostGoalsOneSeason <int>, mostGoalsSeasonIds <chr>,
    ## #   mostPenaltyMinutesOneSeason <int>, mostPenaltyMinutesSeasonIds <chr>, mostPointsGameDates <chr>, mostPointsOneGame <int>,
    ## #   mostPointsOneSeason <int>, mostPointsSeasonIds <chr>, penaltyMinutes <int>, playerId <int>, points <int>, positionCode <chr>,
    ## #   rookieGamesPlayed <int>, rookiePoints <int>, seasons <int>

``` r
# find common columns between the two tibbles
common.names <- intersect(names(goalie.records), names(skater.records))
common.names
```

    ##  [1] "id"                "activePlayer"      "firstName"         "franchiseId"       "franchiseName"     "gameTypeId"       
    ##  [7] "gamesPlayed"       "lastName"          "playerId"          "positionCode"      "rookieGamesPlayed" "seasons"

``` r
# only keep common columns in each tibble
goalie.records <- goalie.records %>% select(all_of(common.names))
skater.records <- skater.records %>% select(all_of(common.names))

# combine the 2 tibbles together by binding rows
hurricanes.players.record <- rbind(goalie.records, skater.records)
hurricanes.players.record
```

    ## # A tibble: 525 x 12
    ##       id activePlayer firstName franchiseId franchiseName      gameTypeId gamesPlayed lastName   playerId positionCode rookieGamesPlay…
    ##    <int> <lgl>        <chr>           <int> <chr>                   <int>       <int> <chr>         <int> <chr>                   <int>
    ##  1   336 FALSE        Tom                26 Carolina Hurrican…          2          34 Barrasso    8445275 G                          NA
    ##  2   363 FALSE        Richard            26 Carolina Hurrican…          2           6 Brodeur     8445694 G                          NA
    ##  3   369 FALSE        Sean               26 Carolina Hurrican…          2         256 Burke       8445769 G                          NA
    ##  4   411 FALSE        Mark               26 Carolina Hurrican…          2           3 Fitzpatri…  8446829 G                          NA
    ##  5   425 FALSE        John               26 Carolina Hurrican…          2         122 Garrett     8447066 G                          NA
    ##  6   430 FALSE        Mario              26 Carolina Hurrican…          2          23 Gosselin    8447303 G                          NA
    ##  7   470 FALSE        Pat                26 Carolina Hurrican…          2           5 Jablonski   8448207 G                          NA
    ##  8   490 FALSE        Mike               26 Carolina Hurrican…          2         252 Liut        8448865 G                          NA
    ##  9   508 FALSE        Kirk               26 Carolina Hurrican…          2           8 McLean      8449474 G                          NA
    ## 10   525 FALSE        Greg               26 Carolina Hurrican…          2         219 Millen      8449627 G                          NA
    ## # … with 515 more rows, and 1 more variable: seasons <int>

And we can do similar things to compile a complete list of players
information for all positions for all franchises. We can also add in the
franchise division information to the players information by left
joining using `franchiseId` as the index.

``` r
# read from goalie records endpoint for all teams
goalie.records2 <- get.nhl.data(table.name = "franchise-goalie-records")
goalie.records2
```

    ## # A tibble: 1,078 x 29
    ##       id activePlayer firstName franchiseId franchiseName  gameTypeId gamesPlayed lastName losses mostGoalsAgainstDa… mostGoalsAgainst…
    ##    <int> <lgl>        <chr>           <int> <chr>               <int>       <int> <chr>     <int> <chr>                           <int>
    ##  1   235 FALSE        Don                15 Dallas Stars            2         315 Beaupre     125 1983-10-07                         10
    ##  2   236 FALSE        Bob                28 Arizona Coyot…          2         281 Essensa     114 1992-12-11, 1992-1…                 8
    ##  3   237 FALSE        Tony               11 Chicago Black…          2         873 Esposito    302 1983-10-15, 1980-1…                10
    ##  4   238 FALSE        Grant              25 Edmonton Oile…          2         423 Fuhr        117 1984-02-05, 1982-1…                 9
    ##  5   239 FALSE        Ron                16 Philadelphia …          2         489 Hextall     172 1987-04-05                          9
    ##  6   240 FALSE        Curtis             18 St. Louis Blu…          2         280 Joseph       96 1992-11-25, 1990-0…                 8
    ##  7   241 FALSE        Olie               24 Washington Ca…          2         711 Kolzig      293 2006-01-25, 2005-1…                 8
    ##  8   242 FALSE        Mike               18 St. Louis Blu…          2         347 Liut        133 1982-02-25                          9
    ##  9   243 FALSE        Kirk               20 Vancouver Can…          2         516 McLean      228 1996-10-19                          9
    ## 10   244 FALSE        Gilles             13 Cleveland Bar…          2         250 Meloche     140 1973-10-21                         11
    ## # … with 1,068 more rows, and 18 more variables: mostSavesDates <chr>, mostSavesOneGame <int>, mostShotsAgainstDates <chr>,
    ## #   mostShotsAgainstOneGame <int>, mostShutoutsOneSeason <int>, mostShutoutsSeasonIds <chr>, mostWinsOneSeason <int>,
    ## #   mostWinsSeasonIds <chr>, overtimeLosses <int>, playerId <int>, positionCode <chr>, rookieGamesPlayed <int>, rookieShutouts <int>,
    ## #   rookieWins <int>, seasons <int>, shutouts <int>, ties <int>, wins <int>

``` r
# read from skater records endpoint for all teams
skater.records2 <- get.nhl.data(table.name = "franchise-skater-records")
skater.records2
```

    ## # A tibble: 17,209 x 31
    ##       id activePlayer assists firstName franchiseId franchiseName  gameTypeId gamesPlayed goals lastName mostAssistsGameDates          
    ##    <int> <lgl>          <int> <chr>           <int> <chr>               <int>       <int> <int> <chr>    <chr>                         
    ##  1 16888 FALSE            417 George              5 Toronto Maple…          2        1188   296 Armstro… 1956-01-07, 1957-03-16, 1957-…
    ##  2 16889 FALSE              0 Billy               2 Montreal Wand…          2           2     1 Bell     1917-12-19, 1917-12-29        
    ##  3 16890 FALSE            794 Johnny              6 Boston Bruins           2        1436   545 Bucyk    1971-01-01                    
    ##  4 16891 FALSE            712 Jean                1 Montréal Cana…          2        1125   507 Beliveau 1955-02-19, 1956-12-01, 1962-…
    ##  5 16892 FALSE           1111 Ray                 6 Boston Bruins           2        1518   395 Bourque  1990-02-18, 1994-01-02        
    ##  6 16893 FALSE             33 Harold              9 Philadelphia …          2         216    60 Darragh  1926-01-19, 1929-11-19, 1929-…
    ##  7 16894 FALSE             13 Herb                9 Philadelphia …          2         216    24 Drury    1926-02-06, 1926-03-04, 1926-…
    ##  8 16895 FALSE            852 Bobby              16 Philadelphia …          2        1144   358 Clarke   1976-04-01                    
    ##  9 16896 FALSE            142 Ken                23 New Jersey De…          2        1283    36 Daneyko  1999-02-13                    
    ## 10 16897 FALSE              0 Gerry               2 Montreal Wand…          2           4     0 Geran    1917-12-19, 1917-12-22, 1917-…
    ## # … with 17,199 more rows, and 20 more variables: mostAssistsOneGame <int>, mostAssistsOneSeason <int>, mostAssistsSeasonIds <chr>,
    ## #   mostGoalsGameDates <chr>, mostGoalsOneGame <int>, mostGoalsOneSeason <int>, mostGoalsSeasonIds <chr>,
    ## #   mostPenaltyMinutesOneSeason <int>, mostPenaltyMinutesSeasonIds <chr>, mostPointsGameDates <chr>, mostPointsOneGame <int>,
    ## #   mostPointsOneSeason <int>, mostPointsSeasonIds <chr>, penaltyMinutes <int>, playerId <int>, points <int>, positionCode <chr>,
    ## #   rookieGamesPlayed <int>, rookiePoints <int>, seasons <int>

``` r
# find common columns between the two tibbles
common.names <- intersect(names(goalie.records2), names(skater.records2))
common.names
```

    ##  [1] "id"                "activePlayer"      "firstName"         "franchiseId"       "franchiseName"     "gameTypeId"       
    ##  [7] "gamesPlayed"       "lastName"          "playerId"          "positionCode"      "rookieGamesPlayed" "seasons"

``` r
# only keep common columns in each tibble
goalie.records2 <- goalie.records2 %>% select(all_of(common.names))
skater.records2 <- skater.records2 %>% select(all_of(common.names))

# combine the 2 tibbles together
players.record.all <- rbind(goalie.records2, skater.records2)
players.record.all
```

    ## # A tibble: 18,287 x 12
    ##       id activePlayer firstName franchiseId franchiseName       gameTypeId gamesPlayed lastName playerId positionCode rookieGamesPlayed
    ##    <int> <lgl>        <chr>           <int> <chr>                    <int>       <int> <chr>       <int> <chr>                    <int>
    ##  1   235 FALSE        Don                15 Dallas Stars                 2         315 Beaupre   8445381 G                           44
    ##  2   236 FALSE        Bob                28 Arizona Coyotes              2         281 Essensa   8446719 G                           36
    ##  3   237 FALSE        Tony               11 Chicago Blackhawks           2         873 Esposito  8446720 G                           63
    ##  4   238 FALSE        Grant              25 Edmonton Oilers              2         423 Fuhr      8446991 G                           48
    ##  5   239 FALSE        Ron                16 Philadelphia Flyers          2         489 Hextall   8447775 G                           66
    ##  6   240 FALSE        Curtis             18 St. Louis Blues              2         280 Joseph    8448382 G                           30
    ##  7   241 FALSE        Olie               24 Washington Capitals          2         711 Kolzig    8448535 G                           14
    ##  8   242 FALSE        Mike               18 St. Louis Blues              2         347 Liut      8448865 G                           NA
    ##  9   243 FALSE        Kirk               20 Vancouver Canucks            2         516 McLean    8449474 G                           41
    ## 10   244 FALSE        Gilles             13 Cleveland Barons             2         250 Meloche   8449550 G                           56
    ## # … with 18,277 more rows, and 1 more variable: seasons <int>

``` r
# find common columns between the above tibble and the previous division tibble
common.names.players <- intersect(names(players.record.all), names(division))
common.names.players
```

    ## [1] "franchiseId"

``` r
# combine 2 tibbles together (matching rows from division to players.record.all) using franchiseId as index
new.data.players <- left_join(players.record.all, division, by="franchiseId")
new.data.players
```

    ## # A tibble: 18,287 x 18
    ##       id activePlayer firstName franchiseId franchiseName       gameTypeId gamesPlayed lastName playerId positionCode rookieGamesPlayed
    ##    <int> <lgl>        <chr>           <int> <chr>                    <int>       <int> <chr>       <int> <chr>                    <int>
    ##  1   235 FALSE        Don                15 Dallas Stars                 2         315 Beaupre   8445381 G                           44
    ##  2   236 FALSE        Bob                28 Arizona Coyotes              2         281 Essensa   8446719 G                           36
    ##  3   237 FALSE        Tony               11 Chicago Blackhawks           2         873 Esposito  8446720 G                           63
    ##  4   238 FALSE        Grant              25 Edmonton Oilers              2         423 Fuhr      8446991 G                           48
    ##  5   239 FALSE        Ron                16 Philadelphia Flyers          2         489 Hextall   8447775 G                           66
    ##  6   240 FALSE        Curtis             18 St. Louis Blues              2         280 Joseph    8448382 G                           30
    ##  7   241 FALSE        Olie               24 Washington Capitals          2         711 Kolzig    8448535 G                           14
    ##  8   242 FALSE        Mike               18 St. Louis Blues              2         347 Liut      8448865 G                           NA
    ##  9   243 FALSE        Kirk               20 Vancouver Canucks            2         516 McLean    8449474 G                           41
    ## 10   244 FALSE        Gilles             13 Cleveland Barons             2         250 Meloche   8449550 G                           56
    ## # … with 18,277 more rows, and 7 more variables: seasons <int>, venue.name <chr>, venue.city <chr>, division.id <int>,
    ## #   division.name <chr>, conference.id <int>, conference.name <chr>

## Contingency Tables

After grabbing data and combining them into new tables, we can now
create contingency tables. Contingency tables summarize the relationship
between several categorical variables. Here we looks at 2 categorical
variables at a time to examine their relationship. First we look at each
categorical variable separately, then we can use the `table` and `kable`
functions to create nice contingency tables.

Here we first look at how many active and inactive players are in each
franchise. Then we also look at what proportion of total active/inactive
players are in each franchise.

``` r
# see how many players are in each team
table(players.record.all$franchiseName)
```

    ## 
    ##         Anaheim Ducks       Arizona Coyotes         Boston Bruins    Brooklyn Americans        Buffalo Sabres        Calgary Flames 
    ##                   402                   590                   969                   152                   508                   602 
    ##   Carolina Hurricanes    Chicago Blackhawks      Cleveland Barons    Colorado Avalanche Columbus Blue Jackets          Dallas Stars 
    ##                   525                   928                   145                   540                   289                   656 
    ##     Detroit Red Wings       Edmonton Oilers      Florida Panthers       Hamilton Tigers     Los Angeles Kings        Minnesota Wild 
    ##                   912                   559                   410                    39                   651                   268 
    ##    Montréal Canadiens      Montreal Maroons    Montreal Wanderers   Nashville Predators     New Jersey Devils    New York Islanders 
    ##                   838                    84                    12                   298                   559                   544 
    ##      New York Rangers       Ottawa Senators   Philadelphia Flyers  Philadelphia Quakers   Pittsburgh Penguins       San Jose Sharks 
    ##                  1038                   393                   617                    42                   711                   354 
    ##       St. Louis Blues      St. Louis Eagles   Tampa Bay Lightning   Toronto Maple Leafs     Vancouver Canucks  Vegas Golden Knights 
    ##                   653                    94                   408                   971                   615                    61 
    ##   Washington Capitals         Winnipeg Jets 
    ##                   552                   298

``` r
# see how many active and inactive players are in each time
table(players.record.all$activePlayer)
```

    ## 
    ## FALSE  TRUE 
    ## 16171  2116

``` r
# find the count of active/inactive players by franchise
contingency.table.players <- table(players.record.all$franchiseName, players.record.all$activePlayer) %>% 
  kable(caption = "Contingency Table of Active (TRUE) / Inactive (FALSE) Player Count by Franchise")
contingency.table.players
```

<table>
<caption>
Contingency Table of Active (TRUE) / Inactive (FALSE) Player Count by
Franchise
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
FALSE
</th>
<th style="text-align:right;">
TRUE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Anaheim Ducks
</td>
<td style="text-align:right;">
323
</td>
<td style="text-align:right;">
79
</td>
</tr>
<tr>
<td style="text-align:left;">
Arizona Coyotes
</td>
<td style="text-align:right;">
520
</td>
<td style="text-align:right;">
70
</td>
</tr>
<tr>
<td style="text-align:left;">
Boston Bruins
</td>
<td style="text-align:right;">
894
</td>
<td style="text-align:right;">
75
</td>
</tr>
<tr>
<td style="text-align:left;">
Brooklyn Americans
</td>
<td style="text-align:right;">
152
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Buffalo Sabres
</td>
<td style="text-align:right;">
425
</td>
<td style="text-align:right;">
83
</td>
</tr>
<tr>
<td style="text-align:left;">
Calgary Flames
</td>
<td style="text-align:right;">
538
</td>
<td style="text-align:right;">
64
</td>
</tr>
<tr>
<td style="text-align:left;">
Carolina Hurricanes
</td>
<td style="text-align:right;">
459
</td>
<td style="text-align:right;">
66
</td>
</tr>
<tr>
<td style="text-align:left;">
Chicago Blackhawks
</td>
<td style="text-align:right;">
848
</td>
<td style="text-align:right;">
80
</td>
</tr>
<tr>
<td style="text-align:left;">
Cleveland Barons
</td>
<td style="text-align:right;">
145
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Colorado Avalanche
</td>
<td style="text-align:right;">
463
</td>
<td style="text-align:right;">
77
</td>
</tr>
<tr>
<td style="text-align:left;">
Columbus Blue Jackets
</td>
<td style="text-align:right;">
215
</td>
<td style="text-align:right;">
74
</td>
</tr>
<tr>
<td style="text-align:left;">
Dallas Stars
</td>
<td style="text-align:right;">
594
</td>
<td style="text-align:right;">
62
</td>
</tr>
<tr>
<td style="text-align:left;">
Detroit Red Wings
</td>
<td style="text-align:right;">
854
</td>
<td style="text-align:right;">
58
</td>
</tr>
<tr>
<td style="text-align:left;">
Edmonton Oilers
</td>
<td style="text-align:right;">
496
</td>
<td style="text-align:right;">
63
</td>
</tr>
<tr>
<td style="text-align:left;">
Florida Panthers
</td>
<td style="text-align:right;">
335
</td>
<td style="text-align:right;">
75
</td>
</tr>
<tr>
<td style="text-align:left;">
Hamilton Tigers
</td>
<td style="text-align:right;">
39
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Los Angeles Kings
</td>
<td style="text-align:right;">
586
</td>
<td style="text-align:right;">
65
</td>
</tr>
<tr>
<td style="text-align:left;">
Minnesota Wild
</td>
<td style="text-align:right;">
208
</td>
<td style="text-align:right;">
60
</td>
</tr>
<tr>
<td style="text-align:left;">
Montréal Canadiens
</td>
<td style="text-align:right;">
768
</td>
<td style="text-align:right;">
70
</td>
</tr>
<tr>
<td style="text-align:left;">
Montreal Maroons
</td>
<td style="text-align:right;">
84
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Montreal Wanderers
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Nashville Predators
</td>
<td style="text-align:right;">
230
</td>
<td style="text-align:right;">
68
</td>
</tr>
<tr>
<td style="text-align:left;">
New Jersey Devils
</td>
<td style="text-align:right;">
492
</td>
<td style="text-align:right;">
67
</td>
</tr>
<tr>
<td style="text-align:left;">
New York Islanders
</td>
<td style="text-align:right;">
486
</td>
<td style="text-align:right;">
58
</td>
</tr>
<tr>
<td style="text-align:left;">
New York Rangers
</td>
<td style="text-align:right;">
966
</td>
<td style="text-align:right;">
72
</td>
</tr>
<tr>
<td style="text-align:left;">
Ottawa Senators
</td>
<td style="text-align:right;">
309
</td>
<td style="text-align:right;">
84
</td>
</tr>
<tr>
<td style="text-align:left;">
Philadelphia Flyers
</td>
<td style="text-align:right;">
556
</td>
<td style="text-align:right;">
61
</td>
</tr>
<tr>
<td style="text-align:left;">
Philadelphia Quakers
</td>
<td style="text-align:right;">
42
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Pittsburgh Penguins
</td>
<td style="text-align:right;">
628
</td>
<td style="text-align:right;">
83
</td>
</tr>
<tr>
<td style="text-align:left;">
San Jose Sharks
</td>
<td style="text-align:right;">
292
</td>
<td style="text-align:right;">
62
</td>
</tr>
<tr>
<td style="text-align:left;">
St. Louis Blues
</td>
<td style="text-align:right;">
595
</td>
<td style="text-align:right;">
58
</td>
</tr>
<tr>
<td style="text-align:left;">
St. Louis Eagles
</td>
<td style="text-align:right;">
94
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
Tampa Bay Lightning
</td>
<td style="text-align:right;">
351
</td>
<td style="text-align:right;">
57
</td>
</tr>
<tr>
<td style="text-align:left;">
Toronto Maple Leafs
</td>
<td style="text-align:right;">
892
</td>
<td style="text-align:right;">
79
</td>
</tr>
<tr>
<td style="text-align:left;">
Vancouver Canucks
</td>
<td style="text-align:right;">
543
</td>
<td style="text-align:right;">
72
</td>
</tr>
<tr>
<td style="text-align:left;">
Vegas Golden Knights
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
53
</td>
</tr>
<tr>
<td style="text-align:left;">
Washington Capitals
</td>
<td style="text-align:right;">
490
</td>
<td style="text-align:right;">
62
</td>
</tr>
<tr>
<td style="text-align:left;">
Winnipeg Jets
</td>
<td style="text-align:right;">
239
</td>
<td style="text-align:right;">
59
</td>
</tr>
</tbody>
</table>

``` r
# find the proportion by margin=2 (by column)
contingency.table.players.prop <- table(players.record.all$franchiseName, players.record.all$activePlayer) %>%
  prop.table(margin=2)*100
contingency.table.players.prop %>% 
  kable(caption = "Proportion of all Active (TRUE) / Inactive (FALSE) Players by Franchise") 
```

<table>
<caption>
Proportion of all Active (TRUE) / Inactive (FALSE) Players by Franchise
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
FALSE
</th>
<th style="text-align:right;">
TRUE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Anaheim Ducks
</td>
<td style="text-align:right;">
1.9974028
</td>
<td style="text-align:right;">
3.733459
</td>
</tr>
<tr>
<td style="text-align:left;">
Arizona Coyotes
</td>
<td style="text-align:right;">
3.2156329
</td>
<td style="text-align:right;">
3.308129
</td>
</tr>
<tr>
<td style="text-align:left;">
Boston Bruins
</td>
<td style="text-align:right;">
5.5284151
</td>
<td style="text-align:right;">
3.544423
</td>
</tr>
<tr>
<td style="text-align:left;">
Brooklyn Americans
</td>
<td style="text-align:right;">
0.9399542
</td>
<td style="text-align:right;">
0.000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Buffalo Sabres
</td>
<td style="text-align:right;">
2.6281615
</td>
<td style="text-align:right;">
3.922495
</td>
</tr>
<tr>
<td style="text-align:left;">
Calgary Flames
</td>
<td style="text-align:right;">
3.3269433
</td>
<td style="text-align:right;">
3.024575
</td>
</tr>
<tr>
<td style="text-align:left;">
Carolina Hurricanes
</td>
<td style="text-align:right;">
2.8384144
</td>
<td style="text-align:right;">
3.119093
</td>
</tr>
<tr>
<td style="text-align:left;">
Chicago Blackhawks
</td>
<td style="text-align:right;">
5.2439552
</td>
<td style="text-align:right;">
3.780718
</td>
</tr>
<tr>
<td style="text-align:left;">
Cleveland Barons
</td>
<td style="text-align:right;">
0.8966669
</td>
<td style="text-align:right;">
0.000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Colorado Avalanche
</td>
<td style="text-align:right;">
2.8631501
</td>
<td style="text-align:right;">
3.638941
</td>
</tr>
<tr>
<td style="text-align:left;">
Columbus Blue Jackets
</td>
<td style="text-align:right;">
1.3295405
</td>
<td style="text-align:right;">
3.497164
</td>
</tr>
<tr>
<td style="text-align:left;">
Dallas Stars
</td>
<td style="text-align:right;">
3.6732422
</td>
<td style="text-align:right;">
2.930057
</td>
</tr>
<tr>
<td style="text-align:left;">
Detroit Red Wings
</td>
<td style="text-align:right;">
5.2810587
</td>
<td style="text-align:right;">
2.741021
</td>
</tr>
<tr>
<td style="text-align:left;">
Edmonton Oilers
</td>
<td style="text-align:right;">
3.0672191
</td>
<td style="text-align:right;">
2.977316
</td>
</tr>
<tr>
<td style="text-align:left;">
Florida Panthers
</td>
<td style="text-align:right;">
2.0716097
</td>
<td style="text-align:right;">
3.544423
</td>
</tr>
<tr>
<td style="text-align:left;">
Hamilton Tigers
</td>
<td style="text-align:right;">
0.2411725
</td>
<td style="text-align:right;">
0.000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Los Angeles Kings
</td>
<td style="text-align:right;">
3.6237709
</td>
<td style="text-align:right;">
3.071834
</td>
</tr>
<tr>
<td style="text-align:left;">
Minnesota Wild
</td>
<td style="text-align:right;">
1.2862532
</td>
<td style="text-align:right;">
2.835539
</td>
</tr>
<tr>
<td style="text-align:left;">
Montréal Canadiens
</td>
<td style="text-align:right;">
4.7492425
</td>
<td style="text-align:right;">
3.308129
</td>
</tr>
<tr>
<td style="text-align:left;">
Montreal Maroons
</td>
<td style="text-align:right;">
0.5194484
</td>
<td style="text-align:right;">
0.000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Montreal Wanderers
</td>
<td style="text-align:right;">
0.0742069
</td>
<td style="text-align:right;">
0.000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Nashville Predators
</td>
<td style="text-align:right;">
1.4222992
</td>
<td style="text-align:right;">
3.213611
</td>
</tr>
<tr>
<td style="text-align:left;">
New Jersey Devils
</td>
<td style="text-align:right;">
3.0424835
</td>
<td style="text-align:right;">
3.166352
</td>
</tr>
<tr>
<td style="text-align:left;">
New York Islanders
</td>
<td style="text-align:right;">
3.0053800
</td>
<td style="text-align:right;">
2.741021
</td>
</tr>
<tr>
<td style="text-align:left;">
New York Rangers
</td>
<td style="text-align:right;">
5.9736565
</td>
<td style="text-align:right;">
3.402646
</td>
</tr>
<tr>
<td style="text-align:left;">
Ottawa Senators
</td>
<td style="text-align:right;">
1.9108280
</td>
<td style="text-align:right;">
3.969754
</td>
</tr>
<tr>
<td style="text-align:left;">
Philadelphia Flyers
</td>
<td style="text-align:right;">
3.4382537
</td>
<td style="text-align:right;">
2.882798
</td>
</tr>
<tr>
<td style="text-align:left;">
Philadelphia Quakers
</td>
<td style="text-align:right;">
0.2597242
</td>
<td style="text-align:right;">
0.000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Pittsburgh Penguins
</td>
<td style="text-align:right;">
3.8834951
</td>
<td style="text-align:right;">
3.922495
</td>
</tr>
<tr>
<td style="text-align:left;">
San Jose Sharks
</td>
<td style="text-align:right;">
1.8057016
</td>
<td style="text-align:right;">
2.930057
</td>
</tr>
<tr>
<td style="text-align:left;">
St. Louis Blues
</td>
<td style="text-align:right;">
3.6794261
</td>
<td style="text-align:right;">
2.741021
</td>
</tr>
<tr>
<td style="text-align:left;">
St. Louis Eagles
</td>
<td style="text-align:right;">
0.5812875
</td>
<td style="text-align:right;">
0.000000
</td>
</tr>
<tr>
<td style="text-align:left;">
Tampa Bay Lightning
</td>
<td style="text-align:right;">
2.1705522
</td>
<td style="text-align:right;">
2.693762
</td>
</tr>
<tr>
<td style="text-align:left;">
Toronto Maple Leafs
</td>
<td style="text-align:right;">
5.5160472
</td>
<td style="text-align:right;">
3.733459
</td>
</tr>
<tr>
<td style="text-align:left;">
Vancouver Canucks
</td>
<td style="text-align:right;">
3.3578628
</td>
<td style="text-align:right;">
3.402646
</td>
</tr>
<tr>
<td style="text-align:left;">
Vegas Golden Knights
</td>
<td style="text-align:right;">
0.0494713
</td>
<td style="text-align:right;">
2.504726
</td>
</tr>
<tr>
<td style="text-align:left;">
Washington Capitals
</td>
<td style="text-align:right;">
3.0301156
</td>
<td style="text-align:right;">
2.930057
</td>
</tr>
<tr>
<td style="text-align:left;">
Winnipeg Jets
</td>
<td style="text-align:right;">
1.4779544
</td>
<td style="text-align:right;">
2.788280
</td>
</tr>
</tbody>
</table>

We also create a second contingency table to look at how many franchises
within a specific division had over 1500 total game wins. We also look
at each categorical variable separately first, then combine the two
variables into a contingency table. We also look at what proportion of
teams within each division had over 1500 wins.

``` r
# see how many teams are in each division
table(new.data$division.name)
```

    ## 
    ## Discover Central       Honda West  MassMutual East     Scotia North 
    ##                8                8                8                7

``` r
# see how many games are won among all teams
table(new.data$wins)
```

    ## 
    ##  173  214  382  678  759  827  852  889  971  985  990 1007 1070 1084 1394 1469 1497 1649 1688 1700 1754 1805 1903 1929 2079 2812 2873 
    ##    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1 
    ## 2883 2891 3241 3473 
    ##    1    1    1    1

``` r
# find the count of franchises w/i a division that had over 1500 wins
contingency.table2 <- table(new.data$division.name, new.data$wins>1500) %>%
  kable(caption = "Contingency Table of Number of Franchises in a Division that had Over 1500 Wins")
contingency.table2
```

<table>
<caption>
Contingency Table of Number of Franchises in a Division that had Over
1500 Wins
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
FALSE
</th>
<th style="text-align:right;">
TRUE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Discover Central
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Honda West
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
MassMutual East
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Scotia North
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
3
</td>
</tr>
</tbody>
</table>

``` r
# find the proportion by margin=1 (by rows)
contingency.table2.prop <- table(new.data$division.name, new.data$wins>1500) %>%
  prop.table(margin=1)*100
contingency.table2.prop %>%
  kable(caption = "Proportion of Teams with Over 1500 Win within Each Division")
```

<table>
<caption>
Proportion of Teams with Over 1500 Win within Each Division
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
FALSE
</th>
<th style="text-align:right;">
TRUE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Discover Central
</td>
<td style="text-align:right;">
75.00000
</td>
<td style="text-align:right;">
25.00000
</td>
</tr>
<tr>
<td style="text-align:left;">
Honda West
</td>
<td style="text-align:right;">
75.00000
</td>
<td style="text-align:right;">
25.00000
</td>
</tr>
<tr>
<td style="text-align:left;">
MassMutual East
</td>
<td style="text-align:right;">
12.50000
</td>
<td style="text-align:right;">
87.50000
</td>
</tr>
<tr>
<td style="text-align:left;">
Scotia North
</td>
<td style="text-align:right;">
57.14286
</td>
<td style="text-align:right;">
42.85714
</td>
</tr>
</tbody>
</table>

## Numerical Summaries for Categorical Variables

With lots of data to play with, we can create numerical summaries for
the categorical variables. We can specifically look at each setting of a
particular categorical variable and see summaries of the quantitative
variables. We look at a few numerical summaries below.

``` r
# find the average games played by each position
new.data.players %>% group_by(positionCode) %>% 
  summarize(avg.games.played = mean(gamesPlayed))
```

    ## # A tibble: 5 x 2
    ##   positionCode avg.games.played
    ##   <chr>                   <dbl>
    ## 1 C                       121. 
    ## 2 D                       117. 
    ## 3 G                        89.4
    ## 4 L                       111. 
    ## 5 R                       118.

``` r
# find the average games played by each position and division name
new.data.players %>% drop_na() %>% group_by(positionCode, division.name) %>% 
  summarize(avg.games.played = mean(gamesPlayed))
```

    ## # A tibble: 20 x 3
    ## # Groups:   positionCode [5]
    ##    positionCode division.name    avg.games.played
    ##    <chr>        <chr>                       <dbl>
    ##  1 C            Discover Central             150.
    ##  2 C            Honda West                   143.
    ##  3 C            MassMutual East              140.
    ##  4 C            Scotia North                 151.
    ##  5 D            Discover Central             139.
    ##  6 D            Honda West                   143.
    ##  7 D            MassMutual East              143.
    ##  8 D            Scotia North                 153.
    ##  9 G            Discover Central             329.
    ## 10 G            Honda West                   152.
    ## 11 G            MassMutual East              327.
    ## 12 G            Scotia North                 238.
    ## 13 L            Discover Central             125.
    ## 14 L            Honda West                   139.
    ## 15 L            MassMutual East              132.
    ## 16 L            Scotia North                 125.
    ## 17 R            Discover Central             132.
    ## 18 R            Honda West                   138.
    ## 19 R            MassMutual East              144.
    ## 20 R            Scotia North                 153.

``` r
# find the 5 number summary (and mean) of the percentage of total games won by all teams
new.data %>% select(perc.total.games.win) %>% summary()
```

    ##  perc.total.games.win
    ##  Min.   :39.93       
    ##  1st Qu.:44.16       
    ##  Median :45.94       
    ##  Mean   :46.56       
    ##  3rd Qu.:48.67       
    ##  Max.   :59.45

``` r
# find the 5 number summary (and mean) of the percentage of total games lost by all teams
new.data %>% select(perc.total.games.loss) %>% summary()
```

    ##  perc.total.games.loss
    ##  Min.   :32.30        
    ##  1st Qu.:38.55        
    ##  Median :40.38        
    ##  Mean   :40.02        
    ##  3rd Qu.:41.73        
    ##  Max.   :48.88

``` r
# find the average percent of games won for home vs. road games by division
new.data %>% select(-3) %>% drop_na() %>% 
  group_by(division.name) %>% 
  summarize(avg.perc.home.win = mean(perc.home.win), avg.perc.road.win = mean(perc.road.win))
```

    ## # A tibble: 4 x 3
    ##   division.name    avg.perc.home.win avg.perc.road.win
    ##   <chr>                        <dbl>             <dbl>
    ## 1 Discover Central              51.7              39.6
    ## 2 Honda West                    52.9              41.0
    ## 3 MassMutual East               53.6              39.4
    ## 4 Scotia North                  52.5              38.9

``` r
# find the average percent of games lost for home vs. road games by division
new.data %>% select(-3) %>% drop_na() %>% 
  group_by(division.name) %>% 
  summarize(avg.perc.home.loss = mean(perc.home.loss), avg.perc.road.loss = mean(perc.road.loss))
```

    ## # A tibble: 4 x 3
    ##   division.name    avg.perc.home.loss avg.perc.road.loss
    ##   <chr>                         <dbl>              <dbl>
    ## 1 Discover Central               34.4               46.3
    ## 2 Honda West                     33.9               45.5
    ## 3 MassMutual East                32.7               46.4
    ## 4 Scotia North                   33.8               47.2

## Plots

A fun part of exploratory data analysis is creating visuals for more
eye-catching representation of the data. We will create bar plots,
histograms, box plot, and scatter plots using different data to
demonstrate this.

### Bar plot

We create 2 bar plots. The first bar plot looks at how many franchises
are in each division with indication of conference in a stacked
position. As someone who’s not familiar with hockey, this is a good
starting point to get to know the basics of NHL. We see from this bar
plot that 3 of the 4 divisions have 8 franchises, the fourth division
only has 7 franchises. What makes this plot interesting, contrary to my
initial thought, is that there are not 2 divisions per conference.
Although we see that all of the 8 franchises in the Honda West division
belong to the Western Conference, and all of the 8 franchises in the
MassMutual East division belong to the Eastern Conference, for the other
two divisions we have almost half of the franchises belonging to the
Eastern Conference while the other half belonging to the Western
Conference. This makes me want to investigate further how a team is
classified into a division or a conference.

``` r
ggplot(data = new.data, aes(x = division.name)) + 
  geom_bar(aes(fill = as.factor(conference.name)), position = "stack") + 
  labs(x = "Division", title = "Bar Plot of Number of Franchise in Each Division with Indication of Conference") + 
  scale_fill_discrete(name = "Conference Name", labels = c("Eastern Conference", "Western Conference"))
```

![](README_files/figure-gfm/plot%20-%20bar%20-%20new.data-1.png)<!-- -->

For the second bar plot, we look at the number of players by position
with indication of their active or inactive statuses. The bars are shown
in a dodge position. We see out of all the players in the table, the
position with the most players is defense, position with the least
players is goalie (which makes sense, since we do don’t typically need
many goalies per team). The table has records of a lot more inactive
players compared to active players, this is clearly shown by the height
of pink and blue bars. We have similar numbers of active center and
defence players, but a lot more defence players are inactive compared to
the center players. After looking at this plot, I’d want to look up how
many players are needed to play in a hockey game, and how many are for
each position.

``` r
ggplot(data = players.record.all, aes(x = positionCode)) + 
  geom_bar(aes(fill = as.factor(activePlayer)), position = "dodge") + 
  labs(x = "Position", title = "Number of Players by Position with Indication of Active or Inactive Status") + 
  scale_x_discrete(labels = c("Center", "Defense", "Goalie", "Left Wing", "Right Wing")) +
  scale_fill_discrete(name = "Active/Inactive Players", labels = c("Inactive", "Active"))
```

![](README_files/figure-gfm/plot%20-%20bar%20-%20players.record.all-1.png)<!-- -->

### Histogram

We also create 2 histograms. For the first histogram we look at how many
games each player has played. Based on this histogram, we see majority
of the players have played up to around 100 games, while the most games
played is above 1500, which is shockingly impressive. We can confirm
this by doing a `summary` function on the gamesPlayed column of the
`players.record.all` table, the result indicates the maximum numbers of
games played is 1687, while the mean number of games played is around
115. With this result, I’d want to look up how many seasons of games a
typical NHL player plays in his career, and how many games are typically
in a season.

``` r
ggplot(data = players.record.all, aes(x = gamesPlayed)) +
  geom_histogram(color = "purple", fill = "orange") +
  labs(x = "Games Played", title = "Number of Games Played by each (of all) Players")
```

![](README_files/figure-gfm/plot%20-%20histogram%20-%20players.record.all-1.png)<!-- -->

``` r
summary(players.record.all$gamesPlayed)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     1.0    14.0    53.0   115.2   144.0  1687.0

The second histogram breaks up the histogram by position of the players,
this is done by adding a facet layer of positions on top of the
histogram layer. We see the shape of the distribution of games played
for each position follows similar patterns, although the height of bars
are different, this is most likely contributed by the different numbers
of players for each position previously shown by the bar plot. We cannot
quite pinpoint which positioned player played 1687 games based on this
histogram.

``` r
ggplot(data = players.record.all, aes(x = gamesPlayed)) +
  geom_histogram(color = "purple", fill = "orange") +
  facet_grid(~ positionCode, 
             labeller = as_labeller(c(C = "Center", D = "Defense", G = "Goalie", L = "Left Wing", R = "Right Wing"))) +
  labs(x = "Games Played", title = "Number of Games Played by each (of all) Players by Position Played")
```

![](README_files/figure-gfm/plot%20-%20histogram%20facet%20-%20player.record.all-1.png)<!-- -->

### Box Plot

For this box plot, we look at how many games are played for each
division, the mean games played for division indicated by each
conference is shown by the two lines. Box plots show the 5 number
summary with dots indicating possible outliers. Based on this box plot,
we see that the numbers of games played for the Honda West division is
generally lower than the other 3 divisions. The games played in the
Scotia North division seems to be the most varied compared to the other
3 division, with the widest spread. We see the medium games played for
the Discover Central and Honda West divisions are about the same, but
other measures are quite different including minimum, 25th percentile,
75th percentile and the maximum number of games. We see there are a
couple of possible outlier values for both the Discover Central and
MassMutual East divisions. Looking at the mean values indicated also by
conference name, we see the Western Conference blue line doesn’t cross
the MassMutual East division’s box plot at all, this makes sense since
we saw from previous plots that all of the 8 teams in this division
belong to the Eastern Conference. Similarly, the red line crosses the
box plot of Honda West at a strange location, this is due to the fact
that all of the 8 teams in the Honda West division belong in the Western
Conference. Therefore, means are not calculated for these two divisions
for these two conferences. For the Discover Central division, Western
Conference teams have a higher mean games played compared to the Eastern
Conference teams, this is interesting because we saw from previous plots
that there are actually more Eastern Conference teams in this division.
And for the Scotia North division, the mean number of games played for
the Eastern Conference teams are a lot higher than those from the
Western Conference teams, this is also interesting because there are
only slightly more teams in the Western Conference than those in the
Eastern Conference. So based off this plot, we can conclude that the
number of games played vary greatly depending on which division a
particular team’s in, the mean number of games played can also vary
greatly based on the conference.

``` r
ggplot(data = new.data, aes(x = division.name, y = gamesPlayed)) +
  geom_boxplot(fill = "maroon") +
  stat_summary(fun = mean, geom = "line", aes(group = conference.name, col = conference.name)) +
  labs(x = "Division Name", y = "# of Games Played", title = "Number of Games Played for Each Division with Mean Games Played for Each Conference")
```

![](README_files/figure-gfm/plot%20-%20box%20plot%20-%20new.data-1.png)<!-- -->

### Scatter Plot

We now look at a few scatter plots. For the first scatter plot, we look
at the number of games won versus the number of games lost for each of
the 31 active franchises. Each dot represents a franchise, the blue line
represents the generalized additive model smoother, while the yellow
line represents the best fit linear model smoother. I also added a black
diagonal abline representing the 1 to 1 ratio of wins and losses.
Looking specifically at the black line, we see all but 4 teams are below
the line, indicating that these teams have higher numbers of wins than
losses. The best fit linear model line has a slightly smaller slope but
does not fit all of the data well. The blue generalized additive model
smoother fits the data better.

``` r
ggplot(data = new.data, aes(x = wins, y = losses)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +  ## diagonal line indicating 1-1 ratio for win/losses
  geom_smooth() +   ## generalized additive model smoother in blue
  geom_smooth(method = lm, col = "yellow") +  ## best fit linear model smoother
  labs(x = "Games Won", y = "Games Lost", title = "Number of Games Won vs. Number of Games Lost for Each Franchise")
```

![](README_files/figure-gfm/plot%20-%20scatter%20-%20new.data-1.png)<!-- -->

Next we look at the same data but break up the 31 teams into each
conference, this is done by adding a facet layer to the scatter plot
layer. Looking specifically at the black diagonal line, we see that all
except 1 team in the Eastern Conference are below the line (teams below
the line have higher number of wins compared to losses), and we have 3
teams above the line for the Western Conference. The yellow best fit
linear model smoother line has a bigger slope for the Western Conference
compared to the Eastern, indicating that teams in the Western Conference
generally have close to same number of wins and losses, while the teams
in the Eastern Conference tend to have more games won than lost. The
generalized additive model smoother for the Western Conference is very
close to the yellow best fit linear model smoother, while the
generalized additive model smoother for the Eastern Conference varies a
bit from the best fit linear model smoother towards the larger games won
values.

``` r
ggplot(data = new.data, aes(x = wins, y = losses)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +  ## diagonal line indicating 1-1 ratio for win/losses
  geom_smooth() +   ## generalized additive model smoother in blue
  geom_smooth(method = lm, col = "yellow") +  ## best fit linear model smoother
  facet_grid(~ conference.name, labeller = as_labeller(c(Eastern = "Eastern Conference", 
                                      Western = "Western Conference"))) +
  labs(x = "Games Won", y = "Games Lost", title = "Number of Games Won vs. Number of Games Lost for Franchies in Each Division")
```

![](README_files/figure-gfm/plot%20-%20scatter%20facet-1.png)<!-- -->

### Scatter Plot using Group

Next, we further subset the number of total games won and loss. For
these next 4 plots, we will use the `group=conference.name` to get a
clearer idea of which teams belong to which conference.

This first plot, we look at the number of home games won versus the
number of home games lost for each team with indication of conference.
We see for these home games, almost all teams have higher numbers of
games won compared to games lost. The purple line represent the best fit
linear model smoother for the Western Conference teams (blue dots), and
the green line represent the besst fit linear model smoother for the
Eastern Conference teams (orange dots). We see the smoother for the
Western Conference has a bigger slope than the Eastern Conference, this
seem to indicate when comparing with the Eastern Conference teams, the
Western Conference teams have less difference in the number of wins
minus losses for home games. The Eastern Conference teams tend to have
even higher number of home games won compared to home games lost.

``` r
ggplot(data = new.data, aes(x = homeWins, y = homeLosses)) +
  geom_point(aes(fill=conference.name, color=conference.name, group=conference.name)) +
  geom_abline(intercept=0, slope=1) +  ## diagonal line indicating 1-1 ratio for win/losses
  geom_smooth(data = subset(new.data, conference.name=="Eastern"),
              method = "lm", color = "springgreen") +  ## best fit linear model smoother
  geom_smooth(data = subset(new.data, conference.name=="Western"),
              method = "lm", color = "slateblue") +  ## best fit linear model smoother
  labs(x = "Home Games Won", y = "Home Games Lost", title = "Number of Home Games Won vs. Lost for Each Franchise by Conference")
```

![](README_files/figure-gfm/plot%20-%20point%20and%20group%20-%20home%20games-1.png)<!-- -->

Next we look at how many road games are won versus how many road games
are lost. Here we see we have all but 2 teams on the black line,
indicating that most of the Eastern Conference teams have higher number
road games lost compared to road games won. We have about 5 teams in the
Western Conference that are on or below the line, the other teams in
this conference also have higher number of road games lost compared to
road games won. Here the best fit linear model smoother for the Western
Conference has larger slope than the smoother for the Eastern
Conference, this seem to suggest that while more Western Conference
teams are below the black line (meaning these teams have higher number
of road game wins than losses), some of the Western Conference teams
that are above the black line will have bigger difference of number of
road games lost minus won. We can also draw a conclusion that teams tend
to do better for home games compared to road games, regardless of which
conference the team’s in.

``` r
ggplot(data = new.data, aes(x = roadWins, y = roadLosses)) +
  geom_point(aes(fill=conference.name, color=conference.name, group=conference.name)) +
  geom_abline(intercept=0, slope=1) +  ## diagonal line indicating 1-1 ratio for win/losses
  geom_smooth(data = subset(new.data, conference.name=="Eastern"),
              method = "lm", color = "springgreen") +  ## best fit linear model smoother
  geom_smooth(data = subset(new.data, conference.name=="Western"),
              method = "lm", color = "slateblue") +  ## best fit linear model smoother
  labs(x = "Road Games Won", y = "Road Games Lost", title = "Number of Road Games Won vs. Lost for Each Franchise by Conference")
```

![](README_files/figure-gfm/plot%20-%20point%20and%20group%20-%20road%20games-1.png)<!-- -->

It also is worth while to look at the percentage of home games won
versus percentage of road games won for each team as indicated by
conference. The diagonal black line is barely visible at the upper left
corner, meaning all of the teams are well below this black line. So all
of the teams have a higher percentage of home games won compared to road
games won. The best fit linear model smoother for the Eastern Conference
has a smaller slope, so these teams tend to have a larger difference in
the percentage of home games won minus percentage of road games won. But
all of these teams do way better on home games, which further confirms
our previous conclusion.

``` r
ggplot(data = new.data, aes(x = perc.home.win, y = perc.road.win)) +
  geom_point(aes(fill=conference.name, color=conference.name, group=conference.name)) +
  geom_abline(intercept=0, slope=1) +  ## diagonal line indicating 1-1 ratio for win/losses
  geom_smooth(data = subset(new.data, conference.name=="Eastern"),
              method = "lm", color = "springgreen") +  ## best fit linear model smoother
  geom_smooth(data = subset(new.data, conference.name=="Western"),
              method = "lm", color = "slateblue") +  ## best fit linear model smoother
  labs(x = "% of Home Games Won", y = "% Road Games Won", title = "% of Home Games Won vs. Road Games Won for Each Franchise by Conference")
```

![](README_files/figure-gfm/plot%20-%20point%20and%20group%20-%20perc%20won%20games-1.png)<!-- -->

Similarly, we can look at the percentage of home games lost compared to
percentage of road games lost. The diagonal black line is barely visible
at the lower right of the plot, so all of these teams are above the line
meaning they all have higher percentage of road games lost compared to
percentage of home games lost. The best fit linear model smoother is
flatter for the Eastern Conference teams, meaning these teams tend to
have larger difference of percentage of home games lost minus percentage
of road games lost. All of these teams do worse on road games.

``` r
ggplot(data = new.data, aes(x = perc.home.loss, y = perc.road.loss)) +
  geom_point(aes(fill=conference.name, color=conference.name, group=conference.name)) +
  geom_abline(intercept=0, slope=1) +  ## diagonal line indicating 1-1 ratio for win/losses
  geom_smooth(data = subset(new.data, conference.name=="Eastern"),
              method = "lm", color = "springgreen") +  ## best fit linear model smoother
  geom_smooth(data = subset(new.data, conference.name=="Western"),
              method = "lm", color = "slateblue") +  ## best fit linear model smoother
  labs(x = "% of Home Games Loss", y = "% Road Games Loss", title = "% of Home Games Win vs. Road Games Loss for Each Franchise by Conference")
```

![](README_files/figure-gfm/plot%20-%20point%20and%20group%20-%20perc%20lost%20games-1.png)<!-- -->

# Outro

I hope you found this vignette useful. If you have any suggestions,
questions, comments, please send them to me at <xyin2@ncsu.edu>. Thank
you.
