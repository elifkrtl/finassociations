# Finassociations: Investment Recommendation Application
# Author: Elif KARTAL, M. Erdal BALABAN, & Zeki OZEN
# Date: September 2024
# File: global.R

# Installing required packages

library(readxl)
library(shinythemes)
library(shinyWidgets)
library(tidyr)
library(tidyverse)
library(shinyjs)
library(ggplot2)
library(V8)
library(plotly)
library(hrbrthemes)
library(dplyr)
library(tidyquant)
library(purrr)
library(GGally)
library(bslib)
library(arules)
library(arulesViz)
library(visNetwork)
library(tibble)
library(sjmisc)
library(quantmod)
library(mailtoR)
library(RColorBrewer)


get_my_data <- function(data_list, stDate, enDate){
  
  myData1 <- list()
  myData2 <- list()
  
  myData <- lapply(data_list, function(sym) {
    na.omit(getSymbols(
      sym,
      from = stDate,
      to = enDate,
      auto.assign = FALSE
    ))
  })
  
  for(i in 1:length(data_list)) {
    colnames(myData[[i]]) <- 1:6
    X1 <- as.data.frame(myData[[i]]$"4")
    if (str_contains(rownames(X1)[nrow(X1)], rownames(X1)[nrow(X1) - 1])) {
      date <- rownames(X1)[1:nrow(X1) - 1]
      date <- gsub("X", "", as.character(date))
      date <- as.Date(date, "%Y.%m.%d")
      X1 <- as.data.frame(X1[1:nrow(X1) - 1, ])
      rownames(X1) <- date
    }
    myData1[[i]] <- X1
    colnames(myData1[[i]]) <- "Close"
  }
  
  myData2 <- myData1 %>%
    map(rownames_to_column, 'date') %>%
    bind_rows(.id = 'grp') %>%
    spread(grp, Close)
  
  myData3 <- as.data.frame(lapply(myData2, zoo::na.locf0, maxgap = 5))
  colnames(myData3)[-1] <- data_list
  myData3 <- myData3[order(as.Date(myData3$date), decreasing = T), ]
  rownames(myData3) <- myData3[[1]]
  myData3
}


# Codes for enable/disable tabPanels

app_jscode <-
  "shinyjs.disableTab = function(name) {
    var tab = $('.nav li a[data-value=' + name + ']');
    tab.bind('click.tab', function(e) {
      e.preventDefault();
      return false;
    });
    tab.addClass('disabled');
  }
  shinyjs.enableTab = function(name) {
    var tab = $('.nav li a[data-value=' + name + ']');
    tab.unbind('click.tab');
    tab.removeClass('disabled');
  }"
app_css <-
  ".nav li a.disabled {
    background-color: #aaa !important;
    color: #333 !important;
    cursor: not-allowed !important;
    border-color: #aaa !important;
  }"


css <- '
.tooltip {
  pointer-events: none;
}
.tooltip > .tooltip-inner {
  pointer-events: none;
  background-color: #73AD21;
  color: #FFFFFF;
  border: 1px solid green;
  padding: 10px;
  font-size: 25px;
  font-style: italic;
  text-align: justify;
  margin-left: 0;
  max-width: 1000px;
}
.tooltip > .arrow::before {
  border-right-color: #73AD21;
}
'

js <- "
$(function () {
  $('[data-toggle=tooltip]').tooltip()
})
"

my_cursInfo <- read_excel(path = "symbols.xlsx", sheet = "forex")
my_cursInfo <- setNames(as.list(my_cursInfo$curSymbol), my_cursInfo$curName)
stMarInfo <- read_excel(path = "symbols.xlsx", sheet = "stock")
stMarInfo <- setNames(as.list(stMarInfo$stSymbol), stMarInfo$stName)
cryptInfo <- read_excel(path = "symbols.xlsx", sheet = "crypto")
cryptInfo <- setNames(as.list(cryptInfo$ccurSymbol), cryptInfo$ccurName)

my_lag <- 1

# Start-End Dates
my_endDate <- as.Date(Sys.Date())
my_endDate <- as.character(my_endDate)

