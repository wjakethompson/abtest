# Cleaning ----
detach_all <- function() {
  basic.pkg <- c("package:stats", "package:graphics", "package:grDevices",
                 "package:utils", "package:datasets", "package:methods", "package:base")

  pkg.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1 ,TRUE, FALSE)]

  pkg.list <- setdiff(pkg.list, basic.pkg)

  lapply(pkg.list, detach, character.only = TRUE)
}
detach_all()
rm(list = ls())

# Loading ----
library(tidyverse)
library(rstanarm)
library(abtest)

library(shiny)
library(shinydashboard)
library(tippy)
library(shinycssloaders)
library(shinytest)
library(htmltools)
library(ggdist)
library(wjake)
library(ggtext)
library(glue)

library(showtext)
font_add_google("Source Sans Pro", "Source Sans Pro")

source("utils/theme.R")
source("utils/logo.R")
