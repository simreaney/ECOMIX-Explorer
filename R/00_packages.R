# Load Packages
library(leaflet)
library(shiny)
library(bslib)
library(sf)
library(dplyr)
library(ggplot2)
library(arrow)

# Also required, but only ever called namespace-qualified, so not attached:
# DT, tidyr, tibble, jsonlite, writexl, cachem, rlang.

# Decimal numbers
options(scipen = 999)
