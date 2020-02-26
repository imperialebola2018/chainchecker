# dependencies
library(plotly)
library(ggplot2)
library(epicontacts)
library(tibble)
library(dplyr)
library(lubridate)
library(data.table)
library(magrittr)
library(igraph)
library(DT)
library(GGally)
library(network)
library(plyr)
source('Functions/vis_epicontacts_ggplot.R')
source('Functions/calculator_functions.R')
source('Functions/internals.R')
source('Functions/UI_functions.R')

source('Functions/update_dictionary.R')

load("translation.bin")

