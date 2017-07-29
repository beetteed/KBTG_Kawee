#initiate R package
library(reshape2)
library(ggplot2)
library(jpeg)
library(parallel)
library(dplyr)
library(RTOMO)
library(parallel)
library(dplyr)
library(e1071)


source('R_code/Evaluation/imAccuracy.R')
source('R_code/Evaluation/imPrecision.R')
source('R_code/Evaluation/imSensitivity.R')
source('R_code/Evaluation/imSpecificity.R')
source('R_code/Evaluation/summarySE.R')
source('R_code/splitData.R')
