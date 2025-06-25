# Load necessary libraries
library(dplyr)
library(readxl)

DR2380 <- read.csv('../data/DR2380_onerecord.csv')
DR2380 <- DR2380[, colSums(!(is.na(DR2380) | DR2380 == "")) > 0]
SNOT2QALY <- read_excel("../data/QALYs and Costs 3.17.25.xlsx", sheet = "SNOT22s to QALYs")
MRN2PC <- SNOT2QALY[,c('Patient Number (coded)', 'mrn')]
names(MRN2PC) <- c('PC', 'MRN')
MRN2PC <- unique(MRN2PC)