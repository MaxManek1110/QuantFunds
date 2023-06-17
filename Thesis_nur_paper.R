rm(list=ls())

###################################
#Loading of libraries and datasets#
###################################

#Loading of libraries
library(readxl)
library(dplyr)
library(lmtest)
library(dplyr)
library(openxlsx)
library(lubridate)
library(ggplot2)
library(Matrix)
library(glmnet)
library(psych)
library(moments)
library(corrplot)


#Loading of datasets
data_etf <- read.xlsx('data_etf_d.xlsx',startRow = 1,detectDates = TRUE)
data_mutual <- read.csv("Data_Mutual.csv")
ff_d <- read.csv("ff_daily.csv")
ff_d <- ff_d[19276:25461,]
sp500 <- read.xlsx('sp500.xlsx')
vix <- read.xlsx('vix.xlsx')
data_mutual_m <- read.xlsx('data_mutual_m.xlsx', startRow = 1,detectDates = TRUE)
data_mutual_m$date <- format(data_mutual_m$date, "%Y-%m")
data_etf  <- data_etf[1:1824,]
data_etf <- data_etf[order(data_etf$Date), ]
names(data_etf) <- c("date", "qPF1", "qPF2", "qPF3", "qPF4", "qPF5", "qPF6", 
                 "qPF7", "qPF8", "qPF9", "qPF10","qPF11", "qPF12", "qPF13", "qPF14","qPF15", "qPF16", "qPF17", "qPF18","qPF19", "qPF20", "qPF21", "MKT","MKT.RF","RF","SMB","HML","sp500","vix")
data_etf_m <- read.xlsx('data_etf_m.xlsx',startRow = 1,detectDates = TRUE)
data_etf_m$Date <- format(data_etf_m$Date, "%Y-%m")
data_etf_m <- data_etf_m[order(data_etf_m$Date), ]
names(data_etf_m) <- c("date", "qPF1", "qPF2", "qPF3", "qPF4", "qPF5", "qPF6", 
                     "qPF7", "qPF8", "qPF9", "qPF10","qPF11", "qPF12", "qPF13", "qPF14","qPF15", "qPF16", "qPF17", "qPF18","qPF19", "qPF20", "qPF21","qPF22","qPF23","qPF24","qPF25", "MKT.RF","MKT","SMB","HML","WML","RF","interest","index","yield")

###################################
#Data cleaning & variable creation#
###################################


#Creating data frame with mutual funds filtered on quantitative in the name
vec <- c(4974)
Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
data <- data.frame(matrix(nrow = 6186, ncol = 7))
data$Date <- Fond1$caldt 
data$r4974 <- Fond1$dret
vec <- c(4975)
Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
data$r4975 <- Fond1$dret
vec <- c(4976)
Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
data$r4976 <- Fond1$dret
vec <- c(4977)
Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
data$r4977 <- Fond1$dret
vec <- c(4978)
Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
data$r4978 <- Fond1$dret
vec <- c(4984)
Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
data$r4984 <- Fond1$dret
#vec <- c(4986)                                                 4405
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r4986 <- Fond1$dret
#vec <- c(4987)                                                 4405
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r4987 <- Fond1$dret
#vec <- c(4988)                                                 4405
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r4988 <- Fond1$dret
#vec <- c(4989)                                                 4405 zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r4989 <- Fond1$dret
vec <- c(4992)
Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
data$r4992 <- Fond1$dret
#vec <- c(4997)                                                  4449 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r4997 <- Fond1$dret
#vec <- c(4998)                                                   4931 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r4998 <- Fond1$dret
#vec <- c(4999)                                                     4931 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r4999 <- Fond1$dret
#vec <- c(5000)                                                   5461 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r5000 <- Fond1$dret
vec <- c(5001)
Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
data$r5001 <- Fond1$dret
#vec <- c(5002)                                                   5474 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r5002 <- Fond1$dret
#vec <- c(5003)                                                   5677 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r5003 <- Fond1$dret
#vec <- c(5004)                                                   5913 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r5004 <- Fond1$dret
vec <- c(5005)
Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
data$r5005 <- Fond1$dret
vec <- c(5009)
Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
data$r5009 <- Fond1$dret
vec <- c(13689)
Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
data$r13689 <- Fond1$dret
#vec <- c(13693)                                                  4096 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r13693 <- Fond1$dret
#vec <- c(13694)                                                  3987 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r13694 <- Fond1$dret
#vec <- c(13695)                                                  4807 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r13695 <- Fond1$dret
#vec <- c(13696)                                                  4807 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r13696 <- Fond1$dret
vec <- c(27070)
Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
data$r27070 <- Fond1$dret
vec <- c(31209)
Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
data$r31209 <- Fond1$dret
#vec <- c(31210)                                                  5506 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r31210 <- Fond1$dret
#vec <- c(36255)                                                  3904 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r36255 <- Fond1$dret
#vec <- c(36267)                                                  3904 zeukeb
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r36267 <- Fond1$dret
#vec <- c(36268)                                                  3904 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r36268 <- Fond1$dret
#vec <- c(36284)                                                  3904 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r36284 <- Fond1$dret
#vec <- c(48974)                                                  3292 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r48974 <- Fond1$dret
#vec <- c(48975)                                                  3292 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r48975 <- Fond1$dret
#vec <- c(49030)                                                  3292 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r49030 <- Fond1$dret
#vec <- c(49930)                                                  3203 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r49930 <- Fond1$dret
#vec <- c(49931)                                                  3206 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r49931 <- Fond1$dret
#vec <- c(49932)                                                  3206 Zeilen  
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r49932 <- Fond1$dret
#vec <- c(49933)                                                  3206 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r49933 <- Fond1$dret
#vec <- c(52153)                                                  3000 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r52153 <- Fond1$dret
#vec <- c(52455)                                                  2985 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r52455 <- Fond1$dret
#vec <- c(52456)                                                  2985 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r52456 <- Fond1$dret
#vec <- c(52457)                                                  2985 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r52457 <- Fond1$dret
vec <- c(62576)                                                  #2125 Zeilen // ETF
Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
Fond1 <- Fond1[-(1:301), ]
Fond2 <- as.numeric(Fond1$dret)
data_etf$qPF25 <- Fond2*100
vec <- c(63000)                                                  #2086 Zeilen // ETF
Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
Fond1 <- Fond1[-(1:262), ]
Fond2 <- as.numeric(Fond1$dret)
data_etf$qPF24 <- Fond2*100
#vec <- c(64492)                                                  1953 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r64492 <- Fond1$dret
#vec <- c(64493)                                                  1953 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r64493 <- Fond1$dret
vec <- c(86423)                                                  #1845 Zeilen // ETF
Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
Fond1 <- Fond1[-(1:21), ]
Fond2 <- as.numeric(Fond1$dret)
data_etf$qPF23 <- Fond2*100
vec <- c(86514)                                                  #1831 Zeilen // ETF
Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
Fond1 <- Fond1[-(1:7), ]
Fond2 <- as.numeric(Fond1$dret)
data_etf$qPF22 <- Fond2*100
#vec <- c(86921)                                                  1787 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r86921 <- Fond1$dret
#vec <- c(86922)                                                  1787 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r86922 <- Fond1$dret
#vec <- c(86923)                                                  1787 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r86923 <- Fond1$dret
#vec <- c(86924)                                                  1787 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r86924 <- Fond1$dret
#vec <- c(86925)                                                  1787 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r86925 <- Fond1$dret
#vec <- c(86926)                                                  1787 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r86926 <- Fond1$dret
#vec <- c(87015)                                                  1769 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r87015 <- Fond1$dret
#vec <- c(87605)                                                  1753 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r87605 <- Fond1$dret
#vec <- c(87606)                                                  1753 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r87606 <- Fond1$dret
#vec <- c(87607)                                                  1753 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r87607 <- Fond1$dret
#vec <- c(88021)                                                  1698 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r88021 <- Fond1$dret
#vec <- c(90360)                                                  1548 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r90360 <- Fond1$dret
#vec <- c(90361)                                                  1548 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r90361 <- Fond1$dret
#vec <- c(90362)                                                  1548 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r90362 <- Fond1$dret
#vec <- c(91072)                                                  1505 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r91072 <- Fond1$dret
#vec <- c(91073)                                                  1505 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r91073 <- Fond1$dret
#vec <- c(91080)                                                  1505 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r91080 <- Fond1$dret
#vec <- c(91098)                                                  1505 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r91098 <- Fond1$dret
#vec <- c(91126)                                                  1505 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r91126 <- Fond1$dret
#vec <- c(92577)                                                  1354 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r92577 <- Fond1$dret
#vec <- c(92578)                                                  1354 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r92578 <- Fond1$dret
#vec <- c(96093)                                                  855 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r96093 <- Fond1$dret
#vec <- c(96481)                                                  819 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r96481 <- Fond1$dret
#vec <- c(96947)                                                  724 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r96947 <- Fond1$dret
#vec <- c(99312)                                                  325 Zeilen // ETF
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r99312 <- Fond1$dret
#vec <- c(100010)                                                 210 Zeilen
#Fond1 <- filter(data_mutual, data_mutual$CRSP_FUNDNO %in% vec)
#data$r100010 <- Fond1$dret


#Excess returns ETF daily data

data_etf$p1 <- data_etf$qPF1 - data_etf$RF
data_etf$p2 <- data_etf$qPF2 - data_etf$RF
data_etf$p3 <- data_etf$qPF3 - data_etf$RF
data_etf$p4 <- data_etf$qPF4 - data_etf$RF
data_etf$p5 <- data_etf$qPF5 - data_etf$RF
data_etf$p6 <- data_etf$qPF6 - data_etf$RF
data_etf$p7 <- data_etf$qPF7 - data_etf$RF
data_etf$p8 <- data_etf$qPF8 - data_etf$RF
data_etf$p9 <- data_etf$qPF9 - data_etf$RF
data_etf$p10 <- data_etf$qPF10 - data_etf$RF
data_etf$p11 <- data_etf$qPF11 - data_etf$RF
data_etf$p12 <- data_etf$qPF12 - data_etf$RF
data_etf$p13 <- data_etf$qPF13 - data_etf$RF
data_etf$p14 <- data_etf$qPF14 - data_etf$RF
data_etf$p15 <- data_etf$qPF15 - data_etf$RF
data_etf$p16 <- data_etf$qPF16 - data_etf$RF
data_etf$p17 <- data_etf$qPF17 - data_etf$RF
data_etf$p18 <- data_etf$qPF18 - data_etf$RF
data_etf$p19 <- data_etf$qPF19 - data_etf$RF
data_etf$p20 <- data_etf$qPF20 - data_etf$RF
data_etf$p21 <- data_etf$qPF21 - data_etf$RF
data_etf$p22 <- data_etf$qPF22 - data_etf$RF
data_etf$p23 <- data_etf$qPF23 - data_etf$RF
data_etf$p24 <- data_etf$qPF24 - data_etf$RF
data_etf$p25 <- data_etf$qPF25 - data_etf$RF
data_etf$p26 <- data_etf$p1*(1/25)+data_etf$p2*(1/25)+data_etf$p3*(1/25)+data_etf$p4*(1/25)+data_etf$p5*(1/25)+data_etf$p6*(1/25)+data_etf$p7*(1/25)+data_etf$p8*(1/25)+data_etf$p9*(1/25)+data_etf$p10*(1/25)+data_etf$p11*(1/25)+data_etf$p12*(1/25)+data_etf$p13*(1/25)+data_etf$p14*(1/25)+data_etf$p15*(1/25)+data_etf$p16*(1/25)+data_etf$p17*(1/25)+data_etf$p18*(1/25)+data_etf$p19*(1/25)+data_etf$p20*(1/25)+data_etf$p21*(1/25)+data_etf$p22*(1/25)+data_etf$p23*(1/25)+data_etf$p24*(1/25)+data_etf$p25*(1/25)
#p26 equally weighted pf

#Excess returns ETF monthly data

data_etf_m$p1 <- data_etf_m$qPF1 - data_etf_m$RF
data_etf_m$p2 <- data_etf_m$qPF2 - data_etf_m$RF
data_etf_m$p3 <- data_etf_m$qPF3 - data_etf_m$RF
data_etf_m$p4 <- data_etf_m$qPF4 - data_etf_m$RF
data_etf_m$p5 <- data_etf_m$qPF5 - data_etf_m$RF
data_etf_m$p6 <- data_etf_m$qPF6 - data_etf_m$RF
data_etf_m$p7 <- data_etf_m$qPF7 - data_etf_m$RF
data_etf_m$p8 <- data_etf_m$qPF8 - data_etf_m$RF
data_etf_m$p9 <- data_etf_m$qPF9 - data_etf_m$RF
data_etf_m$p10 <- data_etf_m$qPF10 - data_etf_m$RF
data_etf_m$p11 <- data_etf_m$qPF11 - data_etf_m$RF
data_etf_m$p12 <- data_etf_m$qPF12 - data_etf_m$RF
data_etf_m$p13 <- data_etf_m$qPF13 - data_etf_m$RF
data_etf_m$p14 <- data_etf_m$qPF14 - data_etf_m$RF
data_etf_m$p15 <- data_etf_m$qPF15 - data_etf_m$RF
data_etf_m$p16 <- data_etf_m$qPF16 - data_etf_m$RF
data_etf_m$p17 <- data_etf_m$qPF17 - data_etf_m$RF
data_etf_m$p18 <- data_etf_m$qPF18 - data_etf_m$RF
data_etf_m$p19 <- data_etf_m$qPF19 - data_etf_m$RF
data_etf_m$p20 <- data_etf_m$qPF20 - data_etf_m$RF
data_etf_m$p21 <- data_etf_m$qPF21 - data_etf_m$RF
data_etf_m$p22 <- data_etf_m$qPF22 - data_etf_m$RF
data_etf_m$p23 <- data_etf_m$qPF23 - data_etf_m$RF
data_etf_m$p24 <- data_etf_m$qPF24 - data_etf_m$RF
data_etf_m$p25 <- data_etf_m$qPF25 - data_etf_m$RF
data_etf_m$p26 <- data_etf_m$p1*(1/25)+data_etf_m$p2*(1/25)+data_etf_m$p3*(1/25)+data_etf_m$p4*(1/25)+data_etf_m$p5*(1/25)+data_etf_m$p6*(1/25)+data_etf_m$p7*(1/25)+data_etf_m$p8*(1/25)+data_etf_m$p9*(1/25)+data_etf_m$p10*(1/25)+data_etf_m$p11*(1/25)+data_etf_m$p12*(1/25)+data_etf_m$p13*(1/25)+data_etf_m$p14*(1/25)+data_etf_m$p15*(1/25)+data_etf_m$p16*(1/25)+data_etf_m$p17*(1/25)+data_etf_m$p18*(1/25)+data_etf_m$p19*(1/25)+data_etf_m$p20*(1/25)+data_etf_m$p21*(1/25)+data_etf_m$p22*(1/25)+data_etf_m$p23*(1/25)+data_etf_m$p24*(1/25)+data_etf_m$p25*(1/25)
#p26 equally weighted pf



data_mutual <- data
data_mutual <- data_mutual[, -(1:7)]
data_mutual$MKT.RF <- ff_d$Mkt.RF
data_mutual$HML <- ff_d$HML
data_mutual$SMB <- ff_d$SMB
data_mutual$RF <- ff_d$RF
data_mutual$MKT <- data_mutual$MKT.RF + data_mutual$RF
names(data_mutual) <- c("date", "qPF1", "qPF2", "qPF3", "qPF4", "qPF5", "qPF6", 
                    "qPF7", "qPF8", "qPF9", "qPF10","qPF11", "qPF12", "qPF13","MKT.RF", "HML", "SMB", "RF","MKT")

#numeric values
data_mutual$qPF1 <- as.numeric(data_mutual$qPF1)
data_mutual$qPF2 <- as.numeric(data_mutual$qPF2)
data_mutual$qPF3 <- as.numeric(data_mutual$qPF3)
data_mutual$qPF4 <- as.numeric(data_mutual$qPF4)
data_mutual$qPF5 <- as.numeric(data_mutual$qPF5)
data_mutual$qPF6 <- as.numeric(data_mutual$qPF6)
data_mutual$qPF7 <- as.numeric(data_mutual$qPF7)
data_mutual$qPF8 <- as.numeric(data_mutual$qPF8)
data_mutual$qPF9 <- as.numeric(data_mutual$qPF9)
data_mutual$qPF10 <- as.numeric(data_mutual$qPF10)
data_mutual$qPF11 <- as.numeric(data_mutual$qPF11)
data_mutual$qPF12 <- as.numeric(data_mutual$qPF12)
data_mutual$qPF13 <- as.numeric(data_mutual$qPF13)

#Excess returns Mutual daily data
data_mutual$p1 <- (data_mutual$qPF1*100) - data_mutual$RF
data_mutual$p2 <- (data_mutual$qPF2*100) - data_mutual$RF
data_mutual$p3 <- (data_mutual$qPF3*100) - data_mutual$RF
data_mutual$p4 <- (data_mutual$qPF4*100) - data_mutual$RF
data_mutual$p5 <- (data_mutual$qPF5*100) - data_mutual$RF
data_mutual$p6 <- (data_mutual$qPF6*100) - data_mutual$RF
data_mutual$p7 <- (data_mutual$qPF7*100) - data_mutual$RF
data_mutual$p8 <- (data_mutual$qPF8*100) - data_mutual$RF
data_mutual$p9 <- (data_mutual$qPF9*100) - data_mutual$RF
data_mutual$p10 <- (data_mutual$qPF10*100) - data_mutual$RF
data_mutual$p11 <- (data_mutual$qPF11*100) - data_mutual$RF
data_mutual$p12 <- (data_mutual$qPF12*100) - data_mutual$RF
data_mutual$p13 <- (data_mutual$qPF13*100) - data_mutual$RF
data_mutual$p14 <- data_mutual$p1*(1/13)+data_mutual$p2*(1/13)+data_mutual$p3*(1/13)+data_mutual$p4*(1/13)+data_mutual$p5*(1/13)+data_mutual$p6*(1/13)+data_mutual$p7*(1/13)+data_mutual$p8*(1/13)+data_mutual$p9*(1/13)+data_mutual$p10*(1/13)+data_mutual$p11*(1/13)+data_mutual$p12*(1/13)+data_mutual$p13*(1/13)

#Excess returns Mutual monthly data

data_mutual_m$p1 <- (data_mutual_m$qPF1*100) - data_mutual_m$RF
data_mutual_m$p2 <- (data_mutual_m$qPF2*100) - data_mutual_m$RF
data_mutual_m$p3 <- (data_mutual_m$qPF3*100) - data_mutual_m$RF
data_mutual_m$p4 <- (data_mutual_m$qPF4*100) - data_mutual_m$RF
data_mutual_m$p5 <- (data_mutual_m$qPF5*100) - data_mutual_m$RF
data_mutual_m$p6 <- (data_mutual_m$qPF6*100) - data_mutual_m$RF
data_mutual_m$p7 <- (data_mutual_m$qPF7*100) - data_mutual_m$RF
data_mutual_m$p8 <- (data_mutual_m$qPF8*100) - data_mutual_m$RF
data_mutual_m$p9 <- (data_mutual_m$qPF9*100) - data_mutual_m$RF
data_mutual_m$p10 <- (data_mutual_m$qPF10*100) - data_mutual_m$RF
data_mutual_m$p11 <- (data_mutual_m$qPF11*100) - data_mutual_m$RF
data_mutual_m$p12 <- (data_mutual_m$qPF12*100) - data_mutual_m$RF
data_mutual_m$p13 <- (data_mutual_m$qPF13*100) - data_mutual_m$RF
data_mutual_m$p14 <- data_mutual_m$p1*(1/13)+data_mutual_m$p2*(1/13)+data_mutual_m$p3*(1/13)+data_mutual_m$p4*(1/13)+data_mutual_m$p5*(1/13)+data_mutual_m$p6*(1/13)+data_mutual_m$p7*(1/13)+data_mutual_m$p8*(1/13)+data_mutual_m$p9*(1/13)+data_mutual_m$p10*(1/13)+data_mutual_m$p11*(1/13)+data_mutual_m$p12*(1/13)+data_mutual_m$p13*(1/13)

dates <- read.xlsx('dates.xlsx',startRow = 1,detectDates = TRUE)
data_mutual$date <- dates$date
data_mutual$sp500 <- sp500$sp500
data_mutual$vix <- vix$vix

objects <- ls()
objects_to_remove <- objects[!objects %in% c("data_etf", "data_mutual", "data_etf_m", "data_mutual_m")]
rm(list = objects_to_remove)


######################
#####Correlation######
######################

Coretf <- data_etf[,34:59]
Coretf$MKT.RF <- data_etf$MKT.RF
Cormutual <- data_mutual[,20:33]
Cormutual$MKT.RF <- data_mutual$MKT.RF

cor(Coretf)
cor(Cormutual)
cor_e <- cor(Coretf)
cor_m <-cor(Cormutual)
corrplot(cor_e, method = "pie")
corrplot(cor_m, method = "pie")



####################
#Indicator creation#
####################


    #####################################################
    # 1 Bear market indicator (9 months, 12 months and 24 months) #
    #####################################################
#ETF
#9 months
data_etf_m$MKT <- 1+(data_etf_m$MKT/100)
data_etf_m$bear9 <- rep(NA, nrow(data_etf_m))
window_size <-  9
for (i in (window_size+1):(nrow(data_etf_m))) {
  cumprod_i <- cumprod(data_etf_m$MKT[(i-window_size):(i-1)])[window_size] - 1
  data_etf_m$bear9[i] <- ifelse(cumprod_i < 0, 1, 0)
}
data_etf_m$bear9

#12 months
data_etf_m$bear12 <- rep(NA, nrow(data_etf_m))
window_size <-  12
for (i in (window_size+1):(nrow(data_etf_m))) {
  cumprod_i <- cumprod(data_etf_m$MKT[(i-window_size):(i-1)])[window_size] - 1
  data_etf_m$bear12[i] <- ifelse(cumprod_i < 0, 1, 0)
}
data_etf_m$bear12

#Mutual
#9 months
data_mutual_m$MKT <- 1+(data_mutual_m$MKT/100)
data_mutual_m$bear9 <- rep(NA, nrow(data_mutual_m))
window_size <-  9
for (i in (window_size+1):(nrow(data_mutual_m))) {
  cumprod_i <- cumprod(data_mutual_m$MKT[(i-window_size):(i-1)])[window_size] - 1
  data_mutual_m$bear9[i] <- ifelse(cumprod_i < 0, 1, 0)
}
data_mutual_m$bear9

#12 months
data_mutual_m$bear12 <- rep(NA, nrow(data_mutual_m))
window_size <-  12
for (i in (window_size+1):(nrow(data_mutual_m))) {
  cumprod_i <- cumprod(data_mutual_m$MKT[(i-window_size):(i-1)])[window_size] - 1
  data_mutual_m$bear12[i] <- ifelse(cumprod_i < 0, 1, 0)
}
data_mutual_m$bear12

#24 months
data_mutual_m$bear24 <- rep(NA, nrow(data_mutual_m))
window_size <-  24
for (i in (window_size+1):(nrow(data_mutual_m))) {
  cumprod_i <- cumprod(data_mutual_m$MKT[(i-window_size):(i-1)])[window_size] - 1
  data_mutual_m$bear24[i] <- ifelse(cumprod_i < 0, 1, 0)
}
data_mutual_m$bear24


    ##############################################################
    # 2 High and low market variance indicator (9 months,12 months and 24 months)##
    ##############################################################
  
    #ETF
    #9 months
    data_etf$MKT <- data_etf$MKT.RF + data_etf$RF
    data_etf$date <- as.Date(data_etf$date, format = "%Y%m%d")
    data_etf$Month <- format(data_etf$date, "%Y-%m")
    monthly_std_dev <- aggregate(MKT ~ Month, data_etf, sd)
    names(monthly_std_dev)[names(monthly_std_dev) == "Month"] <- "date"
    names(monthly_std_dev)[names(monthly_std_dev) == "Mkt"] <- "Mkt_sd"
    str(monthly_std_dev)
    monthly_std_dev$date <- paste0(substring(monthly_std_dev$date, 1, 4), substring(monthly_std_dev$date, 6, 7))
    data_etf_m$MKT_sd<- monthly_std_dev$MKT
    data_etf_m$var_high_9 <- rep(NA, nrow(data_etf_m))
    data_etf_m$var_low_9 <- rep(NA, nrow(data_etf_m))
    window_size <- 9
    for (i in (window_size+1):length(data_etf_m$date)) {
    start_index <- i - window_size 
    end_index <- i - 1  
    rolling_data <- data_etf_m[start_index:end_index,]
    data_etf_m$var_high_9[i] <- ifelse(((data_etf_m$MKT_sd[i] - mean(rolling_data$MKT_sd)) / sd(rolling_data$MKT_sd)) > 1, 1, 0)
    data_etf_m$var_low_9[i] <- ifelse(((data_etf_m$MKT_sd[i] - mean(rolling_data$MKT_sd)) / sd(rolling_data$MKT_sd)) < -1, 1, 0)
    }
    #ETF
    #12 months
    data_etf$MKT <- data_etf$MKT.RF + data_etf$RF
    data_etf$date <- as.Date(data_etf$date, format = "%Y%m%d")
    data_etf$Month <- format(data_etf$date, "%Y-%m")
    monthly_std_dev <- aggregate(MKT ~ Month, data_etf, sd)
    names(monthly_std_dev)[names(monthly_std_dev) == "Month"] <- "date"
    names(monthly_std_dev)[names(monthly_std_dev) == "Mkt"] <- "Mkt_sd"
    str(monthly_std_dev)
    monthly_std_dev$date <- paste0(substring(monthly_std_dev$date, 1, 4), substring(monthly_std_dev$date, 6, 7))
    data_etf_m$MKT_sd<- monthly_std_dev$MKT
    data_etf_m$var_high_12 <- rep(NA, nrow(data_etf_m))
    data_etf_m$var_low_12 <- rep(NA, nrow(data_etf_m))
    window_size <- 12
    for (i in (window_size+1):length(data_etf_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_etf_m[start_index:end_index,]
      data_etf_m$var_high_12[i] <- ifelse(((data_etf_m$MKT_sd[i] - mean(rolling_data$MKT_sd)) / sd(rolling_data$MKT_sd)) > 1, 1, 0)
      data_etf_m$var_low_12[i] <- ifelse(((data_etf_m$MKT_sd[i] - mean(rolling_data$MKT_sd)) / sd(rolling_data$MKT_sd)) < -1, 1, 0)
    }
 
    #Mutual
    #9 months
    data_mutual$MKT <- data_mutual$MKT.RF + data_mutual$RF
    data_mutual$date <- as.Date(data_mutual$date, format = "%Y%m%d")
    data_mutual$Month <- format(data_mutual$date, "%Y-%m")
    monthly_std_dev <- aggregate(MKT ~ Month, data_mutual, sd)
    names(monthly_std_dev)[names(monthly_std_dev) == "Month"] <- "date"
    names(monthly_std_dev)[names(monthly_std_dev) == "Mkt"] <- "Mkt_sd"
    str(monthly_std_dev)
    monthly_std_dev$date <- paste0(substring(monthly_std_dev$date, 1, 4), substring(monthly_std_dev$date, 6, 7))
    data_mutual_m$MKT_sd<- monthly_std_dev$MKT
    data_mutual_m$var_high_9 <- rep(NA, nrow(data_mutual_m))
    data_mutual_m$var_low_9 <- rep(NA, nrow(data_mutual_m))
    window_size <- 9
    for (i in (window_size+1):length(data_mutual_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_mutual_m[start_index:end_index,]
      data_mutual_m$var_high_9[i] <- ifelse(((data_mutual_m$MKT_sd[i] - mean(rolling_data$MKT_sd)) / sd(rolling_data$MKT_sd)) > 1, 1, 0)
      data_mutual_m$var_low_9[i] <- ifelse(((data_mutual_m$MKT_sd[i] - mean(rolling_data$MKT_sd)) / sd(rolling_data$MKT_sd)) < -1, 1, 0)
    }
    
    #Mutual
    #12 months
    data_mutual$MKT <- data_mutual$MKT.RF + data_mutual$RF
    data_mutual$date <- as.Date(data_mutual$date, format = "%Y%m%d")
    data_mutual$Month <- format(data_mutual$date, "%Y-%m")
    monthly_std_dev <- aggregate(MKT ~ Month, data_mutual, sd)
    names(monthly_std_dev)[names(monthly_std_dev) == "Month"] <- "date"
    names(monthly_std_dev)[names(monthly_std_dev) == "Mkt"] <- "Mkt_sd"
    str(monthly_std_dev)
    monthly_std_dev$date <- paste0(substring(monthly_std_dev$date, 1, 4), substring(monthly_std_dev$date, 6, 7))
    data_mutual_m$MKT_sd<- monthly_std_dev$MKT
    data_mutual_m$var_high_12 <- rep(NA, nrow(data_mutual_m))
    data_mutual_m$var_low_12 <- rep(NA, nrow(data_mutual_m))
    window_size <- 12
    for (i in (window_size+1):length(data_mutual_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_mutual_m[start_index:end_index,]
      data_mutual_m$var_high_12[i] <- ifelse(((data_mutual_m$MKT_sd[i] - mean(rolling_data$MKT_sd)) / sd(rolling_data$MKT_sd)) > 1, 1, 0)
      data_mutual_m$var_low_12[i] <- ifelse(((data_mutual_m$MKT_sd[i] - mean(rolling_data$MKT_sd)) / sd(rolling_data$MKT_sd)) < -1, 1, 0)
    }
    
    #Mutual
    #24 months
    data_mutual$MKT <- data_mutual$MKT.RF + data_mutual$RF
    data_mutual$date <- as.Date(data_mutual$date, format = "%Y%m%d")
    data_mutual$Month <- format(data_mutual$date, "%Y-%m")
    monthly_std_dev <- aggregate(MKT ~ Month, data_mutual, sd)
    names(monthly_std_dev)[names(monthly_std_dev) == "Month"] <- "date"
    names(monthly_std_dev)[names(monthly_std_dev) == "Mkt"] <- "Mkt_sd"
    str(monthly_std_dev)
    monthly_std_dev$date <- paste0(substring(monthly_std_dev$date, 1, 4), substring(monthly_std_dev$date, 6, 7))
    data_mutual_m$MKT_sd<- monthly_std_dev$MKT
    data_mutual_m$var_high_24 <- rep(NA, nrow(data_mutual_m))
    data_mutual_m$var_low_24 <- rep(NA, nrow(data_mutual_m))
    window_size <- 24
    for (i in (window_size+1):length(data_mutual_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_mutual_m[start_index:end_index,]
      data_mutual_m$var_high_24[i] <- ifelse(((data_mutual_m$MKT_sd[i] - mean(rolling_data$MKT_sd)) / sd(rolling_data$MKT_sd)) > 1, 1, 0)
      data_mutual_m$var_low_24[i] <- ifelse(((data_mutual_m$MKT_sd[i] - mean(rolling_data$MKT_sd)) / sd(rolling_data$MKT_sd)) < -1, 1, 0)
    }
    
    ################################################
    # 3 High and low swap return indicator (9, 12) #
    ################################################
    
    #ETF
    #9 months
    data_etf$swap <- (1+data_etf$RF*252)^(20/252) *
      ((1/21)*(252*(100*log(data_etf$sp500/lag(data_etf$sp500)))^2
               - lag(data_etf$vix)^2)+
         (20/21)*(data_etf$vix^2-lag(data_etf$vix)^2))
    monthly_std_dev <- aggregate(swap ~ Month, data_etf, sd)
    names(monthly_std_dev)[names(monthly_std_dev) == "Month"] <- "date"
    names(monthly_std_dev)[names(monthly_std_dev) == "swap"] <- "swap_sd"
    monthly_std_dev$date <- paste0(substring(monthly_std_dev$date, 1, 4), substring(monthly_std_dev$date, 6, 7))
    data_etf_m$swap_sd <- monthly_std_dev$swap_sd 
    
    data_etf_m$swap_high_9 <- rep(NA, nrow(data_etf_m))
    data_etf_m$swap_low_9 <- rep(NA, nrow(data_etf_m))
    window_size <- 9 
    for (i in (window_size+1):length(data_etf_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_etf_m[start_index:end_index,]
      data_etf_m$swap_high_9[i] <- ifelse(((data_etf_m$swap_sd[i] - mean(rolling_data$swap_sd)) / sd(rolling_data$swap_sd)) > 1, 1, 0)
      data_etf_m$swap_low_9[i] <- ifelse(((data_etf_m$swap_sd[i] - mean(rolling_data$swap_sd)) / sd(rolling_data$swap_sd)) < -1, 1, 0)
    }
    
    #ETF
    #12 months
    data_etf$swap <- (1+data_etf$RF*252)^(20/252) *
      ((1/21)*(252*(100*log(data_etf$sp500/lag(data_etf$sp500)))^2
               - lag(data_etf$vix)^2)+
         (20/21)*(data_etf$vix^2-lag(data_etf$vix)^2))
    monthly_std_dev <- aggregate(swap ~ Month, data_etf, sd)
    names(monthly_std_dev)[names(monthly_std_dev) == "Month"] <- "date"
    names(monthly_std_dev)[names(monthly_std_dev) == "swap"] <- "swap_sd"
    monthly_std_dev$date <- paste0(substring(monthly_std_dev$date, 1, 4), substring(monthly_std_dev$date, 6, 7))
    data_etf_m$swap_sd <- monthly_std_dev$swap_sd 
    
    data_etf_m$swap_high_12 <- rep(NA, nrow(data_etf_m))
    data_etf_m$swap_low_12 <- rep(NA, nrow(data_etf_m))
    window_size <- 12 
    for (i in (window_size+1):length(data_etf_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_etf_m[start_index:end_index,]
      data_etf_m$swap_high_12[i] <- ifelse(((data_etf_m$swap_sd[i] - mean(rolling_data$swap_sd)) / sd(rolling_data$swap_sd)) > 1, 1, 0)
      data_etf_m$swap_low_12[i] <- ifelse(((data_etf_m$swap_sd[i] - mean(rolling_data$swap_sd)) / sd(rolling_data$swap_sd)) < -1, 1, 0)
    }
   
    #Mutual
    #9 months
    data_mutual$swap <- (1+data_mutual$RF*252)^(20/252) *
      ((1/21)*(252*(100*log(data_mutual$sp500/lag(data_mutual$sp500)))^2
               - lag(data_mutual$vix)^2)+
         (20/21)*(data_mutual$vix^2-lag(data_mutual$vix)^2))
    monthly_std_dev <- aggregate(swap ~ Month, data_mutual, sd)
    names(monthly_std_dev)[names(monthly_std_dev) == "Month"] <- "date"
    names(monthly_std_dev)[names(monthly_std_dev) == "swap"] <- "swap_sd"
    monthly_std_dev$date <- paste0(substring(monthly_std_dev$date, 1, 4), substring(monthly_std_dev$date, 6, 7))
    data_mutual_m$swap_sd <- monthly_std_dev$swap_sd 
    
    data_mutual_m$swap_high_9 <- rep(NA, nrow(data_mutual_m))
    data_mutual_m$swap_low_9 <- rep(NA, nrow(data_mutual_m))
    window_size <- 9 
    for (i in (window_size+1):length(data_mutual_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_mutual_m[start_index:end_index,]
      data_mutual_m$swap_high_9[i] <- ifelse(((data_mutual_m$swap_sd[i] - mean(rolling_data$swap_sd)) / sd(rolling_data$swap_sd)) > 1, 1, 0)
      data_mutual_m$swap_low_9[i] <- ifelse(((data_mutual_m$swap_sd[i] - mean(rolling_data$swap_sd)) / sd(rolling_data$swap_sd)) < -1, 1, 0)
    }
    
    #Mutual
    #12 months
    data_mutual$swap <- (1+data_mutual$RF*252)^(20/252) *
      ((1/21)*(252*(100*log(data_mutual$sp500/lag(data_mutual$sp500)))^2
               - lag(data_mutual$vix)^2)+
         (20/21)*(data_mutual$vix^2-lag(data_mutual$vix)^2))
    monthly_std_dev <- aggregate(swap ~ Month, data_mutual, sd)
    names(monthly_std_dev)[names(monthly_std_dev) == "Month"] <- "date"
    names(monthly_std_dev)[names(monthly_std_dev) == "swap"] <- "swap_sd"
    monthly_std_dev$date <- paste0(substring(monthly_std_dev$date, 1, 4), substring(monthly_std_dev$date, 6, 7))
    data_mutual_m$swap_sd <- monthly_std_dev$swap_sd 
    
    data_mutual_m$swap_high_12 <- rep(NA, nrow(data_mutual_m))
    data_mutual_m$swap_low_12 <- rep(NA, nrow(data_mutual_m))
    window_size <- 12 
    for (i in (window_size+1):length(data_mutual_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_mutual_m[start_index:end_index,]
      data_mutual_m$swap_high_12[i] <- ifelse(((data_mutual_m$swap_sd[i] - mean(rolling_data$swap_sd)) / sd(rolling_data$swap_sd)) > 1, 1, 0)
      data_mutual_m$swap_low_12[i] <- ifelse(((data_mutual_m$swap_sd[i] - mean(rolling_data$swap_sd)) / sd(rolling_data$swap_sd)) < -1, 1, 0)
    }
    
       
    
    #Mutual
    #24 months
    data_mutual$swap <- (1+data_mutual$RF*252)^(20/252) *
      ((1/21)*(252*(100*log(data_mutual$sp500/lag(data_mutual$sp500)))^2
               - lag(data_mutual$vix)^2)+
         (20/21)*(data_mutual$vix^2-lag(data_mutual$vix)^2))
    monthly_std_dev <- aggregate(swap ~ Month, data_mutual, sd)
    names(monthly_std_dev)[names(monthly_std_dev) == "Month"] <- "date"
    names(monthly_std_dev)[names(monthly_std_dev) == "swap"] <- "swap_sd"
    monthly_std_dev$date <- paste0(substring(monthly_std_dev$date, 1, 4), substring(monthly_std_dev$date, 6, 7))
    data_mutual_m$swap_sd <- monthly_std_dev$swap_sd 
    
    data_mutual_m$swap_high_24 <- rep(NA, nrow(data_mutual_m))
    data_mutual_m$swap_low_24 <- rep(NA, nrow(data_mutual_m))
    window_size <- 24 
    for (i in (window_size+1):length(data_mutual_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_mutual_m[start_index:end_index,]
      data_mutual_m$swap_high_24[i] <- ifelse(((data_mutual_m$swap_sd[i] - mean(rolling_data$swap_sd)) / sd(rolling_data$swap_sd)) > 1, 1, 0)
      data_mutual_m$swap_low_24[i] <- ifelse(((data_mutual_m$swap_sd[i] - mean(rolling_data$swap_sd)) / sd(rolling_data$swap_sd)) < -1, 1, 0)
    }
    
     
    ##################################################
    # 4 High and low interest rate indicator (9, 12) #
    ##################################################
    
    #ETF
    #9 months
    data_etf_m$int_high_9 <- rep(NA, nrow(data_etf_m))
    data_etf_m$int_low_9 <- rep(NA, nrow(data_etf_m))
    window_size <- 9 
    for (i in (window_size+1):length(data_etf_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_etf_m[start_index:end_index,]
      data_etf_m$int_high_9[i] <- ifelse(((data_etf_m$interest[i] - mean(rolling_data$interest)) / sd(rolling_data$interest)) > 1, 1, 0)
      data_etf_m$int_low_9[i] <- ifelse(((data_etf_m$interest[i] - mean(rolling_data$interest)) / sd(rolling_data$interest)) < -1, 1, 0)
    }
    
    #ETF
    #12 months
    data_etf_m$int_high_12 <- rep(NA, nrow(data_etf_m))
    data_etf_m$int_low_12 <- rep(NA, nrow(data_etf_m))
    window_size <- 12 
    for (i in (window_size+1):length(data_etf_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_etf_m[start_index:end_index,]
      data_etf_m$int_high_12[i] <- ifelse(((data_etf_m$interest[i] - mean(rolling_data$interest)) / sd(rolling_data$interest)) > 1, 1, 0)
      data_etf_m$int_low_12[i] <- ifelse(((data_etf_m$interest[i] - mean(rolling_data$interest)) / sd(rolling_data$interest)) < -1, 1, 0)
    }
    
    #Mutual
    #9 months
    data_mutual_m$int_high_9 <- rep(NA, nrow(data_mutual_m))
    data_mutual_m$int_low_9 <- rep(NA, nrow(data_mutual_m))
    window_size <- 9 
    for (i in (window_size+1):length(data_mutual_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_mutual_m[start_index:end_index,]
      data_mutual_m$int_high_9[i] <- ifelse(((data_mutual_m$interest[i] - mean(rolling_data$interest)) / sd(rolling_data$interest)) > 1, 1, 0)
      data_mutual_m$int_low_9[i] <- ifelse(((data_mutual_m$interest[i] - mean(rolling_data$interest)) / sd(rolling_data$interest)) < -1, 1, 0)
    }   
    
    #Mutual
    #12 months
    data_mutual_m$int_high_12 <- rep(NA, nrow(data_mutual_m))
    data_mutual_m$int_low_12 <- rep(NA, nrow(data_mutual_m))
    window_size <- 12 
    for (i in (window_size+1):length(data_mutual_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_mutual_m[start_index:end_index,]
      data_mutual_m$int_high_12[i] <- ifelse(((data_mutual_m$interest[i] - mean(rolling_data$interest)) / sd(rolling_data$interest)) > 1, 1, 0)
      data_mutual_m$int_low_12[i] <- ifelse(((data_mutual_m$interest[i] - mean(rolling_data$interest)) / sd(rolling_data$interest)) < -1, 1, 0)
    }
    
    #Mutual
    #24 months
    data_mutual_m$int_high_24 <- rep(NA, nrow(data_mutual_m))
    data_mutual_m$int_low_24 <- rep(NA, nrow(data_mutual_m))
    window_size <- 24 
    for (i in (window_size+1):length(data_mutual_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_mutual_m[start_index:end_index,]
      data_mutual_m$int_high_24[i] <- ifelse(((data_mutual_m$interest[i] - mean(rolling_data$interest)) / sd(rolling_data$interest)) > 1, 1, 0)
      data_mutual_m$int_low_24[i] <- ifelse(((data_mutual_m$interest[i] - mean(rolling_data$interest)) / sd(rolling_data$interest)) < -1, 1, 0)
    }
    
    #####################################################
    # 5 Recession indicator (GDP growth rate, 6 months) #
    #####################################################
    
    #ETF
    data_etf_m$recession <- rep(NA, nrow(data_etf_m))
    window_size <- 7 
    for (i in (window_size+1):length(data_etf_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_etf_m[start_index:end_index,]
      data_etf_m$recession[i] <- ifelse((rolling_data$index[1]>rolling_data$index[4])&(rolling_data$index[4]>rolling_data$index[7]), 1, 0)
    }
    
    #Mutual
    data_mutual_m$recession <- rep(NA, nrow(data_mutual_m))
    window_size <- 7 
    for (i in (window_size+1):length(data_mutual_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_mutual_m[start_index:end_index,]
      data_mutual_m$recession[i] <- ifelse((rolling_data$index[1]>rolling_data$index[4])&(rolling_data$index[4]>rolling_data$index[7]), 1, 0)
    }
       
      
    ##################################################################
    # 6 High and low dividend yield indicator (9, 12) #
    ##################################################################
    
    #ETF
    #9 months 
    data_etf_m$yield_high_9 <- rep(NA, nrow(data_etf_m))
    data_etf_m$yield_low_9 <- rep(NA, nrow(data_etf_m))
    window_size <- 9 
    for (i in (window_size+1):length(data_etf_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_etf_m[start_index:end_index,]
      data_etf_m$yield_high_9[i] <- ifelse(((data_etf_m$yield[i] - mean(rolling_data$yield)) / sd(rolling_data$yield)) > 1, 1, 0)
      data_etf_m$yield_low_9[i] <- ifelse(((data_etf_m$yield[i] - mean(rolling_data$yield)) / sd(rolling_data$yield)) < -1, 1, 0)
    } 
    
    #ETF
    #12 months 
    data_etf_m$yield_high_12 <- rep(NA, nrow(data_etf_m))
    data_etf_m$yield_low_12 <- rep(NA, nrow(data_etf_m))
    window_size <- 12 
    for (i in (window_size+1):length(data_etf_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_etf_m[start_index:end_index,]
      data_etf_m$yield_high_12[i] <- ifelse(((data_etf_m$yield[i] - mean(rolling_data$yield)) / sd(rolling_data$yield)) > 1, 1, 0)
      data_etf_m$yield_low_12[i] <- ifelse(((data_etf_m$yield[i] - mean(rolling_data$yield)) / sd(rolling_data$yield)) < -1, 1, 0)
    } 
  
    #Mutual
    #9 months 
    data_mutual_m$yield_high_9 <- rep(NA, nrow(data_mutual_m))
    data_mutual_m$yield_low_9 <- rep(NA, nrow(data_mutual_m))
    window_size <- 9 
    for (i in (window_size+1):length(data_mutual_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_mutual_m[start_index:end_index,]
      data_mutual_m$yield_high_9[i] <- ifelse(((data_mutual_m$yield[i] - mean(rolling_data$yield)) / sd(rolling_data$yield)) > 1, 1, 0)
      data_mutual_m$yield_low_9[i] <- ifelse(((data_mutual_m$yield[i] - mean(rolling_data$yield)) / sd(rolling_data$yield)) < -1, 1, 0)
    } 
    
    #Mutual
    #12 months 
    data_mutual_m$yield_high_12 <- rep(NA, nrow(data_mutual_m))
    data_mutual_m$yield_low_12 <- rep(NA, nrow(data_mutual_m))
    window_size <- 12 
    for (i in (window_size+1):length(data_mutual_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_mutual_m[start_index:end_index,]
      data_mutual_m$yield_high_12[i] <- ifelse(((data_mutual_m$yield[i] - mean(rolling_data$yield)) / sd(rolling_data$yield)) > 1, 1, 0)
      data_mutual_m$yield_low_12[i] <- ifelse(((data_mutual_m$yield[i] - mean(rolling_data$yield)) / sd(rolling_data$yield)) < -1, 1, 0)
    } 
    
    #Mutual
    #24 months 
    data_mutual_m$yield_high_24 <- rep(NA, nrow(data_mutual_m))
    data_mutual_m$yield_low_24 <- rep(NA, nrow(data_mutual_m))
    window_size <- 24 
    for (i in (window_size+1):length(data_mutual_m$date)) {
      start_index <- i - window_size 
      end_index <- i - 1  
      rolling_data <- data_mutual_m[start_index:end_index,]
      data_mutual_m$yield_high_24[i] <- ifelse(((data_mutual_m$yield[i] - mean(rolling_data$yield)) / sd(rolling_data$yield)) > 1, 1, 0)
      data_mutual_m$yield_low_24[i] <- ifelse(((data_mutual_m$yield[i] - mean(rolling_data$yield)) / sd(rolling_data$yield)) < -1, 1, 0)
    } 
      
    objects <- ls()
    objects_to_remove <- objects[!objects %in% c("data_etf", "data_mutual", "data_etf_m", "data_mutual_m")]
    rm(list = objects_to_remove)    
    

    
           
    
   
   


    
    
    
                                      ################################
                                      ###########Tables###############
                                      ################################
    
    ##########################################################
    ###########Table 1 - Summary statistics###################
    ##########################################################
    
    #Tabelle 1 
    #ETFs
    #mean returns
    print(mean(data_etf_m$p15, na.rm = TRUE))
    print(mean(data_etf_m$p22, na.rm = TRUE))
    print(mean(data_etf_m$p26, na.rm = TRUE))
    print(mean(data_etf_m$MKT.RF, na.rm = TRUE))
    #sd
    print(sd(data_etf_m$p15, na.rm = TRUE))
    print(sd(data_etf_m$p22, na.rm = TRUE))
    print(sd(data_etf_m$p26, na.rm = TRUE))
    print(sd(data_etf_m$MKT.RF, na.rm = TRUE))
    #Sharpe ratio
    print((mean(data_etf_m$p15, na.rm = TRUE)*12) / (sd(data_etf_m$p15, na.rm = TRUE)*sqrt(12)))
    print((mean(data_etf_m$p22, na.rm = TRUE)*12) / (sd(data_etf_m$p22, na.rm = TRUE)*sqrt(12)))
    print((mean(data_etf_m$p26, na.rm = TRUE)*12) / (sd(data_etf_m$p26, na.rm = TRUE)*sqrt(12)))
    print((mean(data_etf_m$MKT.RF, na.rm = TRUE)*12) / (sd(data_etf_m$MKT.RF, na.rm = TRUE)*sqrt(12)))
    #Monthly Model output: CAPM model 
    model1 <- lm(data_etf_m$p15~data_etf_m$MKT.RF)
    summary(model1) 
    lmtest::bgtest(model1, order = 3) #autocorrelation
    model1 <- lm(data_etf_m$p22~data_etf_m$MKT.RF)
    summary(model1) 
    lmtest::bgtest(model1, order = 3) #autocorrelation
    model1 <- lm(data_etf_m$p26~data_etf_m$MKT.RF)
    summary(model1) 
 
    
    #MF
    #mean returns
    print(mean(data_mutual_m$p1, na.rm = TRUE))
    print(mean(data_mutual_m$p11, na.rm = TRUE))
    print(mean(data_mutual_m$p14, na.rm = TRUE))
    print(mean(data_mutual_m$MKT.RF, na.rm = TRUE))
    #sd
    print(sd(data_mutual_m$p1, na.rm = TRUE))
    print(sd(data_mutual_m$p11, na.rm = TRUE))
    print(sd(data_mutual_m$p14, na.rm = TRUE))
    print(sd(data_mutual_m$MKT.RF, na.rm = TRUE))
    #Sharpe ratio
    print((mean(data_mutual_m$p1, na.rm = TRUE)*12) / (sd(data_mutual_m$p1, na.rm = TRUE)*sqrt(12)))
    print((mean(data_mutual_m$p11, na.rm = TRUE)*12) / (sd(data_mutual_m$p11, na.rm = TRUE)*sqrt(12)))
    print((mean(data_mutual_m$p14, na.rm = TRUE)*12) / (sd(data_mutual_m$p14, na.rm = TRUE)*sqrt(12)))
    print((mean(data_mutual_m$MKT.RF, na.rm = TRUE)*12) / (sd(data_mutual_m$MKT.RF, na.rm = TRUE)*sqrt(12)))
   
    #Monthly Model output: CAPM model
    model1 <- lm(data_mutual_m$p1~data_mutual_m$MKT.RF)
    summary(model1) 
    lmtest::bgtest(model1, order = 3) #autocorrelation
    model1 <- lm(data_mutual_m$p11~data_mutual_m$MKT.RF)
    summary(model1) 
    lmtest::bgtest(model1, order = 3) #autocorrelation
    model1 <- lm(data_mutual_m$p14~data_mutual_m$MKT.RF)
    summary(model1) 
    lmtest::bgtest(model1, order = 3) #autocorrelation
    
    objects <- ls()
    objects_to_remove <- objects[!objects %in% c("data_etf", "data_mutual", "data_etf_m", "data_mutual_m")]
    rm(list = objects_to_remove)
    
    #############################################
    #######Table 2 - Carhart Four Factor#########
    #############################################
    
    #Monthly Model output: 4-Factor model ETF
    model1 <- lm(data_etf_m$p26~data_etf_m$MKT.RF+data_etf_m$SMB+data_etf_m$HML+data_etf_m$WML)
    summary(model1) 
    lmtest::bgtest(model1, order = 3) #autocorrelation
    
    #Monthly Model output: 4-Factor model MF
    model1 <- lm(data_mutual_m$p14~data_mutual_m$MKT.RF+data_mutual_m$SMB+data_mutual_m$HML+data_mutual_m$WML)
    summary(model1) 
    lmtest::bgtest(model1, order = 3) #autocorrelation
    
    objects <- ls()
    objects_to_remove <- objects[!objects %in% c("data_etf", "data_mutual", "data_etf_m", "data_mutual_m")]
    rm(list = objects_to_remove)
    
    #############################################################################
    ######Table 4 - Results Conditional Carhart Four Factor analysis (ETFs)######
    #############################################################################
    
    #ETFs
    
    ##Bear Market indicator##
    
    data_etf_m_new <- subset(data_etf_m, !is.na(data_etf_m$bear12))
    model_new <- lm(data_etf_m_new$p26~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    
    # Market variance indicator #
    
    data_etf_m_new <- subset(data_etf_m, !is.na(data_etf_m$swap_high_12))
    
    model_new <- lm(data_etf_m_new$p26~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_etf_m_new$p26~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    
    # Short-term interest rate indicators #
    
    data_etf_m_new <- subset(data_etf_m, !is.na(data_etf_m$int_high_12))
    
    model_new <- lm(data_etf_m_new$p26~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_etf_m_new$p26~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    
    summary(model_new)
    
    ###Recession indicator###
    
    data_etf_m_new <- subset(data_etf_m, !is.na(data_etf_m$recession))
    
    model_new <- lm(data_etf_m_new$p26~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    
    ####Divident yield indicator###
    
    data_etf_m_new <- subset(data_etf_m, !is.na(data_etf_m$yield_high_12))
    
    model_new <- lm(data_etf_m_new$p26~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_etf_m_new$p26~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    
    # Variance risk indicators (swap returns) #
    
    data_etf_m_new <- subset(data_etf_m, !is.na(data_etf_m$swap_high_12))
    
    model_new <- lm(data_etf_m_new$p26~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_etf_m_new$p26~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    
    objects <- ls()
    objects_to_remove <- objects[!objects %in% c("data_etf", "data_mutual", "data_etf_m", "data_mutual_m")]
    rm(list = objects_to_remove)
  
    
    
    ###########################################################################
    #####Table 3 - Results Conditional Carhart Four Factor analysis (MFs)######
    ###########################################################################
    
    ##Bear-market##
    data_mutual_m_new <- subset(data_mutual_m, !is.na(data_mutual_m$bear12))
    model_new <- lm(data_mutual_m_new$p14~data_mutual_m_new$bear12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$bear12*data_mutual_m_new$SMB +
                      data_mutual_m_new$bear12*data_mutual_m_new$HML +
                      data_mutual_m_new$bear12*data_mutual_m_new$WML)
    summary(model_new)
    
    ##Market variance indicators
    
    data_mutual_m_new <- subset(data_mutual_m, !is.na(data_mutual_m$var_high_12))
    model_new <- lm(data_mutual_m_new$p14~data_mutual_m_new$var_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_mutual_m_new$p14~data_mutual_m_new$var_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$WML)
    summary(model_new)
    
    ##Variance risk indicators (swap returns)##
    
    data_mutual_m_new <- subset(data_mutual_m, !is.na(data_mutual_m$swap_high_12))
    
    model_new <- lm(data_mutual_m_new$p14~data_mutual_m_new$swap_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_mutual_m_new$p14~data_mutual_m_new$swap_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$WML)
    summary(model_new)
    
    ### Short-term interest rate indicators #
    
    data_mutual_m_new <- subset(data_mutual_m, !is.na(data_mutual_m$int_high_12))
    
    model_new <- lm(data_mutual_m_new$p14~data_mutual_m_new$int_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_mutual_m_new$p14~data_mutual_m_new$int_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$WML)
    
    summary(model_new)
    
    # Recession indicator #
    
    data_mutual_m_new <- subset(data_mutual_m, !is.na(data_mutual_m$recession))
    
    model_new <- lm(data_mutual_m_new$p14~data_mutual_m_new$recession*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$recession*data_mutual_m_new$SMB +
                      data_mutual_m_new$recession*data_mutual_m_new$HML +
                      data_mutual_m_new$recession*data_mutual_m_new$WML)
    summary(model_new)
    
      # Dividend yield indicator #
    
    data_mutual_m_new <- subset(data_mutual_m, !is.na(data_mutual_m$yield_high_12))
    
    model_new <- lm(data_mutual_m_new$p14~data_mutual_m_new$yield_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_mutual_m_new$p14~data_mutual_m_new$yield_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$WML)
    summary(model_new)
    
    objects <- ls()
    objects_to_remove <- objects[!objects %in% c("data_etf", "data_mutual", "data_etf_m", "data_mutual_m")]
    rm(list = objects_to_remove)
    
    #####################################################
    #########Table 6 - Individual Results (MFs)##########
    #####################################################
    ##Bear-market##
    data_mutual_m_new <- subset(data_mutual_m, !is.na(data_mutual_m$bear12))
    model_new <- lm(data_mutual_m_new$p1~data_mutual_m_new$bear12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$bear12*data_mutual_m_new$SMB +
                      data_mutual_m_new$bear12*data_mutual_m_new$HML +
                      data_mutual_m_new$bear12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p2~data_mutual_m_new$bear12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$bear12*data_mutual_m_new$SMB +
                      data_mutual_m_new$bear12*data_mutual_m_new$HML +
                      data_mutual_m_new$bear12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p3~data_mutual_m_new$bear12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$bear12*data_mutual_m_new$SMB +
                      data_mutual_m_new$bear12*data_mutual_m_new$HML +
                      data_mutual_m_new$bear12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p4~data_mutual_m_new$bear12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$bear12*data_mutual_m_new$SMB +
                      data_mutual_m_new$bear12*data_mutual_m_new$HML +
                      data_mutual_m_new$bear12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p5~data_mutual_m_new$bear12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$bear12*data_mutual_m_new$SMB +
                      data_mutual_m_new$bear12*data_mutual_m_new$HML +
                      data_mutual_m_new$bear12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p6~data_mutual_m_new$bear12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$bear12*data_mutual_m_new$SMB +
                      data_mutual_m_new$bear12*data_mutual_m_new$HML +
                      data_mutual_m_new$bear12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p7~data_mutual_m_new$bear12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$bear12*data_mutual_m_new$SMB +
                      data_mutual_m_new$bear12*data_mutual_m_new$HML +
                      data_mutual_m_new$bear12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p8~data_mutual_m_new$bear12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$bear12*data_mutual_m_new$SMB +
                      data_mutual_m_new$bear12*data_mutual_m_new$HML +
                      data_mutual_m_new$bear12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p9~data_mutual_m_new$bear12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$bear12*data_mutual_m_new$SMB +
                      data_mutual_m_new$bear12*data_mutual_m_new$HML +
                      data_mutual_m_new$bear12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p10~data_mutual_m_new$bear12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$bear12*data_mutual_m_new$SMB +
                      data_mutual_m_new$bear12*data_mutual_m_new$HML +
                      data_mutual_m_new$bear12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p11~data_mutual_m_new$bear12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$bear12*data_mutual_m_new$SMB +
                      data_mutual_m_new$bear12*data_mutual_m_new$HML +
                      data_mutual_m_new$bear12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p12~data_mutual_m_new$bear12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$bear12*data_mutual_m_new$SMB +
                      data_mutual_m_new$bear12*data_mutual_m_new$HML +
                      data_mutual_m_new$bear12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p13~data_mutual_m_new$bear12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$bear12*data_mutual_m_new$SMB +
                      data_mutual_m_new$bear12*data_mutual_m_new$HML +
                      data_mutual_m_new$bear12*data_mutual_m_new$WML)
    summary(model_new)
    
    ##Market variance indicators high
    
    data_mutual_m_new <- subset(data_mutual_m, !is.na(data_mutual_m$var_high_12))
    model_new <- lm(data_mutual_m_new$p1~data_mutual_m_new$var_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p2~data_mutual_m_new$var_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p3~data_mutual_m_new$var_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p4~data_mutual_m_new$var_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p5~data_mutual_m_new$var_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p6~data_mutual_m_new$var_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p7~data_mutual_m_new$var_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p8~data_mutual_m_new$var_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p9~data_mutual_m_new$var_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p10~data_mutual_m_new$var_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_mutual_m_new$p11~data_mutual_m_new$var_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p12~data_mutual_m_new$var_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p13~data_mutual_m_new$var_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_high_12*data_mutual_m_new$WML)
    summary(model_new)
    
    ##Market variance indicators low
    
    model_new <- lm(data_mutual_m_new$p1~data_mutual_m_new$var_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p2~data_mutual_m_new$var_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p3~data_mutual_m_new$var_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p4~data_mutual_m_new$var_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p5~data_mutual_m_new$var_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p6~data_mutual_m_new$var_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p7~data_mutual_m_new$var_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p8~data_mutual_m_new$var_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p9~data_mutual_m_new$var_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p10~data_mutual_m_new$var_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p11~data_mutual_m_new$var_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p12~data_mutual_m_new$var_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p13~data_mutual_m_new$var_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$var_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$var_low_12*data_mutual_m_new$WML)
    summary(model_new)
    
    ##Variance risk indicators (swap returns) high##
    
    data_mutual_m_new <- subset(data_mutual_m, !is.na(data_mutual_m$swap_high_12))
    
    model_new <- lm(data_mutual_m_new$p1~data_mutual_m_new$swap_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p2~data_mutual_m_new$swap_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p3~data_mutual_m_new$swap_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p4~data_mutual_m_new$swap_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p5~data_mutual_m_new$swap_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p6~data_mutual_m_new$swap_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p7~data_mutual_m_new$swap_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p8~data_mutual_m_new$swap_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p9~data_mutual_m_new$swap_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p10~data_mutual_m_new$swap_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p11~data_mutual_m_new$swap_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p12~data_mutual_m_new$swap_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p13~data_mutual_m_new$swap_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_high_12*data_mutual_m_new$WML)
    summary(model_new)
    
    ##Variance risk indicators (swap returns) low##
    
    model_new <- lm(data_mutual_m_new$p1~data_mutual_m_new$swap_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p2~data_mutual_m_new$swap_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p3~data_mutual_m_new$swap_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p4~data_mutual_m_new$swap_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p5~data_mutual_m_new$swap_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p6~data_mutual_m_new$swap_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p7~data_mutual_m_new$swap_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p8~data_mutual_m_new$swap_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p9~data_mutual_m_new$swap_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p10~data_mutual_m_new$swap_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p11~data_mutual_m_new$swap_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p12~data_mutual_m_new$swap_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p13~data_mutual_m_new$swap_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$swap_low_12*data_mutual_m_new$WML)
    summary(model_new)
    
    ### Short-term interest rate indicators high #
    
    data_mutual_m_new <- subset(data_mutual_m, !is.na(data_mutual_m$int_high_12))
    
    model_new <- lm(data_mutual_m_new$p1~data_mutual_m_new$int_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p2~data_mutual_m_new$int_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p3~data_mutual_m_new$int_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p4~data_mutual_m_new$int_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p5~data_mutual_m_new$int_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p6~data_mutual_m_new$int_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p7~data_mutual_m_new$int_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p8~data_mutual_m_new$int_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p9~data_mutual_m_new$int_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p10~data_mutual_m_new$int_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p11~data_mutual_m_new$int_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p12~data_mutual_m_new$int_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p13~data_mutual_m_new$int_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_high_12*data_mutual_m_new$WML)
    summary(model_new)
    
    ### Short-term interest rate indicators low #
    
    model_new <- lm(data_mutual_m_new$p1~data_mutual_m_new$int_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$WML)
    
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p2~data_mutual_m_new$int_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$WML)
    
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p3~data_mutual_m_new$int_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$WML)
    
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p4~data_mutual_m_new$int_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$WML)
    
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p5~data_mutual_m_new$int_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$WML)
    
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p6~data_mutual_m_new$int_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$WML)
    
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p7~data_mutual_m_new$int_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$WML)
    
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p8~data_mutual_m_new$int_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$WML)
    
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p9~data_mutual_m_new$int_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$WML)
    
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p10~data_mutual_m_new$int_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$WML)
    
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p11~data_mutual_m_new$int_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$WML)
    
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p12~data_mutual_m_new$int_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$WML)
    
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p13~data_mutual_m_new$int_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$int_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$int_low_12*data_mutual_m_new$WML)
    
    summary(model_new)
    
    # Recession indicator #
    
    data_mutual_m_new <- subset(data_mutual_m, !is.na(data_mutual_m$recession))
    
    model_new <- lm(data_mutual_m_new$p1~data_mutual_m_new$recession*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$recession*data_mutual_m_new$SMB +
                      data_mutual_m_new$recession*data_mutual_m_new$HML +
                      data_mutual_m_new$recession*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p2~data_mutual_m_new$recession*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$recession*data_mutual_m_new$SMB +
                      data_mutual_m_new$recession*data_mutual_m_new$HML +
                      data_mutual_m_new$recession*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p3~data_mutual_m_new$recession*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$recession*data_mutual_m_new$SMB +
                      data_mutual_m_new$recession*data_mutual_m_new$HML +
                      data_mutual_m_new$recession*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p4~data_mutual_m_new$recession*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$recession*data_mutual_m_new$SMB +
                      data_mutual_m_new$recession*data_mutual_m_new$HML +
                      data_mutual_m_new$recession*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p5~data_mutual_m_new$recession*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$recession*data_mutual_m_new$SMB +
                      data_mutual_m_new$recession*data_mutual_m_new$HML +
                      data_mutual_m_new$recession*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p6~data_mutual_m_new$recession*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$recession*data_mutual_m_new$SMB +
                      data_mutual_m_new$recession*data_mutual_m_new$HML +
                      data_mutual_m_new$recession*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p7~data_mutual_m_new$recession*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$recession*data_mutual_m_new$SMB +
                      data_mutual_m_new$recession*data_mutual_m_new$HML +
                      data_mutual_m_new$recession*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p8~data_mutual_m_new$recession*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$recession*data_mutual_m_new$SMB +
                      data_mutual_m_new$recession*data_mutual_m_new$HML +
                      data_mutual_m_new$recession*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p9~data_mutual_m_new$recession*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$recession*data_mutual_m_new$SMB +
                      data_mutual_m_new$recession*data_mutual_m_new$HML +
                      data_mutual_m_new$recession*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p10~data_mutual_m_new$recession*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$recession*data_mutual_m_new$SMB +
                      data_mutual_m_new$recession*data_mutual_m_new$HML +
                      data_mutual_m_new$recession*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p11~data_mutual_m_new$recession*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$recession*data_mutual_m_new$SMB +
                      data_mutual_m_new$recession*data_mutual_m_new$HML +
                      data_mutual_m_new$recession*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p12~data_mutual_m_new$recession*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$recession*data_mutual_m_new$SMB +
                      data_mutual_m_new$recession*data_mutual_m_new$HML +
                      data_mutual_m_new$recession*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p13~data_mutual_m_new$recession*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$recession*data_mutual_m_new$SMB +
                      data_mutual_m_new$recession*data_mutual_m_new$HML +
                      data_mutual_m_new$recession*data_mutual_m_new$WML)
    summary(model_new)
 
    # Dividend yield indicator high #
    
    data_mutual_m_new <- subset(data_mutual_m, !is.na(data_mutual_m$yield_high_12))
    
    model_new <- lm(data_mutual_m_new$p1~data_mutual_m_new$yield_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p2~data_mutual_m_new$yield_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p3~data_mutual_m_new$yield_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p4~data_mutual_m_new$yield_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p5~data_mutual_m_new$yield_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p6~data_mutual_m_new$yield_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p7~data_mutual_m_new$yield_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p8~data_mutual_m_new$yield_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p9~data_mutual_m_new$yield_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p10~data_mutual_m_new$yield_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p11~data_mutual_m_new$yield_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p12~data_mutual_m_new$yield_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p13~data_mutual_m_new$yield_high_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_high_12*data_mutual_m_new$WML)
    summary(model_new)
    
    # Dividend yield indicator low #
    
    model_new <- lm(data_mutual_m_new$p1~data_mutual_m_new$yield_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$WML)
    summary(model_new)
    model_new <- lm(data_mutual_m_new$p2~data_mutual_m_new$yield_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_mutual_m_new$p3~data_mutual_m_new$yield_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_mutual_m_new$p4~data_mutual_m_new$yield_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_mutual_m_new$p5~data_mutual_m_new$yield_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_mutual_m_new$p6~data_mutual_m_new$yield_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_mutual_m_new$p7~data_mutual_m_new$yield_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_mutual_m_new$p8~data_mutual_m_new$yield_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_mutual_m_new$p9~data_mutual_m_new$yield_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_mutual_m_new$p10~data_mutual_m_new$yield_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_mutual_m_new$p11~data_mutual_m_new$yield_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_mutual_m_new$p12~data_mutual_m_new$yield_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_mutual_m_new$p13~data_mutual_m_new$yield_low_12*data_mutual_m_new$MKT.RF + 
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$SMB +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$HML +
                      data_mutual_m_new$yield_low_12*data_mutual_m_new$WML)
    summary(model_new)
    
    
    objects <- ls()
    objects_to_remove <- objects[!objects %in% c("data_etf", "data_mutual", "data_etf_m", "data_mutual_m")]
    rm(list = objects_to_remove)
    #####################################################
    #########Table 7 - Individual Results (ETFs)#########
    ############################################
    
    ##Bear Market indicator##
    
    data_etf_m_new <- subset(data_etf_m, !is.na(data_etf_m$bear12))
    model_new <- lm(data_etf_m_new$p1~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p2~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p3~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p4~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p5~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p6~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p7~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p8~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p9~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p10~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p11~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p12~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p13~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p14~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p15~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p16~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p17~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p18~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p19~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p20~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p21~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p22~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p23~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p24~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p25~data_etf_m_new$bear12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$bear12*data_etf_m_new$SMB +
                      data_etf_m_new$bear12*data_etf_m_new$HML +
                      data_etf_m_new$bear12*data_etf_m_new$WML)
    summary(model_new)
    
    # Market variance indicator high#
    
    data_etf_m_new <- subset(data_etf_m, !is.na(data_etf_m$swap_high_12))
    
    model_new <- lm(data_etf_m_new$p1~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p2~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p3~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p4~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p5~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p6~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p7~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p8~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p9~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p10~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p11~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p12~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p13~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p14~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p15~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p16~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p17~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p18~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p19~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p20~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p21~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p22~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p23~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p24~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p25~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    
    # Market variance indicator low#
    
    model_new <- lm(data_etf_m_new$p1~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p2~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p3~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p4~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p5~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p6~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p7~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p8~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p9~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p10~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p11~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p12~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p13~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p14~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p15~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p16~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p17~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p18~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p19~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p20~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p21~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p22~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p23~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p24~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p25~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    
    # Short-term interest rate indicators high#
    
    data_etf_m_new <- subset(data_etf_m, !is.na(data_etf_m$int_high_12))
    
    model_new <- lm(data_etf_m_new$p1~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p2~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p3~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p4~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p5~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p6~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p7~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p8~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p9~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p10~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p11~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p12~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p13~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p14~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p15~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p16~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p17~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p18~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p19~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p20~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p21~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p22~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p23~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p24~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p25~data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_high_12*data_etf_m_new$HML +
                      data_etf_m_new$int_high_12*data_etf_m_new$WML)
    summary(model_new)
    
    # Short-term interest rate indicators low#
    
    model_new <- lm(data_etf_m_new$p1~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p2~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p3~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p4~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p5~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p6~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p7~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p8~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p9~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p10~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p11~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p12~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p13~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p14~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p15~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p16~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    
    model_new <- lm(data_etf_m_new$p17~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p18~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p19~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p20~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p21~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p22~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p23~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p24~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p25~data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$int_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$int_low_12*data_etf_m_new$HML +
                      data_etf_m_new$int_low_12*data_etf_m_new$WML)
    summary(model_new)
   
    ###Recession indicator###
    
    data_etf_m_new <- subset(data_etf_m, !is.na(data_etf_m$recession))
    
    model_new <- lm(data_etf_m_new$p1~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p2~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p3~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p4~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p5~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p6~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p7~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p8~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p9~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p10~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p11~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p12~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p13~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p14~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p15~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p16~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p17~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p18~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p19~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p20~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p21~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p22~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p23~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p24~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p25~data_etf_m_new$recession*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$recession*data_etf_m_new$SMB +
                      data_etf_m_new$recession*data_etf_m_new$HML +
                      data_etf_m_new$recession*data_etf_m_new$WML)
    summary(model_new)
    
    
    ####Divident yield indicator high###
    
    data_etf_m_new <- subset(data_etf_m, !is.na(data_etf_m$yield_high_12))
    
    model_new <- lm(data_etf_m_new$p1~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p2~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p3~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p4~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p5~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p6~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p7~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p8~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p9~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p10~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p11~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p12~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p13~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p14~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p15~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p16~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p17~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p18~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p19~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p21~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p22~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p23~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p24~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p25~data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_high_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_high_12*data_etf_m_new$WML)
    summary(model_new)
    
    ####Divident yield indicator low###
    
    model_new <- lm(data_etf_m_new$p1~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p2~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p3~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p4~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p5~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p6~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p7~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p8~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p9~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p10~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p11~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p12~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p13~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p14~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p15~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p16~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p17~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p18~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p19~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p20~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p21~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p22~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p23~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p24~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p25~data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$yield_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$yield_low_12*data_etf_m_new$HML +
                      data_etf_m_new$yield_low_12*data_etf_m_new$WML)
    summary(model_new)
    
    # Variance risk indicators (swap returns) high#
    
    data_etf_m_new <- subset(data_etf_m, !is.na(data_etf_m$swap_high_12))
    
    model_new <- lm(data_etf_m_new$p1~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p2~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p3~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p4~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p5~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p6~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p7~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p8~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p9~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p10~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p11~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p12~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p13~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p14~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p15~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p16~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p17~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p18~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p19~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p20~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p21~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p22~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p23~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p24~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p25~data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_high_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_high_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_high_12*data_etf_m_new$WML)
    summary(model_new)
    
    
    # Variance risk indicators (swap returns) low#
    
    model_new <- lm(data_etf_m_new$p1~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p2~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p3~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p4~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p5~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p6~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p7~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p8~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p9~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p10~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p11~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p12~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p13~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p14~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p15~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p16~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p17~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p18~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p19~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p20~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p21~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p22~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p23~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p24~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    model_new <- lm(data_etf_m_new$p25~data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF + 
                      data_etf_m_new$swap_low_12*data_etf_m_new$SMB +
                      data_etf_m_new$swap_low_12*data_etf_m_new$HML +
                      data_etf_m_new$swap_low_12*data_etf_m_new$WML)
    summary(model_new)
    
    
    objects <- ls()
    objects_to_remove <- objects[!objects %in% c("data_etf", "data_mutual", "data_etf_m", "data_mutual_m")]
    rm(list = objects_to_remove)
    ###############################################
    #########Table 5 - Lasso regressions###########
    ###############################################
    
    #ETF
    
    #EW
    
    #Info: Removal of missing observations for swap return indicator
    #      since most observations missing here 
    #Resulting sample period: 201701 - 202303 
    
    
    ###Avg Portfolio (26)###
    data_etf_m_new <- subset(data_etf_m, !is.na(data_etf_m$swap_high_12))
    summary(data_etf_m_new)
    
    data <- data.frame(1:nrow(data_etf_m_new), NA)
    
    # Y variable
    data$excess <- data_etf_m_new$p26
    
    # x no interactions
    
    data$Mkt.RF <- data_etf_m_new$MKT.RF
    data$SMB <- data_etf_m_new$SMB
    data$HML <- data_etf_m_new$HML
    data$WML <- data_etf_m_new$WML
    
    data$bear9 <- data_etf_m_new$bear9
    data$bear12 <- data_etf_m_new$bear12
    
    data$var_high_12 <- data_etf_m_new$var_high_12
    data$var_high_9 <- data_etf_m_new$var_high_9
    data$var_low_12 <- data_etf_m_new$var_low_12
    data$var_low_9 <- data_etf_m_new$var_low_9
    
    data$swap_high_12 <- data_etf_m_new$swap_high_12
    data$swap_high_9 <- data_etf_m_new$swap_high_9
    data$swap_low_12 <- data_etf_m_new$swap_low_12
    data$swap_low_9 <- data_etf_m_new$swap_low_9
    
    
    data$int_high_12 <- data_etf_m_new$int_high_12
    data$int_high_9 <- data_etf_m_new$int_high_9
    data$int_low_12 <- data_etf_m_new$int_low_12
    data$int_low_9 <- data_etf_m_new$int_low_9
    
    
    data$recession <- data_etf_m_new$recession
    
    data$yield_high_12 <- data_etf_m_new$yield_high_12
    data$yield_high_9 <- data_etf_m_new$yield_high_9
    data$yield_low_12 <- data_etf_m_new$yield_low_12
    data$yield_low_9 <- data_etf_m_new$yield_low_9
    
    # x  interactions
    
    data$bear12_mkt <- data_etf_m_new$bear12*data_etf_m_new$MKT.RF
    data$bear12_smb <- data_etf_m_new$bear12*data_etf_m_new$SMB
    data$bear12_hml <- data_etf_m_new$bear12*data_etf_m_new$HML
    data$bear12_wml <- data_etf_m_new$bear12*data_etf_m_new$WML
    data$bear9_mkt <- data_etf_m_new$bear9*data_etf_m_new$MKT.RF
    data$bear9_smb <- data_etf_m_new$bear9*data_etf_m_new$SMB
    data$bear9_hml <- data_etf_m_new$bear9*data_etf_m_new$HML
    data$bear9_wml <- data_etf_m_new$bear9*data_etf_m_new$WML
    
    data$var_high_12_mkt <- data_etf_m_new$var_high_12*data_etf_m_new$MKT.RF
    data$var_high_12_smb <- data_etf_m_new$var_high_12*data_etf_m_new$SMB
    data$var_high_12_hml <- data_etf_m_new$var_high_12*data_etf_m_new$HML
    data$var_high_12_wml <- data_etf_m_new$var_high_12*data_etf_m_new$WML
    data$var_high_9_mkt <- data_etf_m_new$var_high_9*data_etf_m_new$MKT.RF
    data$var_high_9_smb <- data_etf_m_new$var_high_9*data_etf_m_new$SMB
    data$var_high_9_hml <- data_etf_m_new$var_high_9*data_etf_m_new$HML
    data$var_high_9_wml <- data_etf_m_new$var_high_9*data_etf_m_new$WML
    data$var_low_12_mkt <- data_etf_m_new$var_low_12*data_etf_m_new$MKT.RF
    data$var_low_12_smb <- data_etf_m_new$var_low_12*data_etf_m_new$SMB
    data$var_low_12_hml <- data_etf_m_new$var_low_12*data_etf_m_new$HML
    data$var_low_12_wml <- data_etf_m_new$var_low_12*data_etf_m_new$WML
    data$var_low_9_mkt <- data_etf_m_new$var_low_9*data_etf_m_new$MKT.RF
    data$var_low_9_smb <- data_etf_m_new$var_low_9*data_etf_m_new$SMB
    data$var_low_9_hml <- data_etf_m_new$var_low_9*data_etf_m_new$HML
    data$var_low_9_wml <- data_etf_m_new$var_low_9*data_etf_m_new$WML
    
    
    data$swap_high_12_mkt <- data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF
    data$swap_high_12_smb <- data_etf_m_new$swap_high_12*data_etf_m_new$SMB
    data$swap_high_12_hml <- data_etf_m_new$swap_high_12*data_etf_m_new$HML
    data$swap_high_12_wml <- data_etf_m_new$swap_high_12*data_etf_m_new$WML
    data$swap_high_9_mkt <- data_etf_m_new$swap_high_9*data_etf_m_new$MKT.RF
    data$swap_high_9_smb <- data_etf_m_new$swap_high_9*data_etf_m_new$SMB
    data$swap_high_9_hml <- data_etf_m_new$swap_high_9*data_etf_m_new$HML
    data$swap_high_9_wml <- data_etf_m_new$swap_high_9*data_etf_m_new$WML
    data$swap_low_12_mkt <- data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF
    data$swap_low_12_smb <- data_etf_m_new$swap_low_12*data_etf_m_new$SMB
    data$swap_low_12_hml <- data_etf_m_new$swap_low_12*data_etf_m_new$HML
    data$swap_low_12_wml <- data_etf_m_new$swap_low_12*data_etf_m_new$WML
    data$swap_low_9_mkt <- data_etf_m_new$swap_low_9*data_etf_m_new$MKT.RF
    data$swap_low_9_smb <- data_etf_m_new$swap_low_9*data_etf_m_new$SMB
    data$swap_low_9_hml <- data_etf_m_new$swap_low_9*data_etf_m_new$HML
    data$swap_low_9_wml <- data_etf_m_new$swap_low_9*data_etf_m_new$WML
    
    
    data$int_high_12_mkt <- data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF
    data$int_high_12_smb <- data_etf_m_new$int_high_12*data_etf_m_new$SMB
    data$int_high_12_hml <- data_etf_m_new$int_high_12*data_etf_m_new$HML
    data$int_high_12_wml <- data_etf_m_new$int_high_12*data_etf_m_new$WML
    data$int_high_9_mkt <- data_etf_m_new$int_high_9*data_etf_m_new$MKT.RF
    data$int_high_9_smb <- data_etf_m_new$int_high_9*data_etf_m_new$SMB
    data$int_high_9_hml <- data_etf_m_new$int_high_9*data_etf_m_new$HML
    data$int_high_9_wml <- data_etf_m_new$int_high_9*data_etf_m_new$WML
    data$int_low_12_mkt <- data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF
    data$int_low_12_smb <- data_etf_m_new$int_low_12*data_etf_m_new$SMB
    data$int_low_12_hml <- data_etf_m_new$int_low_12*data_etf_m_new$HML
    data$int_low_12_wml <- data_etf_m_new$int_low_12*data_etf_m_new$WML
    data$int_low_9_mkt <- data_etf_m_new$int_low_9*data_etf_m_new$MKT.RF
    data$int_low_9_smb <- data_etf_m_new$int_low_9*data_etf_m_new$SMB
    data$int_low_9_hml <- data_etf_m_new$int_low_9*data_etf_m_new$HML
    data$int_low_9_wml <- data_etf_m_new$int_low_9*data_etf_m_new$WML
    
    
    data$recession_mkt <- data_etf_m_new$recession*data_etf_m_new$MKT.RF
    data$recession_smb <- data_etf_m_new$recession*data_etf_m_new$SMB
    data$recession_hml <- data_etf_m_new$recession*data_etf_m_new$HML
    data$recession_wml <- data_etf_m_new$recession*data_etf_m_new$WML
    
    data$yield_high_12_mkt <- data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF
    data$yield_high_12_smb <- data_etf_m_new$yield_high_12*data_etf_m_new$SMB
    data$yield_high_12_hml <- data_etf_m_new$yield_high_12*data_etf_m_new$HML
    data$yield_high_12_wml <- data_etf_m_new$yield_high_12*data_etf_m_new$WML
    data$yield_high_9_mkt <- data_etf_m_new$yield_high_9*data_etf_m_new$MKT.RF
    data$yield_high_9_smb <- data_etf_m_new$yield_high_9*data_etf_m_new$SMB
    data$yield_high_9_hml <- data_etf_m_new$yield_high_9*data_etf_m_new$HML
    data$yield_high_9_wml <- data_etf_m_new$yield_high_9*data_etf_m_new$WML
    data$yield_low_12_mkt <- data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF
    data$yield_low_12_smb <- data_etf_m_new$yield_low_12*data_etf_m_new$SMB
    data$yield_low_12_hml <- data_etf_m_new$yield_low_12*data_etf_m_new$HML
    data$yield_low_12_wml <- data_etf_m_new$yield_low_12*data_etf_m_new$WML
    data$yield_low_9_mkt <- data_etf_m_new$yield_low_9*data_etf_m_new$MKT.RF
    data$yield_low_9_smb <- data_etf_m_new$yield_low_9*data_etf_m_new$SMB
    data$yield_low_9_hml <- data_etf_m_new$yield_low_9*data_etf_m_new$HML
    data$yield_low_9_wml <- data_etf_m_new$yield_low_9*data_etf_m_new$WML
    
    
    data <- data[,-c(1:2)]
    
    # Center y, X will be standardized in the modelling function
    y <- data %>% select(excess) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
    X <- data %>% select(-excess) %>% as.matrix()
    
    # Perform 10-fold cross-validation to select lambda
    lambdas_to_try <- 10^seq(-4, 4, length.out = 50)
    # Setting alpha = 0 implements ridge regression
    lasso_cv <- cv.glmnet(X, y, alpha = 1, lambda = lambdas_to_try,
                          standardize = TRUE, nfolds = 10)
    # Plot cross-validation results
    plot(lasso_cv)
    # Best cross-validated lambda
    lambda_cv <- lasso_cv$lambda.min
    lambda_cv
    # Fit final model
    model_cv <- glmnet(X, y, alpha = 1, lambda = lambda_cv, standardize = TRUE)
    coef(model_cv)
    
    ##PF 14##
    data_etf_m_new <- subset(data_etf_m, !is.na(data_etf_m$swap_high_12))
    summary(data_etf_m_new)
    
    data <- data.frame(1:nrow(data_etf_m_new), NA)
    
    # Y variable
    data$excess <- data_etf_m_new$p14
    
    # x no interactions
    
    data$Mkt.RF <- data_etf_m_new$MKT.RF
    data$SMB <- data_etf_m_new$SMB
    data$HML <- data_etf_m_new$HML
    data$WML <- data_etf_m_new$WML
    
    data$bear9 <- data_etf_m_new$bear9
    data$bear12 <- data_etf_m_new$bear12
    
    data$var_high_12 <- data_etf_m_new$var_high_12
    data$var_high_9 <- data_etf_m_new$var_high_9
    data$var_low_12 <- data_etf_m_new$var_low_12
    data$var_low_9 <- data_etf_m_new$var_low_9
    
    data$swap_high_12 <- data_etf_m_new$swap_high_12
    data$swap_high_9 <- data_etf_m_new$swap_high_9
    data$swap_low_12 <- data_etf_m_new$swap_low_12
    data$swap_low_9 <- data_etf_m_new$swap_low_9
    
    
    data$int_high_12 <- data_etf_m_new$int_high_12
    data$int_high_9 <- data_etf_m_new$int_high_9
    data$int_low_12 <- data_etf_m_new$int_low_12
    data$int_low_9 <- data_etf_m_new$int_low_9
    
    
    data$recession <- data_etf_m_new$recession
    
    data$yield_high_12 <- data_etf_m_new$yield_high_12
    data$yield_high_9 <- data_etf_m_new$yield_high_9
    data$yield_low_12 <- data_etf_m_new$yield_low_12
    data$yield_low_9 <- data_etf_m_new$yield_low_9
    
    # x  interactions
    
    data$bear12_mkt <- data_etf_m_new$bear12*data_etf_m_new$MKT.RF
    data$bear12_smb <- data_etf_m_new$bear12*data_etf_m_new$SMB
    data$bear12_hml <- data_etf_m_new$bear12*data_etf_m_new$HML
    data$bear12_wml <- data_etf_m_new$bear12*data_etf_m_new$WML
    data$bear9_mkt <- data_etf_m_new$bear9*data_etf_m_new$MKT.RF
    data$bear9_smb <- data_etf_m_new$bear9*data_etf_m_new$SMB
    data$bear9_hml <- data_etf_m_new$bear9*data_etf_m_new$HML
    data$bear9_wml <- data_etf_m_new$bear9*data_etf_m_new$WML
    
    data$var_high_12_mkt <- data_etf_m_new$var_high_12*data_etf_m_new$MKT.RF
    data$var_high_12_smb <- data_etf_m_new$var_high_12*data_etf_m_new$SMB
    data$var_high_12_hml <- data_etf_m_new$var_high_12*data_etf_m_new$HML
    data$var_high_12_wml <- data_etf_m_new$var_high_12*data_etf_m_new$WML
    data$var_high_9_mkt <- data_etf_m_new$var_high_9*data_etf_m_new$MKT.RF
    data$var_high_9_smb <- data_etf_m_new$var_high_9*data_etf_m_new$SMB
    data$var_high_9_hml <- data_etf_m_new$var_high_9*data_etf_m_new$HML
    data$var_high_9_wml <- data_etf_m_new$var_high_9*data_etf_m_new$WML
    data$var_low_12_mkt <- data_etf_m_new$var_low_12*data_etf_m_new$MKT.RF
    data$var_low_12_smb <- data_etf_m_new$var_low_12*data_etf_m_new$SMB
    data$var_low_12_hml <- data_etf_m_new$var_low_12*data_etf_m_new$HML
    data$var_low_12_wml <- data_etf_m_new$var_low_12*data_etf_m_new$WML
    data$var_low_9_mkt <- data_etf_m_new$var_low_9*data_etf_m_new$MKT.RF
    data$var_low_9_smb <- data_etf_m_new$var_low_9*data_etf_m_new$SMB
    data$var_low_9_hml <- data_etf_m_new$var_low_9*data_etf_m_new$HML
    data$var_low_9_wml <- data_etf_m_new$var_low_9*data_etf_m_new$WML
    
    
    data$swap_high_12_mkt <- data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF
    data$swap_high_12_smb <- data_etf_m_new$swap_high_12*data_etf_m_new$SMB
    data$swap_high_12_hml <- data_etf_m_new$swap_high_12*data_etf_m_new$HML
    data$swap_high_12_wml <- data_etf_m_new$swap_high_12*data_etf_m_new$WML
    data$swap_high_9_mkt <- data_etf_m_new$swap_high_9*data_etf_m_new$MKT.RF
    data$swap_high_9_smb <- data_etf_m_new$swap_high_9*data_etf_m_new$SMB
    data$swap_high_9_hml <- data_etf_m_new$swap_high_9*data_etf_m_new$HML
    data$swap_high_9_wml <- data_etf_m_new$swap_high_9*data_etf_m_new$WML
    data$swap_low_12_mkt <- data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF
    data$swap_low_12_smb <- data_etf_m_new$swap_low_12*data_etf_m_new$SMB
    data$swap_low_12_hml <- data_etf_m_new$swap_low_12*data_etf_m_new$HML
    data$swap_low_12_wml <- data_etf_m_new$swap_low_12*data_etf_m_new$WML
    data$swap_low_9_mkt <- data_etf_m_new$swap_low_9*data_etf_m_new$MKT.RF
    data$swap_low_9_smb <- data_etf_m_new$swap_low_9*data_etf_m_new$SMB
    data$swap_low_9_hml <- data_etf_m_new$swap_low_9*data_etf_m_new$HML
    data$swap_low_9_wml <- data_etf_m_new$swap_low_9*data_etf_m_new$WML
    
    
    data$int_high_12_mkt <- data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF
    data$int_high_12_smb <- data_etf_m_new$int_high_12*data_etf_m_new$SMB
    data$int_high_12_hml <- data_etf_m_new$int_high_12*data_etf_m_new$HML
    data$int_high_12_wml <- data_etf_m_new$int_high_12*data_etf_m_new$WML
    data$int_high_9_mkt <- data_etf_m_new$int_high_9*data_etf_m_new$MKT.RF
    data$int_high_9_smb <- data_etf_m_new$int_high_9*data_etf_m_new$SMB
    data$int_high_9_hml <- data_etf_m_new$int_high_9*data_etf_m_new$HML
    data$int_high_9_wml <- data_etf_m_new$int_high_9*data_etf_m_new$WML
    data$int_low_12_mkt <- data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF
    data$int_low_12_smb <- data_etf_m_new$int_low_12*data_etf_m_new$SMB
    data$int_low_12_hml <- data_etf_m_new$int_low_12*data_etf_m_new$HML
    data$int_low_12_wml <- data_etf_m_new$int_low_12*data_etf_m_new$WML
    data$int_low_9_mkt <- data_etf_m_new$int_low_9*data_etf_m_new$MKT.RF
    data$int_low_9_smb <- data_etf_m_new$int_low_9*data_etf_m_new$SMB
    data$int_low_9_hml <- data_etf_m_new$int_low_9*data_etf_m_new$HML
    data$int_low_9_wml <- data_etf_m_new$int_low_9*data_etf_m_new$WML
    
    
    data$recession_mkt <- data_etf_m_new$recession*data_etf_m_new$MKT.RF
    data$recession_smb <- data_etf_m_new$recession*data_etf_m_new$SMB
    data$recession_hml <- data_etf_m_new$recession*data_etf_m_new$HML
    data$recession_wml <- data_etf_m_new$recession*data_etf_m_new$WML
    
    data$yield_high_12_mkt <- data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF
    data$yield_high_12_smb <- data_etf_m_new$yield_high_12*data_etf_m_new$SMB
    data$yield_high_12_hml <- data_etf_m_new$yield_high_12*data_etf_m_new$HML
    data$yield_high_12_wml <- data_etf_m_new$yield_high_12*data_etf_m_new$WML
    data$yield_high_9_mkt <- data_etf_m_new$yield_high_9*data_etf_m_new$MKT.RF
    data$yield_high_9_smb <- data_etf_m_new$yield_high_9*data_etf_m_new$SMB
    data$yield_high_9_hml <- data_etf_m_new$yield_high_9*data_etf_m_new$HML
    data$yield_high_9_wml <- data_etf_m_new$yield_high_9*data_etf_m_new$WML
    data$yield_low_12_mkt <- data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF
    data$yield_low_12_smb <- data_etf_m_new$yield_low_12*data_etf_m_new$SMB
    data$yield_low_12_hml <- data_etf_m_new$yield_low_12*data_etf_m_new$HML
    data$yield_low_12_wml <- data_etf_m_new$yield_low_12*data_etf_m_new$WML
    data$yield_low_9_mkt <- data_etf_m_new$yield_low_9*data_etf_m_new$MKT.RF
    data$yield_low_9_smb <- data_etf_m_new$yield_low_9*data_etf_m_new$SMB
    data$yield_low_9_hml <- data_etf_m_new$yield_low_9*data_etf_m_new$HML
    data$yield_low_9_wml <- data_etf_m_new$yield_low_9*data_etf_m_new$WML
    
    
    data <- data[,-c(1:2)]
    
    # Center y, X will be standardized in the modelling function
    y <- data %>% select(excess) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
    X <- data %>% select(-excess) %>% as.matrix()
    
    # Perform 10-fold cross-validation to select lambda
    lambdas_to_try <- 10^seq(-4, 4, length.out = 50)
    # Setting alpha = 0 implements ridge regression
    lasso_cv <- cv.glmnet(X, y, alpha = 1, lambda = lambdas_to_try,
                          standardize = TRUE, nfolds = 10)
    # Plot cross-validation results
    plot(lasso_cv)
    # Best cross-validated lambda
    lambda_cv <- lasso_cv$lambda.min
    lambda_cv
    # Fit final model
    model_cv <- glmnet(X, y, alpha = 1, lambda = lambda_cv, standardize = TRUE)
    coef(model_cv)
    
    ##PF 5##
    data_etf_m_new <- subset(data_etf_m, !is.na(data_etf_m$swap_high_12))
    summary(data_etf_m_new)
    
    data <- data.frame(1:nrow(data_etf_m_new), NA)
    
    # Y variable
    data$excess <- data_etf_m_new$p5
    
    # x no interactions
    
    data$Mkt.RF <- data_etf_m_new$MKT.RF
    data$SMB <- data_etf_m_new$SMB
    data$HML <- data_etf_m_new$HML
    data$WML <- data_etf_m_new$WML
    
    data$bear9 <- data_etf_m_new$bear9
    data$bear12 <- data_etf_m_new$bear12
    
    data$var_high_12 <- data_etf_m_new$var_high_12
    data$var_high_9 <- data_etf_m_new$var_high_9
    data$var_low_12 <- data_etf_m_new$var_low_12
    data$var_low_9 <- data_etf_m_new$var_low_9
    
    data$swap_high_12 <- data_etf_m_new$swap_high_12
    data$swap_high_9 <- data_etf_m_new$swap_high_9
    data$swap_low_12 <- data_etf_m_new$swap_low_12
    data$swap_low_9 <- data_etf_m_new$swap_low_9
    
    
    data$int_high_12 <- data_etf_m_new$int_high_12
    data$int_high_9 <- data_etf_m_new$int_high_9
    data$int_low_12 <- data_etf_m_new$int_low_12
    data$int_low_9 <- data_etf_m_new$int_low_9
    
    
    data$recession <- data_etf_m_new$recession
    
    data$yield_high_12 <- data_etf_m_new$yield_high_12
    data$yield_high_9 <- data_etf_m_new$yield_high_9
    data$yield_low_12 <- data_etf_m_new$yield_low_12
    data$yield_low_9 <- data_etf_m_new$yield_low_9
    
    # x  interactions
    
    data$bear12_mkt <- data_etf_m_new$bear12*data_etf_m_new$MKT.RF
    data$bear12_smb <- data_etf_m_new$bear12*data_etf_m_new$SMB
    data$bear12_hml <- data_etf_m_new$bear12*data_etf_m_new$HML
    data$bear12_wml <- data_etf_m_new$bear12*data_etf_m_new$WML
    data$bear9_mkt <- data_etf_m_new$bear9*data_etf_m_new$MKT.RF
    data$bear9_smb <- data_etf_m_new$bear9*data_etf_m_new$SMB
    data$bear9_hml <- data_etf_m_new$bear9*data_etf_m_new$HML
    data$bear9_wml <- data_etf_m_new$bear9*data_etf_m_new$WML
    
    data$var_high_12_mkt <- data_etf_m_new$var_high_12*data_etf_m_new$MKT.RF
    data$var_high_12_smb <- data_etf_m_new$var_high_12*data_etf_m_new$SMB
    data$var_high_12_hml <- data_etf_m_new$var_high_12*data_etf_m_new$HML
    data$var_high_12_wml <- data_etf_m_new$var_high_12*data_etf_m_new$WML
    data$var_high_9_mkt <- data_etf_m_new$var_high_9*data_etf_m_new$MKT.RF
    data$var_high_9_smb <- data_etf_m_new$var_high_9*data_etf_m_new$SMB
    data$var_high_9_hml <- data_etf_m_new$var_high_9*data_etf_m_new$HML
    data$var_high_9_wml <- data_etf_m_new$var_high_9*data_etf_m_new$WML
    data$var_low_12_mkt <- data_etf_m_new$var_low_12*data_etf_m_new$MKT.RF
    data$var_low_12_smb <- data_etf_m_new$var_low_12*data_etf_m_new$SMB
    data$var_low_12_hml <- data_etf_m_new$var_low_12*data_etf_m_new$HML
    data$var_low_12_wml <- data_etf_m_new$var_low_12*data_etf_m_new$WML
    data$var_low_9_mkt <- data_etf_m_new$var_low_9*data_etf_m_new$MKT.RF
    data$var_low_9_smb <- data_etf_m_new$var_low_9*data_etf_m_new$SMB
    data$var_low_9_hml <- data_etf_m_new$var_low_9*data_etf_m_new$HML
    data$var_low_9_wml <- data_etf_m_new$var_low_9*data_etf_m_new$WML
    
    
    data$swap_high_12_mkt <- data_etf_m_new$swap_high_12*data_etf_m_new$MKT.RF
    data$swap_high_12_smb <- data_etf_m_new$swap_high_12*data_etf_m_new$SMB
    data$swap_high_12_hml <- data_etf_m_new$swap_high_12*data_etf_m_new$HML
    data$swap_high_12_wml <- data_etf_m_new$swap_high_12*data_etf_m_new$WML
    data$swap_high_9_mkt <- data_etf_m_new$swap_high_9*data_etf_m_new$MKT.RF
    data$swap_high_9_smb <- data_etf_m_new$swap_high_9*data_etf_m_new$SMB
    data$swap_high_9_hml <- data_etf_m_new$swap_high_9*data_etf_m_new$HML
    data$swap_high_9_wml <- data_etf_m_new$swap_high_9*data_etf_m_new$WML
    data$swap_low_12_mkt <- data_etf_m_new$swap_low_12*data_etf_m_new$MKT.RF
    data$swap_low_12_smb <- data_etf_m_new$swap_low_12*data_etf_m_new$SMB
    data$swap_low_12_hml <- data_etf_m_new$swap_low_12*data_etf_m_new$HML
    data$swap_low_12_wml <- data_etf_m_new$swap_low_12*data_etf_m_new$WML
    data$swap_low_9_mkt <- data_etf_m_new$swap_low_9*data_etf_m_new$MKT.RF
    data$swap_low_9_smb <- data_etf_m_new$swap_low_9*data_etf_m_new$SMB
    data$swap_low_9_hml <- data_etf_m_new$swap_low_9*data_etf_m_new$HML
    data$swap_low_9_wml <- data_etf_m_new$swap_low_9*data_etf_m_new$WML
    
    
    data$int_high_12_mkt <- data_etf_m_new$int_high_12*data_etf_m_new$MKT.RF
    data$int_high_12_smb <- data_etf_m_new$int_high_12*data_etf_m_new$SMB
    data$int_high_12_hml <- data_etf_m_new$int_high_12*data_etf_m_new$HML
    data$int_high_12_wml <- data_etf_m_new$int_high_12*data_etf_m_new$WML
    data$int_high_9_mkt <- data_etf_m_new$int_high_9*data_etf_m_new$MKT.RF
    data$int_high_9_smb <- data_etf_m_new$int_high_9*data_etf_m_new$SMB
    data$int_high_9_hml <- data_etf_m_new$int_high_9*data_etf_m_new$HML
    data$int_high_9_wml <- data_etf_m_new$int_high_9*data_etf_m_new$WML
    data$int_low_12_mkt <- data_etf_m_new$int_low_12*data_etf_m_new$MKT.RF
    data$int_low_12_smb <- data_etf_m_new$int_low_12*data_etf_m_new$SMB
    data$int_low_12_hml <- data_etf_m_new$int_low_12*data_etf_m_new$HML
    data$int_low_12_wml <- data_etf_m_new$int_low_12*data_etf_m_new$WML
    data$int_low_9_mkt <- data_etf_m_new$int_low_9*data_etf_m_new$MKT.RF
    data$int_low_9_smb <- data_etf_m_new$int_low_9*data_etf_m_new$SMB
    data$int_low_9_hml <- data_etf_m_new$int_low_9*data_etf_m_new$HML
    data$int_low_9_wml <- data_etf_m_new$int_low_9*data_etf_m_new$WML
    
    
    data$recession_mkt <- data_etf_m_new$recession*data_etf_m_new$MKT.RF
    data$recession_smb <- data_etf_m_new$recession*data_etf_m_new$SMB
    data$recession_hml <- data_etf_m_new$recession*data_etf_m_new$HML
    data$recession_wml <- data_etf_m_new$recession*data_etf_m_new$WML
    
    data$yield_high_12_mkt <- data_etf_m_new$yield_high_12*data_etf_m_new$MKT.RF
    data$yield_high_12_smb <- data_etf_m_new$yield_high_12*data_etf_m_new$SMB
    data$yield_high_12_hml <- data_etf_m_new$yield_high_12*data_etf_m_new$HML
    data$yield_high_12_wml <- data_etf_m_new$yield_high_12*data_etf_m_new$WML
    data$yield_high_9_mkt <- data_etf_m_new$yield_high_9*data_etf_m_new$MKT.RF
    data$yield_high_9_smb <- data_etf_m_new$yield_high_9*data_etf_m_new$SMB
    data$yield_high_9_hml <- data_etf_m_new$yield_high_9*data_etf_m_new$HML
    data$yield_high_9_wml <- data_etf_m_new$yield_high_9*data_etf_m_new$WML
    data$yield_low_12_mkt <- data_etf_m_new$yield_low_12*data_etf_m_new$MKT.RF
    data$yield_low_12_smb <- data_etf_m_new$yield_low_12*data_etf_m_new$SMB
    data$yield_low_12_hml <- data_etf_m_new$yield_low_12*data_etf_m_new$HML
    data$yield_low_12_wml <- data_etf_m_new$yield_low_12*data_etf_m_new$WML
    data$yield_low_9_mkt <- data_etf_m_new$yield_low_9*data_etf_m_new$MKT.RF
    data$yield_low_9_smb <- data_etf_m_new$yield_low_9*data_etf_m_new$SMB
    data$yield_low_9_hml <- data_etf_m_new$yield_low_9*data_etf_m_new$HML
    data$yield_low_9_wml <- data_etf_m_new$yield_low_9*data_etf_m_new$WML
    
    
    data <- data[,-c(1:2)]
    
    # Center y, X will be standardized in the modelling function
    y <- data %>% select(excess) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
    X <- data %>% select(-excess) %>% as.matrix()
    
    # Perform 10-fold cross-validation to select lambda
    lambdas_to_try <- 10^seq(-4, 4, length.out = 50)
    # Setting alpha = 0 implements ridge regression
    lasso_cv <- cv.glmnet(X, y, alpha = 1, lambda = lambdas_to_try,
                          standardize = TRUE, nfolds = 10)
    # Plot cross-validation results
    plot(lasso_cv)
    # Best cross-validated lambda
    lambda_cv <- lasso_cv$lambda.min
    lambda_cv
    # Fit final model
    model_cv <- glmnet(X, y, alpha = 1, lambda = lambda_cv, standardize = TRUE)
    coef(model_cv)
    
    
    #MFs
    #Ew_MF
    data_mutual_m_new <- subset(data_mutual_m, !is.na(data_mutual_m$swap_high_24))
    summary(data_mutual_m_new)
    
    data <- data.frame(1:nrow(data_mutual_m_new), NA)
    
    # Y variable
    data$excess <- data_mutual_m_new$p14
    
    # x no interactions
    
    data$Mkt.RF <- data_mutual_m_new$MKT.RF
    data$SMB <- data_mutual_m_new$SMB
    data$HML <- data_mutual_m_new$HML
    data$WML <- data_mutual_m_new$WML
    
    data$bear9 <- data_mutual_m_new$bear9
    data$bear12 <- data_mutual_m_new$bear12
    data$bear24 <- data_mutual_m_new$bear24
    
    data$var_high_24 <- data_mutual_m_new$var_high_24
    data$var_high_12 <- data_mutual_m_new$var_high_12
    data$var_high_9 <- data_mutual_m_new$var_high_9
    data$var_low_24 <- data_mutual_m_new$var_low_24
    data$var_low_12 <- data_mutual_m_new$var_low_12
    data$var_low_9 <- data_mutual_m_new$var_low_9
    
    data$swap_high_24 <- data_mutual_m_new$swap_high_24
    data$swap_high_12 <- data_mutual_m_new$swap_high_12
    data$swap_high_9 <- data_mutual_m_new$swap_high_9
    data$swap_low_24 <- data_mutual_m_new$swap_low_24
    data$swap_low_12 <- data_mutual_m_new$swap_low_12
    data$swap_low_9 <- data_mutual_m_new$swap_low_9
    
    data$int_high_24 <- data_mutual_m_new$int_high_24
    data$int_high_12 <- data_mutual_m_new$int_high_12
    data$int_high_9 <- data_mutual_m_new$int_high_9
    data$int_low_24 <- data_mutual_m_new$int_low_24
    data$int_low_12 <- data_mutual_m_new$int_low_12
    data$int_low_9 <- data_mutual_m_new$int_low_9
    
    
    data$recession <- data_mutual_m_new$recession
    
    data$yield_high_24 <- data_mutual_m_new$yield_high_24
    data$yield_high_12 <- data_mutual_m_new$yield_high_12
    data$yield_high_9 <- data_mutual_m_new$yield_high_9
    data$yield_low_24 <- data_mutual_m_new$yield_low_24
    data$yield_low_12 <- data_mutual_m_new$yield_low_12
    data$yield_low_9 <- data_mutual_m_new$yield_low_9
    
    # x  interactions
    
    data$bear24_mkt <- data_mutual_m_new$bear24*data_mutual_m_new$MKT.RF
    data$bear24_smb <- data_mutual_m_new$bear24*data_mutual_m_new$SMB
    data$bear24_hml <- data_mutual_m_new$bear24*data_mutual_m_new$HML
    data$bear24_wml <- data_mutual_m_new$bear24*data_mutual_m_new$WML
    data$bear12_mkt <- data_mutual_m_new$bear12*data_mutual_m_new$MKT.RF
    data$bear12_smb <- data_mutual_m_new$bear12*data_mutual_m_new$SMB
    data$bear12_hml <- data_mutual_m_new$bear12*data_mutual_m_new$HML
    data$bear12_wml <- data_mutual_m_new$bear12*data_mutual_m_new$WML
    data$bear9_mkt <- data_mutual_m_new$bear9*data_mutual_m_new$MKT.RF
    data$bear9_smb <- data_mutual_m_new$bear9*data_mutual_m_new$SMB
    data$bear9_hml <- data_mutual_m_new$bear9*data_mutual_m_new$HML
    data$bear9_wml <- data_mutual_m_new$bear9*data_mutual_m_new$WML
    
    
    data$var_high_24_mkt <- data_mutual_m_new$var_high_24*data_mutual_m_new$MKT.RF
    data$var_high_24_smb <- data_mutual_m_new$var_high_24*data_mutual_m_new$SMB
    data$var_high_24_hml <- data_mutual_m_new$var_high_24*data_mutual_m_new$HML
    data$var_high_24_wml <- data_mutual_m_new$var_high_24*data_mutual_m_new$WML
    data$var_high_12_mkt <- data_mutual_m_new$var_high_12*data_mutual_m_new$MKT.RF
    data$var_high_12_smb <- data_mutual_m_new$var_high_12*data_mutual_m_new$SMB
    data$var_high_12_hml <- data_mutual_m_new$var_high_12*data_mutual_m_new$HML
    data$var_high_12_wml <- data_mutual_m_new$var_high_12*data_mutual_m_new$WML
    data$var_high_9_mkt <- data_mutual_m_new$var_high_9*data_mutual_m_new$MKT.RF
    data$var_high_9_smb <- data_mutual_m_new$var_high_9*data_mutual_m_new$SMB
    data$var_high_9_hml <- data_mutual_m_new$var_high_9*data_mutual_m_new$HML
    data$var_high_9_wml <- data_mutual_m_new$var_high_9*data_mutual_m_new$WML
    data$var_low_24_mkt <- data_mutual_m_new$var_low_24*data_mutual_m_new$MKT.RF
    data$var_low_24_smb <- data_mutual_m_new$var_low_24*data_mutual_m_new$SMB
    data$var_low_24_hml <- data_mutual_m_new$var_low_24*data_mutual_m_new$HML
    data$var_low_24_wml <- data_mutual_m_new$var_low_24*data_mutual_m_new$WML
    data$var_low_12_mkt <- data_mutual_m_new$var_low_12*data_mutual_m_new$MKT.RF
    data$var_low_12_smb <- data_mutual_m_new$var_low_12*data_mutual_m_new$SMB
    data$var_low_12_hml <- data_mutual_m_new$var_low_12*data_mutual_m_new$HML
    data$var_low_12_wml <- data_mutual_m_new$var_low_12*data_mutual_m_new$WML
    data$var_low_9_mkt <- data_mutual_m_new$var_low_9*data_mutual_m_new$MKT.RF
    data$var_low_9_smb <- data_mutual_m_new$var_low_9*data_mutual_m_new$SMB
    data$var_low_9_hml <- data_mutual_m_new$var_low_9*data_mutual_m_new$HML
    data$var_low_9_wml <- data_mutual_m_new$var_low_9*data_mutual_m_new$WML
    
    data$swap_high_24_mkt <- data_mutual_m_new$swap_high_24*data_mutual_m_new$MKT.RF
    data$swap_high_24_smb <- data_mutual_m_new$swap_high_24*data_mutual_m_new$SMB
    data$swap_high_24_hml <- data_mutual_m_new$swap_high_24*data_mutual_m_new$HML
    data$swap_high_24_wml <- data_mutual_m_new$swap_high_24*data_mutual_m_new$WML
    data$swap_high_12_mkt <- data_mutual_m_new$swap_high_12*data_mutual_m_new$MKT.RF
    data$swap_high_12_smb <- data_mutual_m_new$swap_high_12*data_mutual_m_new$SMB
    data$swap_high_12_hml <- data_mutual_m_new$swap_high_12*data_mutual_m_new$HML
    data$swap_high_12_wml <- data_mutual_m_new$swap_high_12*data_mutual_m_new$WML
    data$swap_high_9_mkt <- data_mutual_m_new$swap_high_9*data_mutual_m_new$MKT.RF
    data$swap_high_9_smb <- data_mutual_m_new$swap_high_9*data_mutual_m_new$SMB
    data$swap_high_9_hml <- data_mutual_m_new$swap_high_9*data_mutual_m_new$HML
    data$swap_high_9_wml <- data_mutual_m_new$swap_high_9*data_mutual_m_new$WML
    data$swap_low_24_mkt <- data_mutual_m_new$swap_low_24*data_mutual_m_new$MKT.RF
    data$swap_low_24_smb <- data_mutual_m_new$swap_low_24*data_mutual_m_new$SMB
    data$swap_low_24_hml <- data_mutual_m_new$swap_low_24*data_mutual_m_new$HML
    data$swap_low_24_wml <- data_mutual_m_new$swap_low_24*data_mutual_m_new$WML
    data$swap_low_12_mkt <- data_mutual_m_new$swap_low_12*data_mutual_m_new$MKT.RF
    data$swap_low_12_smb <- data_mutual_m_new$swap_low_12*data_mutual_m_new$SMB
    data$swap_low_12_hml <- data_mutual_m_new$swap_low_12*data_mutual_m_new$HML
    data$swap_low_12_wml <- data_mutual_m_new$swap_low_12*data_mutual_m_new$WML
    data$swap_low_9_mkt <- data_mutual_m_new$swap_low_9*data_mutual_m_new$MKT.RF
    data$swap_low_9_smb <- data_mutual_m_new$swap_low_9*data_mutual_m_new$SMB
    data$swap_low_9_hml <- data_mutual_m_new$swap_low_9*data_mutual_m_new$HML
    data$swap_low_9_wml <- data_mutual_m_new$swap_low_9*data_mutual_m_new$WML
    
    data$int_high_24_mkt <- data_mutual_m_new$int_high_24*data_mutual_m_new$MKT.RF
    data$int_high_24_smb <- data_mutual_m_new$int_high_24*data_mutual_m_new$SMB
    data$int_high_24_hml <- data_mutual_m_new$int_high_24*data_mutual_m_new$HML
    data$int_high_24_wml <- data_mutual_m_new$int_high_24*data_mutual_m_new$WML
    data$int_high_12_mkt <- data_mutual_m_new$int_high_12*data_mutual_m_new$MKT.RF
    data$int_high_12_smb <- data_mutual_m_new$int_high_12*data_mutual_m_new$SMB
    data$int_high_12_hml <- data_mutual_m_new$int_high_12*data_mutual_m_new$HML
    data$int_high_12_wml <- data_mutual_m_new$int_high_12*data_mutual_m_new$WML
    data$int_high_9_mkt <- data_mutual_m_new$int_high_9*data_mutual_m_new$MKT.RF
    data$int_high_9_smb <- data_mutual_m_new$int_high_9*data_mutual_m_new$SMB
    data$int_high_9_hml <- data_mutual_m_new$int_high_9*data_mutual_m_new$HML
    data$int_high_9_wml <- data_mutual_m_new$int_high_9*data_mutual_m_new$WML
    data$int_low_24_mkt <- data_mutual_m_new$int_low_24*data_mutual_m_new$MKT.RF
    data$int_low_24_smb <- data_mutual_m_new$int_low_24*data_mutual_m_new$SMB
    data$int_low_24_hml <- data_mutual_m_new$int_low_24*data_mutual_m_new$HML
    data$int_low_24_wml <- data_mutual_m_new$int_low_24*data_mutual_m_new$WML
    data$int_low_12_mkt <- data_mutual_m_new$int_low_12*data_mutual_m_new$MKT.RF
    data$int_low_12_smb <- data_mutual_m_new$int_low_12*data_mutual_m_new$SMB
    data$int_low_12_hml <- data_mutual_m_new$int_low_12*data_mutual_m_new$HML
    data$int_low_12_wml <- data_mutual_m_new$int_low_12*data_mutual_m_new$WML
    data$int_low_9_mkt <- data_mutual_m_new$int_low_9*data_mutual_m_new$MKT.RF
    data$int_low_9_smb <- data_mutual_m_new$int_low_9*data_mutual_m_new$SMB
    data$int_low_9_hml <- data_mutual_m_new$int_low_9*data_mutual_m_new$HML
    data$int_low_9_wml <- data_mutual_m_new$int_low_9*data_mutual_m_new$WML
    
    
    data$recession_mkt <- data_mutual_m_new$recession*data_mutual_m_new$MKT.RF
    data$recession_smb <- data_mutual_m_new$recession*data_mutual_m_new$SMB
    data$recession_hml <- data_mutual_m_new$recession*data_mutual_m_new$HML
    data$recession_wml <- data_mutual_m_new$recession*data_mutual_m_new$WML
    
    data$yield_high_24_mkt <- data_mutual_m_new$yield_high_24*data_mutual_m_new$MKT.RF
    data$yield_high_24_smb <- data_mutual_m_new$yield_high_24*data_mutual_m_new$SMB
    data$yield_high_24_hml <- data_mutual_m_new$yield_high_24*data_mutual_m_new$HML
    data$yield_high_24_wml <- data_mutual_m_new$yield_high_24*data_mutual_m_new$WML
    data$yield_high_12_mkt <- data_mutual_m_new$yield_high_12*data_mutual_m_new$MKT.RF
    data$yield_high_12_smb <- data_mutual_m_new$yield_high_12*data_mutual_m_new$SMB
    data$yield_high_12_hml <- data_mutual_m_new$yield_high_12*data_mutual_m_new$HML
    data$yield_high_12_wml <- data_mutual_m_new$yield_high_12*data_mutual_m_new$WML
    data$yield_high_9_mkt <- data_mutual_m_new$yield_high_9*data_mutual_m_new$MKT.RF
    data$yield_high_9_smb <- data_mutual_m_new$yield_high_9*data_mutual_m_new$SMB
    data$yield_high_9_hml <- data_mutual_m_new$yield_high_9*data_mutual_m_new$HML
    data$yield_high_9_wml <- data_mutual_m_new$yield_high_9*data_mutual_m_new$WML
    data$yield_low_24_mkt <- data_mutual_m_new$yield_low_24*data_mutual_m_new$MKT.RF
    data$yield_low_24_smb <- data_mutual_m_new$yield_low_24*data_mutual_m_new$SMB
    data$yield_low_24_hml <- data_mutual_m_new$yield_low_24*data_mutual_m_new$HML
    data$yield_low_24_wml <- data_mutual_m_new$yield_low_24*data_mutual_m_new$WML
    data$yield_low_12_mkt <- data_mutual_m_new$yield_low_12*data_mutual_m_new$MKT.RF
    data$yield_low_12_smb <- data_mutual_m_new$yield_low_12*data_mutual_m_new$SMB
    data$yield_low_12_hml <- data_mutual_m_new$yield_low_12*data_mutual_m_new$HML
    data$yield_low_12_wml <- data_mutual_m_new$yield_low_12*data_mutual_m_new$WML
    data$yield_low_9_mkt <- data_mutual_m_new$yield_low_9*data_mutual_m_new$MKT.RF
    data$yield_low_9_smb <- data_mutual_m_new$yield_low_9*data_mutual_m_new$SMB
    data$yield_low_9_hml <- data_mutual_m_new$yield_low_9*data_mutual_m_new$HML
    data$yield_low_9_wml <- data_mutual_m_new$yield_low_9*data_mutual_m_new$WML
    
    
    data <- data[,-c(1:2)]
    
    # Center y, X will be standardized in the modelling function
    y <- data %>% select(excess) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
    X <- data %>% select(-excess) %>% as.matrix()
    
    # Perform 10-fold cross-validation to select lambda
    lambdas_to_try <- 10^seq(-4, 4, length.out = 50)
    # Setting alpha = 0 implements ridge regression
    lasso_cv <- cv.glmnet(X, y, alpha = 1, lambda = lambdas_to_try,
                          standardize = TRUE, nfolds = 10)
    # Plot cross-validation results
    plot(lasso_cv)
    # Best cross-validated lambda
    lambda_cv <- lasso_cv$lambda.min
    lambda_cv
    # Fit final model
    model_cv <- glmnet(X, y, alpha = 1, lambda = lambda_cv, standardize = TRUE)
    coef(model_cv)
    
    #P3
    data_mutual_m_new <- subset(data_mutual_m, !is.na(data_mutual_m$swap_high_24))
    summary(data_mutual_m_new)
    
    data <- data.frame(1:nrow(data_mutual_m_new), NA)
    
    # Y variable
    data$excess <- data_mutual_m_new$p3
    
    # x no interactions
    
    data$Mkt.RF <- data_mutual_m_new$MKT.RF
    data$SMB <- data_mutual_m_new$SMB
    data$HML <- data_mutual_m_new$HML
    data$WML <- data_mutual_m_new$WML
    
    data$bear9 <- data_mutual_m_new$bear9
    data$bear12 <- data_mutual_m_new$bear12
    data$bear24 <- data_mutual_m_new$bear24
    
    data$var_high_24 <- data_mutual_m_new$var_high_24
    data$var_high_12 <- data_mutual_m_new$var_high_12
    data$var_high_9 <- data_mutual_m_new$var_high_9
    data$var_low_24 <- data_mutual_m_new$var_low_24
    data$var_low_12 <- data_mutual_m_new$var_low_12
    data$var_low_9 <- data_mutual_m_new$var_low_9
    
    data$swap_high_24 <- data_mutual_m_new$swap_high_24
    data$swap_high_12 <- data_mutual_m_new$swap_high_12
    data$swap_high_9 <- data_mutual_m_new$swap_high_9
    data$swap_low_24 <- data_mutual_m_new$swap_low_24
    data$swap_low_12 <- data_mutual_m_new$swap_low_12
    data$swap_low_9 <- data_mutual_m_new$swap_low_9
    
    data$int_high_24 <- data_mutual_m_new$int_high_24
    data$int_high_12 <- data_mutual_m_new$int_high_12
    data$int_high_9 <- data_mutual_m_new$int_high_9
    data$int_low_24 <- data_mutual_m_new$int_low_24
    data$int_low_12 <- data_mutual_m_new$int_low_12
    data$int_low_9 <- data_mutual_m_new$int_low_9
    
    
    data$recession <- data_mutual_m_new$recession
    
    data$yield_high_24 <- data_mutual_m_new$yield_high_24
    data$yield_high_12 <- data_mutual_m_new$yield_high_12
    data$yield_high_9 <- data_mutual_m_new$yield_high_9
    data$yield_low_24 <- data_mutual_m_new$yield_low_24
    data$yield_low_12 <- data_mutual_m_new$yield_low_12
    data$yield_low_9 <- data_mutual_m_new$yield_low_9
    
    # x  interactions
    
    data$bear24_mkt <- data_mutual_m_new$bear24*data_mutual_m_new$MKT.RF
    data$bear24_smb <- data_mutual_m_new$bear24*data_mutual_m_new$SMB
    data$bear24_hml <- data_mutual_m_new$bear24*data_mutual_m_new$HML
    data$bear24_wml <- data_mutual_m_new$bear24*data_mutual_m_new$WML
    data$bear12_mkt <- data_mutual_m_new$bear12*data_mutual_m_new$MKT.RF
    data$bear12_smb <- data_mutual_m_new$bear12*data_mutual_m_new$SMB
    data$bear12_hml <- data_mutual_m_new$bear12*data_mutual_m_new$HML
    data$bear12_wml <- data_mutual_m_new$bear12*data_mutual_m_new$WML
    data$bear9_mkt <- data_mutual_m_new$bear9*data_mutual_m_new$MKT.RF
    data$bear9_smb <- data_mutual_m_new$bear9*data_mutual_m_new$SMB
    data$bear9_hml <- data_mutual_m_new$bear9*data_mutual_m_new$HML
    data$bear9_wml <- data_mutual_m_new$bear9*data_mutual_m_new$WML
    
    
    data$var_high_24_mkt <- data_mutual_m_new$var_high_24*data_mutual_m_new$MKT.RF
    data$var_high_24_smb <- data_mutual_m_new$var_high_24*data_mutual_m_new$SMB
    data$var_high_24_hml <- data_mutual_m_new$var_high_24*data_mutual_m_new$HML
    data$var_high_24_wml <- data_mutual_m_new$var_high_24*data_mutual_m_new$WML
    data$var_high_12_mkt <- data_mutual_m_new$var_high_12*data_mutual_m_new$MKT.RF
    data$var_high_12_smb <- data_mutual_m_new$var_high_12*data_mutual_m_new$SMB
    data$var_high_12_hml <- data_mutual_m_new$var_high_12*data_mutual_m_new$HML
    data$var_high_12_wml <- data_mutual_m_new$var_high_12*data_mutual_m_new$WML
    data$var_high_9_mkt <- data_mutual_m_new$var_high_9*data_mutual_m_new$MKT.RF
    data$var_high_9_smb <- data_mutual_m_new$var_high_9*data_mutual_m_new$SMB
    data$var_high_9_hml <- data_mutual_m_new$var_high_9*data_mutual_m_new$HML
    data$var_high_9_wml <- data_mutual_m_new$var_high_9*data_mutual_m_new$WML
    data$var_low_24_mkt <- data_mutual_m_new$var_low_24*data_mutual_m_new$MKT.RF
    data$var_low_24_smb <- data_mutual_m_new$var_low_24*data_mutual_m_new$SMB
    data$var_low_24_hml <- data_mutual_m_new$var_low_24*data_mutual_m_new$HML
    data$var_low_24_wml <- data_mutual_m_new$var_low_24*data_mutual_m_new$WML
    data$var_low_12_mkt <- data_mutual_m_new$var_low_12*data_mutual_m_new$MKT.RF
    data$var_low_12_smb <- data_mutual_m_new$var_low_12*data_mutual_m_new$SMB
    data$var_low_12_hml <- data_mutual_m_new$var_low_12*data_mutual_m_new$HML
    data$var_low_12_wml <- data_mutual_m_new$var_low_12*data_mutual_m_new$WML
    data$var_low_9_mkt <- data_mutual_m_new$var_low_9*data_mutual_m_new$MKT.RF
    data$var_low_9_smb <- data_mutual_m_new$var_low_9*data_mutual_m_new$SMB
    data$var_low_9_hml <- data_mutual_m_new$var_low_9*data_mutual_m_new$HML
    data$var_low_9_wml <- data_mutual_m_new$var_low_9*data_mutual_m_new$WML
    
    data$swap_high_24_mkt <- data_mutual_m_new$swap_high_24*data_mutual_m_new$MKT.RF
    data$swap_high_24_smb <- data_mutual_m_new$swap_high_24*data_mutual_m_new$SMB
    data$swap_high_24_hml <- data_mutual_m_new$swap_high_24*data_mutual_m_new$HML
    data$swap_high_24_wml <- data_mutual_m_new$swap_high_24*data_mutual_m_new$WML
    data$swap_high_12_mkt <- data_mutual_m_new$swap_high_12*data_mutual_m_new$MKT.RF
    data$swap_high_12_smb <- data_mutual_m_new$swap_high_12*data_mutual_m_new$SMB
    data$swap_high_12_hml <- data_mutual_m_new$swap_high_12*data_mutual_m_new$HML
    data$swap_high_12_wml <- data_mutual_m_new$swap_high_12*data_mutual_m_new$WML
    data$swap_high_9_mkt <- data_mutual_m_new$swap_high_9*data_mutual_m_new$MKT.RF
    data$swap_high_9_smb <- data_mutual_m_new$swap_high_9*data_mutual_m_new$SMB
    data$swap_high_9_hml <- data_mutual_m_new$swap_high_9*data_mutual_m_new$HML
    data$swap_high_9_wml <- data_mutual_m_new$swap_high_9*data_mutual_m_new$WML
    data$swap_low_24_mkt <- data_mutual_m_new$swap_low_24*data_mutual_m_new$MKT.RF
    data$swap_low_24_smb <- data_mutual_m_new$swap_low_24*data_mutual_m_new$SMB
    data$swap_low_24_hml <- data_mutual_m_new$swap_low_24*data_mutual_m_new$HML
    data$swap_low_24_wml <- data_mutual_m_new$swap_low_24*data_mutual_m_new$WML
    data$swap_low_12_mkt <- data_mutual_m_new$swap_low_12*data_mutual_m_new$MKT.RF
    data$swap_low_12_smb <- data_mutual_m_new$swap_low_12*data_mutual_m_new$SMB
    data$swap_low_12_hml <- data_mutual_m_new$swap_low_12*data_mutual_m_new$HML
    data$swap_low_12_wml <- data_mutual_m_new$swap_low_12*data_mutual_m_new$WML
    data$swap_low_9_mkt <- data_mutual_m_new$swap_low_9*data_mutual_m_new$MKT.RF
    data$swap_low_9_smb <- data_mutual_m_new$swap_low_9*data_mutual_m_new$SMB
    data$swap_low_9_hml <- data_mutual_m_new$swap_low_9*data_mutual_m_new$HML
    data$swap_low_9_wml <- data_mutual_m_new$swap_low_9*data_mutual_m_new$WML
    
    data$int_high_24_mkt <- data_mutual_m_new$int_high_24*data_mutual_m_new$MKT.RF
    data$int_high_24_smb <- data_mutual_m_new$int_high_24*data_mutual_m_new$SMB
    data$int_high_24_hml <- data_mutual_m_new$int_high_24*data_mutual_m_new$HML
    data$int_high_24_wml <- data_mutual_m_new$int_high_24*data_mutual_m_new$WML
    data$int_high_12_mkt <- data_mutual_m_new$int_high_12*data_mutual_m_new$MKT.RF
    data$int_high_12_smb <- data_mutual_m_new$int_high_12*data_mutual_m_new$SMB
    data$int_high_12_hml <- data_mutual_m_new$int_high_12*data_mutual_m_new$HML
    data$int_high_12_wml <- data_mutual_m_new$int_high_12*data_mutual_m_new$WML
    data$int_high_9_mkt <- data_mutual_m_new$int_high_9*data_mutual_m_new$MKT.RF
    data$int_high_9_smb <- data_mutual_m_new$int_high_9*data_mutual_m_new$SMB
    data$int_high_9_hml <- data_mutual_m_new$int_high_9*data_mutual_m_new$HML
    data$int_high_9_wml <- data_mutual_m_new$int_high_9*data_mutual_m_new$WML
    data$int_low_24_mkt <- data_mutual_m_new$int_low_24*data_mutual_m_new$MKT.RF
    data$int_low_24_smb <- data_mutual_m_new$int_low_24*data_mutual_m_new$SMB
    data$int_low_24_hml <- data_mutual_m_new$int_low_24*data_mutual_m_new$HML
    data$int_low_24_wml <- data_mutual_m_new$int_low_24*data_mutual_m_new$WML
    data$int_low_12_mkt <- data_mutual_m_new$int_low_12*data_mutual_m_new$MKT.RF
    data$int_low_12_smb <- data_mutual_m_new$int_low_12*data_mutual_m_new$SMB
    data$int_low_12_hml <- data_mutual_m_new$int_low_12*data_mutual_m_new$HML
    data$int_low_12_wml <- data_mutual_m_new$int_low_12*data_mutual_m_new$WML
    data$int_low_9_mkt <- data_mutual_m_new$int_low_9*data_mutual_m_new$MKT.RF
    data$int_low_9_smb <- data_mutual_m_new$int_low_9*data_mutual_m_new$SMB
    data$int_low_9_hml <- data_mutual_m_new$int_low_9*data_mutual_m_new$HML
    data$int_low_9_wml <- data_mutual_m_new$int_low_9*data_mutual_m_new$WML
    
    
    data$recession_mkt <- data_mutual_m_new$recession*data_mutual_m_new$MKT.RF
    data$recession_smb <- data_mutual_m_new$recession*data_mutual_m_new$SMB
    data$recession_hml <- data_mutual_m_new$recession*data_mutual_m_new$HML
    data$recession_wml <- data_mutual_m_new$recession*data_mutual_m_new$WML
    
    data$yield_high_24_mkt <- data_mutual_m_new$yield_high_24*data_mutual_m_new$MKT.RF
    data$yield_high_24_smb <- data_mutual_m_new$yield_high_24*data_mutual_m_new$SMB
    data$yield_high_24_hml <- data_mutual_m_new$yield_high_24*data_mutual_m_new$HML
    data$yield_high_24_wml <- data_mutual_m_new$yield_high_24*data_mutual_m_new$WML
    data$yield_high_12_mkt <- data_mutual_m_new$yield_high_12*data_mutual_m_new$MKT.RF
    data$yield_high_12_smb <- data_mutual_m_new$yield_high_12*data_mutual_m_new$SMB
    data$yield_high_12_hml <- data_mutual_m_new$yield_high_12*data_mutual_m_new$HML
    data$yield_high_12_wml <- data_mutual_m_new$yield_high_12*data_mutual_m_new$WML
    data$yield_high_9_mkt <- data_mutual_m_new$yield_high_9*data_mutual_m_new$MKT.RF
    data$yield_high_9_smb <- data_mutual_m_new$yield_high_9*data_mutual_m_new$SMB
    data$yield_high_9_hml <- data_mutual_m_new$yield_high_9*data_mutual_m_new$HML
    data$yield_high_9_wml <- data_mutual_m_new$yield_high_9*data_mutual_m_new$WML
    data$yield_low_24_mkt <- data_mutual_m_new$yield_low_24*data_mutual_m_new$MKT.RF
    data$yield_low_24_smb <- data_mutual_m_new$yield_low_24*data_mutual_m_new$SMB
    data$yield_low_24_hml <- data_mutual_m_new$yield_low_24*data_mutual_m_new$HML
    data$yield_low_24_wml <- data_mutual_m_new$yield_low_24*data_mutual_m_new$WML
    data$yield_low_12_mkt <- data_mutual_m_new$yield_low_12*data_mutual_m_new$MKT.RF
    data$yield_low_12_smb <- data_mutual_m_new$yield_low_12*data_mutual_m_new$SMB
    data$yield_low_12_hml <- data_mutual_m_new$yield_low_12*data_mutual_m_new$HML
    data$yield_low_12_wml <- data_mutual_m_new$yield_low_12*data_mutual_m_new$WML
    data$yield_low_9_mkt <- data_mutual_m_new$yield_low_9*data_mutual_m_new$MKT.RF
    data$yield_low_9_smb <- data_mutual_m_new$yield_low_9*data_mutual_m_new$SMB
    data$yield_low_9_hml <- data_mutual_m_new$yield_low_9*data_mutual_m_new$HML
    data$yield_low_9_wml <- data_mutual_m_new$yield_low_9*data_mutual_m_new$WML
    
    
    data <- data[,-c(1:2)]
    
    # Center y, X will be standardized in the modelling function
    y <- data %>% select(excess) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
    X <- data %>% select(-excess) %>% as.matrix()
    
    # Perform 10-fold cross-validation to select lambda
    lambdas_to_try <- 10^seq(-4, 4, length.out = 50)
    # Setting alpha = 0 implements ridge regression
    lasso_cv <- cv.glmnet(X, y, alpha = 1, lambda = lambdas_to_try,
                          standardize = TRUE, nfolds = 10)
    # Plot cross-validation results
    plot(lasso_cv)
    # Best cross-validated lambda
    lambda_cv <- lasso_cv$lambda.min
    lambda_cv
    # Fit final model
    model_cv <- glmnet(X, y, alpha = 1, lambda = lambda_cv, standardize = TRUE)
    coef(model_cv)
    
    
    #P9
    data_mutual_m_new <- subset(data_mutual_m, !is.na(data_mutual_m$swap_high_24))
    summary(data_mutual_m_new)
    
    data <- data.frame(1:nrow(data_mutual_m_new), NA)
    
    # Y variable
    data$excess <- data_mutual_m_new$p9
    
    # x no interactions
    
    data$Mkt.RF <- data_mutual_m_new$MKT.RF
    data$SMB <- data_mutual_m_new$SMB
    data$HML <- data_mutual_m_new$HML
    data$WML <- data_mutual_m_new$WML
    
    data$bear9 <- data_mutual_m_new$bear9
    data$bear12 <- data_mutual_m_new$bear12
    data$bear24 <- data_mutual_m_new$bear24
    
    data$var_high_24 <- data_mutual_m_new$var_high_24
    data$var_high_12 <- data_mutual_m_new$var_high_12
    data$var_high_9 <- data_mutual_m_new$var_high_9
    data$var_low_24 <- data_mutual_m_new$var_low_24
    data$var_low_12 <- data_mutual_m_new$var_low_12
    data$var_low_9 <- data_mutual_m_new$var_low_9
    
    data$swap_high_24 <- data_mutual_m_new$swap_high_24
    data$swap_high_12 <- data_mutual_m_new$swap_high_12
    data$swap_high_9 <- data_mutual_m_new$swap_high_9
    data$swap_low_24 <- data_mutual_m_new$swap_low_24
    data$swap_low_12 <- data_mutual_m_new$swap_low_12
    data$swap_low_9 <- data_mutual_m_new$swap_low_9
    
    data$int_high_24 <- data_mutual_m_new$int_high_24
    data$int_high_12 <- data_mutual_m_new$int_high_12
    data$int_high_9 <- data_mutual_m_new$int_high_9
    data$int_low_24 <- data_mutual_m_new$int_low_24
    data$int_low_12 <- data_mutual_m_new$int_low_12
    data$int_low_9 <- data_mutual_m_new$int_low_9
    
    
    data$recession <- data_mutual_m_new$recession
    
    data$yield_high_24 <- data_mutual_m_new$yield_high_24
    data$yield_high_12 <- data_mutual_m_new$yield_high_12
    data$yield_high_9 <- data_mutual_m_new$yield_high_9
    data$yield_low_24 <- data_mutual_m_new$yield_low_24
    data$yield_low_12 <- data_mutual_m_new$yield_low_12
    data$yield_low_9 <- data_mutual_m_new$yield_low_9
    
    # x  interactions
    
    data$bear24_mkt <- data_mutual_m_new$bear24*data_mutual_m_new$MKT.RF
    data$bear24_smb <- data_mutual_m_new$bear24*data_mutual_m_new$SMB
    data$bear24_hml <- data_mutual_m_new$bear24*data_mutual_m_new$HML
    data$bear24_wml <- data_mutual_m_new$bear24*data_mutual_m_new$WML
    data$bear12_mkt <- data_mutual_m_new$bear12*data_mutual_m_new$MKT.RF
    data$bear12_smb <- data_mutual_m_new$bear12*data_mutual_m_new$SMB
    data$bear12_hml <- data_mutual_m_new$bear12*data_mutual_m_new$HML
    data$bear12_wml <- data_mutual_m_new$bear12*data_mutual_m_new$WML
    data$bear9_mkt <- data_mutual_m_new$bear9*data_mutual_m_new$MKT.RF
    data$bear9_smb <- data_mutual_m_new$bear9*data_mutual_m_new$SMB
    data$bear9_hml <- data_mutual_m_new$bear9*data_mutual_m_new$HML
    data$bear9_wml <- data_mutual_m_new$bear9*data_mutual_m_new$WML
    
    
    data$var_high_24_mkt <- data_mutual_m_new$var_high_24*data_mutual_m_new$MKT.RF
    data$var_high_24_smb <- data_mutual_m_new$var_high_24*data_mutual_m_new$SMB
    data$var_high_24_hml <- data_mutual_m_new$var_high_24*data_mutual_m_new$HML
    data$var_high_24_wml <- data_mutual_m_new$var_high_24*data_mutual_m_new$WML
    data$var_high_12_mkt <- data_mutual_m_new$var_high_12*data_mutual_m_new$MKT.RF
    data$var_high_12_smb <- data_mutual_m_new$var_high_12*data_mutual_m_new$SMB
    data$var_high_12_hml <- data_mutual_m_new$var_high_12*data_mutual_m_new$HML
    data$var_high_12_wml <- data_mutual_m_new$var_high_12*data_mutual_m_new$WML
    data$var_high_9_mkt <- data_mutual_m_new$var_high_9*data_mutual_m_new$MKT.RF
    data$var_high_9_smb <- data_mutual_m_new$var_high_9*data_mutual_m_new$SMB
    data$var_high_9_hml <- data_mutual_m_new$var_high_9*data_mutual_m_new$HML
    data$var_high_9_wml <- data_mutual_m_new$var_high_9*data_mutual_m_new$WML
    data$var_low_24_mkt <- data_mutual_m_new$var_low_24*data_mutual_m_new$MKT.RF
    data$var_low_24_smb <- data_mutual_m_new$var_low_24*data_mutual_m_new$SMB
    data$var_low_24_hml <- data_mutual_m_new$var_low_24*data_mutual_m_new$HML
    data$var_low_24_wml <- data_mutual_m_new$var_low_24*data_mutual_m_new$WML
    data$var_low_12_mkt <- data_mutual_m_new$var_low_12*data_mutual_m_new$MKT.RF
    data$var_low_12_smb <- data_mutual_m_new$var_low_12*data_mutual_m_new$SMB
    data$var_low_12_hml <- data_mutual_m_new$var_low_12*data_mutual_m_new$HML
    data$var_low_12_wml <- data_mutual_m_new$var_low_12*data_mutual_m_new$WML
    data$var_low_9_mkt <- data_mutual_m_new$var_low_9*data_mutual_m_new$MKT.RF
    data$var_low_9_smb <- data_mutual_m_new$var_low_9*data_mutual_m_new$SMB
    data$var_low_9_hml <- data_mutual_m_new$var_low_9*data_mutual_m_new$HML
    data$var_low_9_wml <- data_mutual_m_new$var_low_9*data_mutual_m_new$WML
    
    data$swap_high_24_mkt <- data_mutual_m_new$swap_high_24*data_mutual_m_new$MKT.RF
    data$swap_high_24_smb <- data_mutual_m_new$swap_high_24*data_mutual_m_new$SMB
    data$swap_high_24_hml <- data_mutual_m_new$swap_high_24*data_mutual_m_new$HML
    data$swap_high_24_wml <- data_mutual_m_new$swap_high_24*data_mutual_m_new$WML
    data$swap_high_12_mkt <- data_mutual_m_new$swap_high_12*data_mutual_m_new$MKT.RF
    data$swap_high_12_smb <- data_mutual_m_new$swap_high_12*data_mutual_m_new$SMB
    data$swap_high_12_hml <- data_mutual_m_new$swap_high_12*data_mutual_m_new$HML
    data$swap_high_12_wml <- data_mutual_m_new$swap_high_12*data_mutual_m_new$WML
    data$swap_high_9_mkt <- data_mutual_m_new$swap_high_9*data_mutual_m_new$MKT.RF
    data$swap_high_9_smb <- data_mutual_m_new$swap_high_9*data_mutual_m_new$SMB
    data$swap_high_9_hml <- data_mutual_m_new$swap_high_9*data_mutual_m_new$HML
    data$swap_high_9_wml <- data_mutual_m_new$swap_high_9*data_mutual_m_new$WML
    data$swap_low_24_mkt <- data_mutual_m_new$swap_low_24*data_mutual_m_new$MKT.RF
    data$swap_low_24_smb <- data_mutual_m_new$swap_low_24*data_mutual_m_new$SMB
    data$swap_low_24_hml <- data_mutual_m_new$swap_low_24*data_mutual_m_new$HML
    data$swap_low_24_wml <- data_mutual_m_new$swap_low_24*data_mutual_m_new$WML
    data$swap_low_12_mkt <- data_mutual_m_new$swap_low_12*data_mutual_m_new$MKT.RF
    data$swap_low_12_smb <- data_mutual_m_new$swap_low_12*data_mutual_m_new$SMB
    data$swap_low_12_hml <- data_mutual_m_new$swap_low_12*data_mutual_m_new$HML
    data$swap_low_12_wml <- data_mutual_m_new$swap_low_12*data_mutual_m_new$WML
    data$swap_low_9_mkt <- data_mutual_m_new$swap_low_9*data_mutual_m_new$MKT.RF
    data$swap_low_9_smb <- data_mutual_m_new$swap_low_9*data_mutual_m_new$SMB
    data$swap_low_9_hml <- data_mutual_m_new$swap_low_9*data_mutual_m_new$HML
    data$swap_low_9_wml <- data_mutual_m_new$swap_low_9*data_mutual_m_new$WML
    
    data$int_high_24_mkt <- data_mutual_m_new$int_high_24*data_mutual_m_new$MKT.RF
    data$int_high_24_smb <- data_mutual_m_new$int_high_24*data_mutual_m_new$SMB
    data$int_high_24_hml <- data_mutual_m_new$int_high_24*data_mutual_m_new$HML
    data$int_high_24_wml <- data_mutual_m_new$int_high_24*data_mutual_m_new$WML
    data$int_high_12_mkt <- data_mutual_m_new$int_high_12*data_mutual_m_new$MKT.RF
    data$int_high_12_smb <- data_mutual_m_new$int_high_12*data_mutual_m_new$SMB
    data$int_high_12_hml <- data_mutual_m_new$int_high_12*data_mutual_m_new$HML
    data$int_high_12_wml <- data_mutual_m_new$int_high_12*data_mutual_m_new$WML
    data$int_high_9_mkt <- data_mutual_m_new$int_high_9*data_mutual_m_new$MKT.RF
    data$int_high_9_smb <- data_mutual_m_new$int_high_9*data_mutual_m_new$SMB
    data$int_high_9_hml <- data_mutual_m_new$int_high_9*data_mutual_m_new$HML
    data$int_high_9_wml <- data_mutual_m_new$int_high_9*data_mutual_m_new$WML
    data$int_low_24_mkt <- data_mutual_m_new$int_low_24*data_mutual_m_new$MKT.RF
    data$int_low_24_smb <- data_mutual_m_new$int_low_24*data_mutual_m_new$SMB
    data$int_low_24_hml <- data_mutual_m_new$int_low_24*data_mutual_m_new$HML
    data$int_low_24_wml <- data_mutual_m_new$int_low_24*data_mutual_m_new$WML
    data$int_low_12_mkt <- data_mutual_m_new$int_low_12*data_mutual_m_new$MKT.RF
    data$int_low_12_smb <- data_mutual_m_new$int_low_12*data_mutual_m_new$SMB
    data$int_low_12_hml <- data_mutual_m_new$int_low_12*data_mutual_m_new$HML
    data$int_low_12_wml <- data_mutual_m_new$int_low_12*data_mutual_m_new$WML
    data$int_low_9_mkt <- data_mutual_m_new$int_low_9*data_mutual_m_new$MKT.RF
    data$int_low_9_smb <- data_mutual_m_new$int_low_9*data_mutual_m_new$SMB
    data$int_low_9_hml <- data_mutual_m_new$int_low_9*data_mutual_m_new$HML
    data$int_low_9_wml <- data_mutual_m_new$int_low_9*data_mutual_m_new$WML
    
    
    data$recession_mkt <- data_mutual_m_new$recession*data_mutual_m_new$MKT.RF
    data$recession_smb <- data_mutual_m_new$recession*data_mutual_m_new$SMB
    data$recession_hml <- data_mutual_m_new$recession*data_mutual_m_new$HML
    data$recession_wml <- data_mutual_m_new$recession*data_mutual_m_new$WML
    
    data$yield_high_24_mkt <- data_mutual_m_new$yield_high_24*data_mutual_m_new$MKT.RF
    data$yield_high_24_smb <- data_mutual_m_new$yield_high_24*data_mutual_m_new$SMB
    data$yield_high_24_hml <- data_mutual_m_new$yield_high_24*data_mutual_m_new$HML
    data$yield_high_24_wml <- data_mutual_m_new$yield_high_24*data_mutual_m_new$WML
    data$yield_high_12_mkt <- data_mutual_m_new$yield_high_12*data_mutual_m_new$MKT.RF
    data$yield_high_12_smb <- data_mutual_m_new$yield_high_12*data_mutual_m_new$SMB
    data$yield_high_12_hml <- data_mutual_m_new$yield_high_12*data_mutual_m_new$HML
    data$yield_high_12_wml <- data_mutual_m_new$yield_high_12*data_mutual_m_new$WML
    data$yield_high_9_mkt <- data_mutual_m_new$yield_high_9*data_mutual_m_new$MKT.RF
    data$yield_high_9_smb <- data_mutual_m_new$yield_high_9*data_mutual_m_new$SMB
    data$yield_high_9_hml <- data_mutual_m_new$yield_high_9*data_mutual_m_new$HML
    data$yield_high_9_wml <- data_mutual_m_new$yield_high_9*data_mutual_m_new$WML
    data$yield_low_24_mkt <- data_mutual_m_new$yield_low_24*data_mutual_m_new$MKT.RF
    data$yield_low_24_smb <- data_mutual_m_new$yield_low_24*data_mutual_m_new$SMB
    data$yield_low_24_hml <- data_mutual_m_new$yield_low_24*data_mutual_m_new$HML
    data$yield_low_24_wml <- data_mutual_m_new$yield_low_24*data_mutual_m_new$WML
    data$yield_low_12_mkt <- data_mutual_m_new$yield_low_12*data_mutual_m_new$MKT.RF
    data$yield_low_12_smb <- data_mutual_m_new$yield_low_12*data_mutual_m_new$SMB
    data$yield_low_12_hml <- data_mutual_m_new$yield_low_12*data_mutual_m_new$HML
    data$yield_low_12_wml <- data_mutual_m_new$yield_low_12*data_mutual_m_new$WML
    data$yield_low_9_mkt <- data_mutual_m_new$yield_low_9*data_mutual_m_new$MKT.RF
    data$yield_low_9_smb <- data_mutual_m_new$yield_low_9*data_mutual_m_new$SMB
    data$yield_low_9_hml <- data_mutual_m_new$yield_low_9*data_mutual_m_new$HML
    data$yield_low_9_wml <- data_mutual_m_new$yield_low_9*data_mutual_m_new$WML
    
    
    data <- data[,-c(1:2)]
    
    # Center y, X will be standardized in the modelling function
    y <- data %>% select(excess) %>% scale(center = TRUE, scale = FALSE) %>% as.matrix()
    X <- data %>% select(-excess) %>% as.matrix()
    
    # Perform 10-fold cross-validation to select lambda
    lambdas_to_try <- 10^seq(-4, 4, length.out = 50)
    # Setting alpha = 0 implements ridge regression
    lasso_cv <- cv.glmnet(X, y, alpha = 1, lambda = lambdas_to_try,
                          standardize = TRUE, nfolds = 10)
    # Plot cross-validation results
    plot(lasso_cv)
    # Best cross-validated lambda
    lambda_cv <- lasso_cv$lambda.min
    lambda_cv
    # Fit final model
    model_cv <- glmnet(X, y, alpha = 1, lambda = lambda_cv, standardize = TRUE)
    coef(model_cv)
    
    objects <- ls()
    objects_to_remove <- objects[!objects %in% c("data_etf", "data_mutual", "data_etf_m", "data_mutual_m")]
    rm(list = objects_to_remove)
    
    
    
    
                              #################################
                              ###########Figures###############
                              #################################
    
    #######################
    #######Figure 2########
    #######################
    #ETFS
    # Create a new data frame with the required data
    data_etf_m$EW_ETF <- data_etf_m$p26
    plot_data <- data.frame(
      Date = data_etf_m$date,
      EW_ETF = data_etf_m$EW_ETF,
      MKT.RF = data_etf_m$MKT.RF
    )
    
    # Reshape the data to long format
    plot_data_long <- reshape2::melt(plot_data, id.vars = "Date")
    test <- read.xlsx('test.xlsx', startRow = 1,detectDates = TRUE)
    plot_data_long$Date <- test$Date
    class(plot_data_long$Date)
    # Create the bar plot with side by side bars
    ggplot(plot_data_long, aes(x = Date, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "dodge", width = 25) +
      #geom_text(aes(label = sprintf("%.2f", value),y = value + 0.5), position = position_dodge(width = 0.8), vjust = 0.2, size = 3, angle = 90) +
      labs(x = "Date", y = "Monthly returns %") +
      ggtitle("Returns of the equally weighted ETF-Portfolio") +
      scale_x_date(date_labels = "%Y")+
      #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      #scale_x_date(date_breaks = "years")+
      theme(legend.position = c(0.82, 0.3), legend.justification = c(0, 1), legend.title = element_blank())+
      scale_color_manual(labels = c("EW_ETF", "Rm-Rf"), values = c("blue", "red")) 
    
    
    #MFs
    # Create a new data frame with the required data
    data_mutual_m$EW_MF <- data_mutual_m$p14
    plot_data <- data.frame(
      Date = data_mutual_m$date,
      EW_MF = data_mutual_m$EW_MF,
      MKT.RF = data_mutual_m$MKT.RF
    )
    
    # Reshape the data to long format
    plot_data_long <- reshape2::melt(plot_data, id.vars = "Date")
    test <- read.xlsx('test2.xlsx', startRow = 1,detectDates = TRUE)
    plot_data_long$Date <- test$date
    class(plot_data_long$Date)
    # Create the bar plot with side by side bars
    ggplot(plot_data_long, aes(x = Date, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "dodge", width = 25) +
      #geom_text(aes(label = sprintf("%.2f", value),y = value + 0.5), position = position_dodge(width = 0.8), vjust = 0.2, size = 3, angle = 90) +
      labs(x = "Date", y = "Monthly returns %") +
      ggtitle("Returns of the equally weighted MF-Portfolio") +
      scale_x_date(date_labels = "%Y")+
      #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      #scale_x_date(date_breaks = "years")+
      theme(legend.position = c(0.82, 0.3), legend.justification = c(0, 1), legend.title = element_blank())+
      scale_color_manual(labels = c("EW_MF", "Rm-Rf"), values = c("blue", "red")) 
    
    #######################
    #######Figure 3########
    #######################
    #dollar value
    #Dollar value of investment: ETF
    #average past winner PF
    data_etf$dollar_p15 <- 1
    for (i in 2:nrow(data_etf)) {
      data_etf$dollar_p15[i] <- data_etf$dollar_p15[i-1] * (1 + data_etf$p15[i-1] / 100)
    }
    
    #average past loser PF
    data_etf$dollar_p22 <- 1
    for (i in 2:nrow(data_etf)) {
      data_etf$dollar_p22[i] <- data_etf$dollar_p22[i-1] * (1 + data_etf$p22[i-1] / 100)
    }
    
    #Average PF
    data_etf$dollar_p26 <- 1
    for (i in 2:nrow(data_etf)) {
      data_etf$dollar_p26[i] <- data_etf$dollar_p26[i-1] * (1 + data_etf$p26[i-1] / 100)
    }
    
    #Market
    data_etf$dollar_mkt <- 1
    for (i in 2:nrow(data_etf)) {
      data_etf$dollar_mkt[i] <- data_etf$dollar_mkt[i-1] * (1 + (data_etf$MKT[i-1]) / 100)
    }
    #Riskfree
    data_etf$dollar_rf <- 1
    for (i in 2:nrow(data_etf)) {
      data_etf$dollar_rf[i] <- data_etf$dollar_rf[i-1] * (1 + data_etf$RF[i-1] / 100)
    }
    data_etf$qEWMP <- data_mutual$p14[4363:6186]
    data_etf$dollar_EWM <- 1
    for (i in 2:nrow(data_etf)) {
      data_etf$dollar_EWM[i] <- data_etf$dollar_EWM[i-1] * (1 + data_etf$qEWMP[i-1] / 100)
    }
    
    data_etf$date <- ymd(data_etf$date)
    ggplot(data_etf, aes(x = date)) +
      geom_line(aes(y = dollar_p15, color = "Dollar P15")) +
      geom_line(aes(y = dollar_p22, color = "Dollar P22")) +
      geom_line(aes(y = dollar_p26, color = "Dollar P26")) +
      geom_line(aes(y = dollar_mkt, color = "Dollar Mkt")) +
      geom_line(aes(y = dollar_rf, color = "Dollar RF")) +
      labs(title = "Development of ETF Investments Over Time",
           x = "Date",
           y = "Value") +
      scale_color_manual(values = c("Dollar P15" = "blue", "Dollar P26" = "red", "Dollar Mkt" = "green", "Dollar RF" = "purple","Dollar P22"="grey"),
                         labels = c("Dollar Mkt", "Dollar Winner","Dollar Loser", "Dollar EW_ETF", "Dollar RF")) +
      theme(legend.position = c(0.02, 0.98), legend.justification = c(0, 1), legend.title = element_blank())+
      scale_y_log10(limits = c(-1, 3),
                    breaks = c(-1, 0, 1, 2, 3),
                    labels = c("-1","0", "1", "2", "3"))
    
    #MFs
    #dollar value
    #Dollar value of investment: Mutual/hedge
    #average winners
    data_mutual$dollar_p1 <- 1
    for (i in 2:nrow(data_mutual)) {
      data_mutual$dollar_p1[i] <- data_mutual$dollar_p1[i-1] * (1 + data_mutual$p1[i-1] / 100)
    }
    #average losers
    data_mutual$dollar_p11 <- 1
    for (i in 2:nrow(data_mutual)) {
      data_mutual$dollar_p11[i] <- data_mutual$dollar_p11[i-1] * (1 + data_mutual$p11[i-1] / 100)
    }
    
    #average pf
    data_mutual$dollar_p14 <- 1
    for (i in 2:nrow(data_mutual)) {
      data_mutual$dollar_p14[i] <- data_mutual$dollar_p14[i-1] * (1 + data_mutual$p14[i-1] / 100)
    }
    
    #Market
    data_mutual$dollar_mkt <- 1
    for (i in 2:nrow(data_mutual)) {
      data_mutual$dollar_mkt[i] <- data_mutual$dollar_mkt[i-1] * (1 + (data_mutual$MKT[i-1]) / 100)
    }
    #Riskfree
    data_mutual$dollar_rf <- 1
    for (i in 2:nrow(data_mutual)) {
      data_mutual$dollar_rf[i] <- data_mutual$dollar_rf[i-1] * (1 + data_mutual$RF[i-1] / 100)
    }
    
    data_mutual$date <- ymd(data_mutual$date)
    ggplot(data_mutual, aes(x = date)) +
      geom_line(aes(y = dollar_p1, color = "Dollar P1")) +
      geom_line(aes(y = dollar_p11, color = "Dollar P11")) +
      geom_line(aes(y = dollar_p14, color = "Dollar P14")) +
      geom_line(aes(y = dollar_mkt, color = "Dollar Mkt")) +
      geom_line(aes(y = dollar_rf, color = "Dollar RF")) +
      labs(title = "Development of MF Investment Over Time",
           x = "Date",
           y = "Value") +
      scale_color_manual(values = c("Dollar P1" = "blue", "Dollar P14" = "red", "Dollar Mkt" = "green", "Dollar RF" = "purple","Dollar P11" = "pink"),
                         labels = c("Dollar Mkt", "Dollar Winner", "Dollar Loser", "Dollar EW_MF", "Dollar RF")) +
      theme(legend.position = c(0.02, 0.98), legend.justification = c(0, 1), legend.title = element_blank())+
      scale_y_log10(limits = c(-1, 11),
                    breaks = c(-1, 2, 5, 8, 11),
                    labels = c("-1","2", "5", "8", "11"))
    
    
    #ETF & MF
    
    ggplot(data_etf, aes(x = date)) +
      geom_line(aes(y = dollar_EWM, color = "dollar_EWM")) +
      geom_line(aes(y = dollar_p26, color = "Dollar P26")) +
      geom_line(aes(y = dollar_mkt, color = "Dollar Mkt")) +
      geom_line(aes(y = dollar_rf, color = "Dollar RF")) +
      labs(title = "Comparison of Equally-Weighted PF Investments Over Time",
           x = "Date",
           y = "Value") +
      scale_color_manual(values = c("dollar_EWM" = "blue", "Dollar P26" = "red", "Dollar Mkt" = "green", "Dollar RF" = "purple"),
                         labels = c("Dollar Mkt","Dollar EW_MF", "Dollar EW_ETF", "Dollar RF")) +
      theme(legend.position = c(0.02, 0.98), legend.justification = c(0, 1), legend.title = element_blank())+
      scale_y_log10(limits = c(-1, 3),
                    breaks = c(-1, 0, 1, 2, 3),
                    labels = c("-1","0", "1", "2", "3"))
    
    
    #######################
    #######Figure 4########
    #######################
   
    #ETFs
     period1 <- subset(data_etf, date >= as.Date("2016-01-04") & date <= as.Date("2023-03-31"))
    #Graph 1 
    
    window_size <- 126
    
    coeff_list_p26 <- list()
    coeff_list_p15 <- list()
    coeff_list_p22 <- list()
    
    for (i in window_size:length(period1$date)) {
      
      start_index <- i - window_size + 1
      end_index <- i 
      
      rolling_mom_d <- period1[start_index:end_index,]
      
      model_p26 <- lm(p26 ~ MKT.RF, data = rolling_mom_d)
      model_p15 <- lm(p15 ~ MKT.RF, data = rolling_mom_d)
      model_p22 <- lm(p22 ~ MKT.RF, data = rolling_mom_d)
      
      coeff_list_p26[[i]] <- coef(model_p26)[2]
      coeff_list_p15[[i]] <- coef(model_p15)[2]
      coeff_list_p22[[i]] <- coef(model_p22)[2]
      
    }
    
    valid_indices_p26 <- which(!is.na(coeff_list_p26))
    valid_indices_p22 <- which(!is.na(coeff_list_p22))
    valid_indices_p15 <- which(!is.na(coeff_list_p15))
    
    coeff_vec_p26 <- unlist(coeff_list_p26)
    coeff_vec_p22 <- unlist(coeff_list_p22)
    coeff_vec_p15 <- unlist(coeff_list_p15)
    
    plot(period1$date[(window_size + valid_indices_p26 - 1)], coeff_vec_p26[valid_indices_p26], 
         type = 'l', xlab = "Date", ylab = "Coefficient value for Mkt.RF", main = "Time variation in market beta for ETFs",
         col = "blue", ylim = range(c(coeff_vec_p26, coeff_vec_p22,coeff_vec_p15)))
    
    lines(period1$date[(window_size + valid_indices_p22 - 1)], coeff_vec_p22[valid_indices_p22], 
          type = 'l', col = "red")
    lines(period1$date[(window_size + valid_indices_p15 - 1)], coeff_vec_p15[valid_indices_p15], 
          type = 'l', col = "violet")
    
    
    legend("bottomright", legend = c("EW_ETF", "Loser","Winner"), col = c("blue", "red","violet"), lty = 1)
    
    #MFs
    period1 <- subset(data_mutual, date >= as.Date("1998-09-01") & date <= as.Date("2023-03-31"))
    
    window_size <- 126
    
    coeff_list_p14 <- list()
    coeff_list_p1 <- list()
    coeff_list_p11 <- list()
    
    for (i in window_size:length(period1$date)) {
      
      start_index <- i - window_size + 1
      end_index <- i 
      
      rolling_mom_d <- period1[start_index:end_index,]
      
      model_p14 <- lm(p14 ~ MKT.RF, data = rolling_mom_d)
      model_p1 <- lm(p1 ~ MKT.RF, data = rolling_mom_d)
      model_p11 <- lm(p11 ~ MKT.RF, data = rolling_mom_d)
      
      coeff_list_p14[[i]] <- coef(model_p14)[2]
      coeff_list_p1[[i]] <- coef(model_p1)[2]
      coeff_list_p11[[i]] <- coef(model_p11)[2]
      
    }
    
    valid_indices_p14 <- which(!is.na(coeff_list_p14))
    valid_indices_p1 <- which(!is.na(coeff_list_p1))
    valid_indices_p11 <- which(!is.na(coeff_list_p11))
    
    coeff_vec_p14 <- unlist(coeff_list_p14)
    coeff_vec_p1 <- unlist(coeff_list_p1)
    coeff_vec_p11 <- unlist(coeff_list_p11)
    
    plot(period1$date[(window_size + valid_indices_p14 - 1)], coeff_vec_p14[valid_indices_p14], 
         type = 'l', xlab = "Date", ylab = "Coefficient value for Mkt.RF", main = "Time variation in market beta for MFs",
         col = "blue", ylim = range(c(coeff_vec_p14, coeff_vec_p1,coeff_vec_p11)))
    
    lines(period1$date[(window_size + valid_indices_p1 - 1)], coeff_vec_p1[valid_indices_p1], 
          type = 'l', col = "red")
    lines(period1$date[(window_size + valid_indices_p11 - 1)], coeff_vec_p11[valid_indices_p11], 
          type = 'l', col = "violet")
    
    
    legend("bottomleft", legend = c("EW_MF", "Winner","Loser"), col = c("blue", "red","violet"), lty = 1)
    
    
    #######################
    #######Figure 5########
    #######################
    #ETFs
    #data_etf$date <- as.Date(data_etf$date, format = "%Y%m%d")
    #Equally Weighted
    period1 <- subset(data_etf, date >= as.Date("2016-01-04") & date <= as.Date("2023-03-31"))
    
    # Graph 1 time varying beta
    window_size <- 126
    coeff_list_p26 <- list()
    se_list_p26 <- list()
    
    for (i in window_size:length(period1$date)) {
      start_index <- i - window_size + 1
      end_index <- i 
      rolling_mom_d <- period1[start_index:end_index,]
      model_p26 <- lm(p26 ~ MKT.RF, data = rolling_mom_d)
      coeff_list_p26[[i]] <- coef(model_p26)[2]
      se_list_p26[[i]] <- summary(model_p26)$coefficients[2, 2] # Standard error of the coefficient
    }
    
    valid_indices_p26 <- which(!is.na(coeff_list_p26))
    coeff_vec_p26 <- unlist(coeff_list_p26)
    se_vec_p26 <- unlist(se_list_p26)
    
    plot(period1$date[(window_size + valid_indices_p26 - 1)], coeff_vec_p26[valid_indices_p26], 
         type = 'l', xlab = "Date", ylab = "Coefficient value for Mkt.RF", 
         col = "blue", ylim = range(coeff_vec_p26),
         main = "Time Variation in Market Beta Coefficients")
    
    # Add standard error bands
    upper_band <- coeff_vec_p26[valid_indices_p26] + 1.96 * se_vec_p26[valid_indices_p26]
    lower_band <- coeff_vec_p26[valid_indices_p26] - 1.96 * se_vec_p26[valid_indices_p26]
    polygon(
      c(period1$date[(window_size + valid_indices_p26 - 1)], rev(period1$date[(window_size + valid_indices_p26 - 1)])),
      c(upper_band, rev(lower_band)),
      col = rgb(0, 0, 1, 0.1),
      border = NA
    )
    
    
    # Graph 2 time-varying alpha
    window_size <- 126
    coeff_list_p26 <- list()
    se_list_p26 <- list()
    
    for (i in window_size:length(period1$date)) {
      start_index <- i - window_size + 1
      end_index <- i 
      rolling_mom_d <- period1[start_index:end_index,]
      model_p26 <- lm(p26 ~ MKT.RF, data = rolling_mom_d)
      coeff_list_p26[[i]] <- coef(model_p26)[1]
      se_list_p26[[i]] <- summary(model_p26)$coefficients[1, 2] # Standard error of the coefficient
    }
    
    valid_indices_p26 <- which(!is.na(coeff_list_p26))
    coeff_vec_p26 <- unlist(coeff_list_p26)
    se_vec_p26 <- unlist(se_list_p26)
    
    plot(period1$date[(window_size + valid_indices_p26 - 1)], coeff_vec_p26[valid_indices_p26], 
         type = 'l', xlab = "Date", ylab = "Coefficient value for alpha", 
         col = "blue", ylim = range(coeff_vec_p26),
         main = "Time Variation in Alpha Coefficients")
    
    # Add standard error bands
    upper_band <- coeff_vec_p26[valid_indices_p26] + 1.96 * se_vec_p26[valid_indices_p26]
    lower_band <- coeff_vec_p26[valid_indices_p26] - 1.96 * se_vec_p26[valid_indices_p26]
    polygon(
      c(period1$date[(window_size + valid_indices_p26 - 1)], rev(period1$date[(window_size + valid_indices_p26 - 1)])),
      c(upper_band, rev(lower_band)),
      col = rgb(0, 0, 1, 0.1),
      border = NA
    )
    
    #P5
    period1 <- subset(data_etf, date >= as.Date("2016-01-04") & date <= as.Date("2023-03-31"))
    
    # Graph 1 time varying beta
    window_size <- 126
    coeff_list_p5 <- list()
    se_list_p5 <- list()
    
    for (i in window_size:length(period1$date)) {
      start_index <- i - window_size + 1
      end_index <- i 
      rolling_mom_d <- period1[start_index:end_index,]
      model_p5 <- lm(p5 ~ MKT.RF, data = rolling_mom_d)
      coeff_list_p5[[i]] <- coef(model_p5)[2]
      se_list_p5[[i]] <- summary(model_p5)$coefficients[2, 2] # Standard error of the coefficient
    }
    
    valid_indices_p5 <- which(!is.na(coeff_list_p5))
    coeff_vec_p5 <- unlist(coeff_list_p5)
    se_vec_p5 <- unlist(se_list_p5)
    
    plot(period1$date[(window_size + valid_indices_p5 - 1)], coeff_vec_p5[valid_indices_p5], 
         type = 'l', xlab = "Date", ylab = "Coefficient value for Mkt.RF", 
         col = "blue", ylim = range(coeff_vec_p5),
         main = "Time Variation in Market Beta Coefficients")
    
    # Add standard error bands
    upper_band <- coeff_vec_p5[valid_indices_p5] + 1.96 * se_vec_p26[valid_indices_p5]
    lower_band <- coeff_vec_p5[valid_indices_p5] - 1.96 * se_vec_p26[valid_indices_p5]
    polygon(
      c(period1$date[(window_size + valid_indices_p5 - 1)], rev(period1$date[(window_size + valid_indices_p5 - 1)])),
      c(upper_band, rev(lower_band)),
      col = rgb(0, 0, 1, 0.1),
      border = NA
    )
    
    
    # Graph 2 time-varying alpha
    window_size <- 126
    coeff_list_p5 <- list()
    se_list_p5 <- list()
    
    for (i in window_size:length(period1$date)) {
      start_index <- i - window_size + 1
      end_index <- i 
      rolling_mom_d <- period1[start_index:end_index,]
      model_p5 <- lm(p5 ~ MKT.RF, data = rolling_mom_d)
      coeff_list_p5[[i]] <- coef(model_p5)[1]
      se_list_p5[[i]] <- summary(model_p5)$coefficients[1, 2] # Standard error of the coefficient
    }
    
    valid_indices_p5 <- which(!is.na(coeff_list_p5))
    coeff_vec_p5 <- unlist(coeff_list_p5)
    se_vec_p5 <- unlist(se_list_p5)
    
    plot(period1$date[(window_size + valid_indices_p5 - 1)], coeff_vec_p5[valid_indices_p5], 
         type = 'l', xlab = "Date", ylab = "Coefficient value for alpha", 
         col = "blue", ylim = range(coeff_vec_p5),
         main = "Time Variation in Alpha Coefficients")
    
    # Add standard error bands
    upper_band <- coeff_vec_p5[valid_indices_p5] + 1.96 * se_vec_p5[valid_indices_p5]
    lower_band <- coeff_vec_p5[valid_indices_p5] - 1.96 * se_vec_p5[valid_indices_p5]
    polygon(
      c(period1$date[(window_size + valid_indices_p5 - 1)], rev(period1$date[(window_size + valid_indices_p5 - 1)])),
      c(upper_band, rev(lower_band)),
      col = rgb(0, 0, 1, 0.1),
      border = NA
    )
    
    #MFs
    #EW
    
    period1 <- subset(data_mutual, date >= as.Date("1999-01-04") & date <= as.Date("2023-03-31"))
    
    # Graph 1 time varying beta
    window_size <- 126
    coeff_list_p14 <- list()
    se_list_p14 <- list()
    
    for (i in window_size:length(period1$date)) {
      start_index <- i - window_size + 1
      end_index <- i 
      rolling_mom_d <- period1[start_index:end_index,]
      model_p14 <- lm(p14 ~ MKT.RF, data = rolling_mom_d)
      coeff_list_p14[[i]] <- coef(model_p14)[2]
      se_list_p14[[i]] <- summary(model_p14)$coefficients[2, 2] # Standard error of the coefficient
    }
    
    valid_indices_p14 <- which(!is.na(coeff_list_p14))
    coeff_vec_p14 <- unlist(coeff_list_p14)
    se_vec_p14 <- unlist(se_list_p14)
    
    plot(period1$date[(window_size + valid_indices_p14 - 1)], coeff_vec_p14[valid_indices_p14], 
         type = 'l', xlab = "Date", ylab = "Coefficient value for Mkt.RF", 
         col = "blue", ylim = range(coeff_vec_p14),
         main = "Time Variation in Market Beta Coefficients")
    
    # Add standard error bands
    upper_band <- coeff_vec_p14[valid_indices_p14] + 1.96 * se_vec_p14[valid_indices_p14]
    lower_band <- coeff_vec_p14[valid_indices_p14] - 1.96 * se_vec_p14[valid_indices_p14]
    polygon(
      c(period1$date[(window_size + valid_indices_p14 - 1)], rev(period1$date[(window_size + valid_indices_p14 - 1)])),
      c(upper_band, rev(lower_band)),
      col = rgb(0, 0, 1, 0.1),
      border = NA
    )
    
    
    # Graph 2 time-varying alpha
    window_size <- 126
    coeff_list_p14 <- list()
    se_list_p14 <- list()
    
    for (i in window_size:length(period1$date)) {
      start_index <- i - window_size + 1
      end_index <- i 
      rolling_mom_d <- period1[start_index:end_index,]
      model_p14 <- lm(p14 ~ MKT.RF, data = rolling_mom_d)
      coeff_list_p14[[i]] <- coef(model_p14)[1]
      se_list_p14[[i]] <- summary(model_p14)$coefficients[1, 2] # Standard error of the coefficient
    }
    
    valid_indices_p14 <- which(!is.na(coeff_list_p14))
    coeff_vec_p14 <- unlist(coeff_list_p14)
    se_vec_p14 <- unlist(se_list_p14)
    
    plot(period1$date[(window_size + valid_indices_p14 - 1)], coeff_vec_p14[valid_indices_p14], 
         type = 'l', xlab = "Date", ylab = "Coefficient value for alpha", 
         col = "blue", ylim = range(coeff_vec_p14),
         main = "Time Variation in Alpha Coefficients")
    
    # Add standard error bands
    upper_band <- coeff_vec_p14[valid_indices_p14] + 1.96 * se_vec_p14[valid_indices_p14]
    lower_band <- coeff_vec_p14[valid_indices_p14] - 1.96 * se_vec_p14[valid_indices_p14]
    polygon(
      c(period1$date[(window_size + valid_indices_p14 - 1)], rev(period1$date[(window_size + valid_indices_p14 - 1)])),
      c(upper_band, rev(lower_band)),
      col = rgb(0, 0, 1, 0.1),
      border = NA
    )
    
    
    
    #P3
    
    
    period1 <- subset(data_mutual, date >= as.Date("1999-01-04") & date <= as.Date("2023-03-31"))
    
    # Graph 1 time varying beta
    window_size <- 126
    coeff_list_p3 <- list()
    se_list_p3 <- list()
    
    for (i in window_size:length(period1$date)) {
      start_index <- i - window_size + 1
      end_index <- i 
      rolling_mom_d <- period1[start_index:end_index,]
      model_p3 <- lm(p3 ~ MKT.RF, data = rolling_mom_d)
      coeff_list_p3[[i]] <- coef(model_p3)[2]
      se_list_p3[[i]] <- summary(model_p3)$coefficients[2, 2] # Standard error of the coefficient
    }
    
    valid_indices_p3 <- which(!is.na(coeff_list_p3))
    coeff_vec_p3 <- unlist(coeff_list_p3)
    se_vec_p3 <- unlist(se_list_p3)
    
    plot(period1$date[(window_size + valid_indices_p3 - 1)], coeff_vec_p3[valid_indices_p3], 
         type = 'l', xlab = "Date", ylab = "Coefficient value for Mkt.RF", 
         col = "blue", ylim = range(coeff_vec_p3),
         main = "Time Variation in Market Beta Coefficients")
    
    # Add standard error bands
    upper_band <- coeff_vec_p3[valid_indices_p3] + 1.96 * se_vec_p3[valid_indices_p3]
    lower_band <- coeff_vec_p3[valid_indices_p3] - 1.96 * se_vec_p3[valid_indices_p3]
    polygon(
      c(period1$date[(window_size + valid_indices_p3 - 1)], rev(period1$date[(window_size + valid_indices_p3 - 1)])),
      c(upper_band, rev(lower_band)),
      col = rgb(0, 0, 1, 0.1),
      border = NA
    )
    
    
    # Graph 2 time-varying alpha
    window_size <- 126
    coeff_list_p3 <- list()
    se_list_p3 <- list()
    
    for (i in window_size:length(period1$date)) {
      start_index <- i - window_size + 1
      end_index <- i 
      rolling_mom_d <- period1[start_index:end_index,]
      model_p3 <- lm(p3 ~ MKT.RF, data = rolling_mom_d)
      coeff_list_p3[[i]] <- coef(model_p3)[1]
      se_list_p3[[i]] <- summary(model_p3)$coefficients[1, 2] # Standard error of the coefficient
    }
    
    valid_indices_p3 <- which(!is.na(coeff_list_p3))
    coeff_vec_p3 <- unlist(coeff_list_p3)
    se_vec_p3 <- unlist(se_list_p3)
    
    plot(period1$date[(window_size + valid_indices_p3 - 1)], coeff_vec_p3[valid_indices_p3], 
         type = 'l', xlab = "Date", ylab = "Coefficient value for alpha", 
         col = "blue", ylim = range(coeff_vec_p3),
         main = "Time Variation in Alpha Coefficients")
    
    # Add standard error bands
    upper_band <- coeff_vec_p3[valid_indices_p3] + 1.96 * se_vec_p3[valid_indices_p3]
    lower_band <- coeff_vec_p3[valid_indices_p3] - 1.96 * se_vec_p3[valid_indices_p3]
    polygon(
      c(period1$date[(window_size + valid_indices_p3 - 1)], rev(period1$date[(window_size + valid_indices_p3 - 1)])),
      c(upper_band, rev(lower_band)),
      col = rgb(0, 0, 1, 0.1),
      border = NA
    )
    
 