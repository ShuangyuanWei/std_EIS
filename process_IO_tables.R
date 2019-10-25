library(reshape2)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(RJSONIO)
library(data.table)
library(scales)
library(multtest)
library(gridExtra)
library(grid)
library(gtools)
library(viridis)
rm(list=ls(all=TRUE))
options(lfe.threads=8)
setwd("/Users/shuanwei/WorkDocs/Public Policy/EIS/Taiwan/")
raw_io <- readxl::read_excel("Raw IO.xlsx")
io_2 <- raw_io[-1,]
colnames(io_2)[colnames(io_2)=="To: (sector in column)"] <- "colname"
io_3 <- io_2[!grepl("IMP_", io_2$colname),]
io_3 <- io_3[!grepl("TXS_", io_3$colname),]
io_3 <- io_3[!grepl("TTL_", io_3$colname),]
io_3 <- io_3[!grepl("VAL", io_3$colname),]
io_3 <- io_3[!grepl("DOM_97T98", io_3$colname),]
dropcol <- c("D97T98: Private households with employed persons",   "HFCE: Final consumption expenditure of households" , 
             "NPISH: Final consumption expenditure of non-profit institutions serving households", "GGFC: Final consumption expenditure of general government"  ,
             "GFCF: Gross Fixed Capital Formation" , "INVNT: Changes in inventories","CONS_ABR: Direct purchases abroad by residents (imports)" ,
             "CONS_NONRES: Direct purchases by non-residents (exports)"  ,"EXPO: Exports (cross border)" ,  "IMPO: Imports (cross border)"     ) 
io_4 <- io_3[ , !(names(io_3) %in% dropcol)]
t <- as.numeric(io_4[grepl("OUTPUT: Output", io_4$colname),][-1])

io_4 <- io_4[!grepl("OUTPUT: Output", io_4$colname),]

### add the labor compensation
raw_val <- readxl::read_excel("Raw VAL.xlsx")
val_1 <- as.numeric(raw_val[2,2:dim(raw_val)[2]])
#val_1$t <- as.numeric(io_4[dim(io_4)[1],dim(io_4)[2]])
df <- rbind(io_4,c(0,val_1))



### add the household consumption
cons <- as.matrix(io_3["HFCE: Final consumption expenditure of households"])
df <- cbind(df,cons)


## add the total row
total <- c(t, sum(io_2["HFCE: Final consumption expenditure of households"])/2)
df <- rbind(df[,-1], c(total))



##################### Calculate Tecnical Coefficient ##################### 
tech_coef <- sweep(df[, -1], 1, df[,"t"], "/") 
tech_coef <- tech_coef[,!(names(tech_coef) %in% c("t"))]
#################### Calcualte Leontief ##################
I <- diag(dim(tech_coef)[1])
A <-  as.matrix(tech_coef)
IminusA <- I - A
Leontief <- solve(IminusA)
################### Type II output multiplier
type2_outputM <- rowSums(Leontief)

################## Get Earrnings DR Matrix ###############
earnings <- as.numeric(raw_val[2,-dim(raw_val)[2]])[-1]
earnings<- c(earnings, mean(earnings))
earnings_dr <-replicate(36,earnings)
earnnings_dr_M <- earnings_dr * Leontief
################### Type II earnings multiplier
type2_earningsM <- rowSums(earnnings_dr_M)

################## Get Value-Added DR Matrix ###############
valadded<- io_2[grepl("VAL", io_2$colname),]
valadded <- valadded[ , !(names(valadded) %in% dropcol)]
valadded <- valadded[,!(names(valadded) %in% c("TOTAL: Total"))]

valadded <- as.numeric(valadded)
valadded<- c(valadded, mean(valadded))[-1]
valadded_dr <-replicate(36,valadded)
valadded_dr_M <- valadded_dr * Leontief
################### Type II Value-Added  multiplier
type2_valaddedM <- rowSums(valadded_dr_M)
