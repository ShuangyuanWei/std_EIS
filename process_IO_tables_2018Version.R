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
             "CONS_NONRES: Direct purchases by non-residents (exports)"  ,"EXPO: Exports (cross border)" ,  "IMPO: Imports (cross border)","TOTAL: Total"     ) 
io_4 <- io_3[ , !(names(io_3) %in% dropcol)]

# Extract the total output row
t <- as.numeric(io_4[grepl("OUTPUT: Output", io_4$colname),][-1])
# Drop the total output
io_4 <- io_4[!grepl("OUTPUT: Output", io_4$colname),]

### add the labor compensation
raw_val <- readxl::read_excel("Raw VAL.xlsx")
val_1 <- as.numeric(raw_val[2,2:(dim(raw_val)[2]-1)])
#val_1$t <- as.numeric(io_4[dim(io_4)[1],dim(io_4)[2]])
df <- rbind(io_4,c(0,val_1))



### add the household consumption
cons <- as.matrix(io_3["HFCE: Final consumption expenditure of households"])
df <- cbind(df,cons)


## add the total row
total <- c(t, sum(io_2["HFCE: Final consumption expenditure of households"])/2)
df <- rbind(df[,-1], c(total))



##################### Calculate Tecnical Coefficient ##################### 


tech_coef<- t(t(df) / total)
n <- dim(tech_coef)[1]
tech_coef <- tech_coef[-n,]
#################### Calcualte Leontief ##################
I <- diag(dim(tech_coef)[1])
A <-  as.matrix(tech_coef)
IminusA <- I - A
Leontief <- solve(IminusA)


################### Type II output multiplier
type2_outputM <- colSums(Leontief)
write.csv(type2_outputM  , file =paste0("type2_outputM",".csv"))

################## Get Earrnings DR Matrix ###############
earnings <- as.numeric(raw_val[2,-dim(raw_val)[2]])[-1]
earnings <- earnings/t
earnings<- c(earnings, mean(earnings))



earnings_dr <-replicate(dim(Leontief)[1],earnings)
earnnings_dr_M <- earnings_dr * Leontief
################### Type II earnings multiplier
type2_earningsM <- colSums(earnnings_dr_M)
write.csv(type2_earningsM  , file =paste0("type2_earningsM",".csv"))



################## Get Value-Added DR Matrix ###############
valadded<- io_2[grepl("VAL", io_2$colname),]
valadded <- valadded[ , !(names(valadded) %in% dropcol)]
valadded <- valadded[,!(names(valadded) %in% c("TOTAL: Total"))][-1]
valadded <- as.numeric(valadded)
valadded <- valadded/t
valadded <- c(valadded, mean(valadded))

valadded_dr <-replicate(dim(Leontief)[1],valadded)
valadded_dr_M <- valadded_dr * Leontief
################### Type II Value-Added  multiplier
type2_valaddedM <- colSums(valadded_dr_M)
write.csv(type2_valaddedM  , file =paste0("type2_valaddedM",".csv"))


c <- io_4$colname
write.csv( c , file =paste0("sectorname",".csv"))


################## employment multiplier
raw_emp <- read.csv("ilostat-export (14).csv")
emp <- raw_emp %>% filter(sex.label=="Sex: Total")
emp <- filter(emp, grepl("Detailed",classif1.label))

map <- read.csv("oecd_isic_mapping.csv")
emp_oecd <- merge(emp, map, 
      by.x = "classif1.label", by.y = "ISIC",  all.x = TRUE) %>% select(c("classif1.label","time","obs_value","OECD"))

earnings_t <- melt(raw_val[2,-dim(raw_val)[2]])
oecd_emp_earn <- merge(emp_oecd, earnings_t, 
                           by.x = "OECD", by.y = "variable",  all.x = TRUE)
isic_earn <- as.data.table(oecd_emp_earn )[,.(total_earn = sum(value)), by=.(classif1.label)] 
oecd_isic_earn <- merge(oecd_emp_earn , isic_earn, 
                        by.x = "classif1.label", by.y = "classif1.label",  all.x = TRUE)

adjust_rate <- 1.03642665
oecd_isic_earn$adj_earning <- oecd_isic_earn$total_earn * adjust_rate
oecd_isic_earn$emp_earn_ratio <- oecd_isic_earn$obs_value/oecd_isic_earn$adj_earning

emp_earn_ratio <- oecd_isic_earn %>% select(c("OECD","emp_earn_ratio")) %>% filter(!is.na(oecd_isic_earn$OECD)) %>% arrange(OECD)
v <- c(as.numeric(emp_earn_ratio$emp_earn_ratio), mean(emp_earn_ratio$emp_earn_ratio))
employment_dr <- matrix(v,nrow = dim(earnings_dr)[1], ncol = dim(earnings_dr)[2])

employment_dr_M <- earnnings_dr_M * employment_dr 
type2_employmentM <- colSums(employment_dr_M)
write.csv( c , file =paste0("type2_employmentM",".csv"))
