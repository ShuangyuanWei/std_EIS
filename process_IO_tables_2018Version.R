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
library(readxl)
rm(list=ls(all=TRUE))
options(lfe.threads=8)
dirname <- "/Users/shuanwei/WorkDocs/Public Policy/EIS/"



process_io_tables <- function(countryname) {
  setwd(paste0(dirname,countryname,"/"))
  ##################### Import and clean the Raw_IO file ##################### 
  raw_io <- readxl::read_excel("Raw_IO.xlsx")
  # drop empty first row and second colum 
  io_2 <- raw_io[-1,]
  io_2 <- io_2[,-2]
  
  colnames(io_2)[colnames(io_2)=="To: (sector in column)"] <- "sectorname"
  # drop irrelevant columns and rows
  
  io_3 <- io_2[!grepl("IMP_", io_2$sectorname),]
  io_3 <- io_3[!grepl("TXS_", io_3$sectorname),]
  io_3 <- io_3[!grepl("TTL_", io_3$sectorname),]
  io_3 <- io_3[!grepl("VAL", io_3$sectorname),]
  io_3 <- io_3[!grepl("DOM_97T98", io_3$sectorname),] #DOM_97T98: Private households with employed person
  
  dropcol <- c("D97T98: Private households with employed persons",   "HFCE: Final consumption expenditure of households" , 
               "NPISH: Final consumption expenditure of non-profit institutions serving households", "GGFC: Final consumption expenditure of general government"  ,
               "GFCF: Gross Fixed Capital Formation" , "INVNT: Changes in inventories","CONS_ABR: Direct purchases abroad by residents (imports)" ,
               "CONS_NONRES: Direct purchases by non-residents (exports)"  ,"EXPO: Exports (cross border)" ,  "IMPO: Imports (cross border)","TOTAL: Total"     ) 
  io_4 <- io_3[ , !(names(io_3) %in% dropcol)]
  
  # Extract the total output row
  t <- as.numeric(io_4[grepl("OUTPUT: Output", io_4$sectorname),][-1])
  # Drop the total output
  io_4 <- io_4[!grepl("OUTPUT: Output", io_4$sectorname),]
  
  ##################### Import and clean the Raw_VAL file ##################### 
  
  
  raw_val <- readxl::read_excel("Raw_VAL.xlsx")
  val_1 <- raw_val[-1,]
  val_2 <- val_1[,-2]
  colnames(val_2)[colnames(val_2)=="To: (sector in column)"] <- "sectorname"
  
  
  
  ##################### Create the input-output matrix ##################### 
  
  ### add the labor compensation row
  labor <- val_2[1, -dim(val_2)[2]]
  df <- rbind(io_4,labor)
  
  ### add the household consumption column
  cons <- as.matrix(io_3["HFCE: Final consumption expenditure of households"])
  df <- cbind(df,cons)
  
  ## add the total row
  total <- c(t, sum(io_2["HFCE: Final consumption expenditure of households"])/2)
  df <- rbind(df[,-1], c(total))
  
  ## created a matrix with an additional row of lobor consumption and column of household consumption
  
  
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
  #write.csv(type2_outputM  , file =paste0("type2_outputM",".csv"))
  
  ################## Get Earrnings DR Matrix ###############
  #earnings <- as.numeric(raw_val[2,-dim(raw_val)[2]])[-1]
  earnings <- as.numeric(labor[,-1])
  earnings <- earnings/t
  earnings<- c(earnings, mean(earnings))
  
  earnings_dr <-replicate(dim(Leontief)[1],earnings)
  earnnings_dr_M <- earnings_dr * Leontief
  ################### Type II earnings multiplier
  type2_earningsM <- colSums(earnnings_dr_M)
  #write.csv(type2_earningsM  , file =paste0("type2_earningsM",".csv"))
  
  
  
  ################## Get Value-Added DR Matrix ###############
  valadded<- io_2[grepl("VAL", io_2$sectorname),]
  valadded <- valadded[ , !(names(valadded) %in% dropcol)]
  valadded <- valadded[,!(names(valadded) %in% c("TOTAL: Total"))][-1]
  valadded <- as.numeric(valadded)
  valadded <- valadded/t
  valadded <- c(valadded, mean(valadded))
  
  valadded_dr <-replicate(dim(Leontief)[1],valadded)
  valadded_dr_M <- valadded_dr * Leontief
  ################### Type II Value-Added  multiplier
  type2_valaddedM <- colSums(valadded_dr_M)
  #write.csv(type2_valaddedM  , file =paste0("type2_valaddedM",".csv"))
  
  
  c <- io_4$sectorname
  #write.csv( c , file =paste0("sectorname",".csv"))
  
  
  
  ################## employment multiplier  ##################
  raw_emp <- read.csv("ILOSTAT_.csv")
  intrate <- readxl::read_excel("imf-dm-export-20191030.xls")
  
  # clean the raw employment data and expand it to OECD industries
  emp <- raw_emp %>% filter(sex.label=="Sex: Total")
  emp <- filter(emp, grepl("Detailed",classif1.label))
  
  map <- read.csv("oecd_isic_mapping.csv")
  emp_oecd <- merge(emp, map, 
        by.x = "classif1.label", by.y = "ISIC",  all.x = TRUE) %>% select(c("classif1.label","time","obs_value","OECD"))
  
  # merge with earnings to get the earnings by oecd industry
  earnings_t <- melt(labor)
  oecd_emp_earn <- merge(emp_oecd, earnings_t, 
                             by.x = "OECD", by.y = "variable",  all.x = TRUE)
  # calcualte the earnings by isic industry
  isic_earn <- as.data.table(oecd_emp_earn )[,.(total_earn = sum(value)), by=.(classif1.label)] 
  oecd_isic_earn <- merge(oecd_emp_earn , isic_earn, 
                          by.x = "classif1.label", by.y = "classif1.label",  all.x = TRUE)
  
  # clean and calculate inflation rate
  infrate <- intrate[2,c("2016","2017","2018")]
  adjust_rate <- as.numeric((infrate["2016"]/100 + 1) * (infrate["2017"]/100 + 1) * (infrate["2018"]/100 + 1))

  oecd_isic_earn$adj_earning <- oecd_isic_earn$total_earn * adjust_rate
  oecd_isic_earn$emp_earn_ratio <- oecd_isic_earn$obs_value/oecd_isic_earn$adj_earning
  
  emp_earn_ratio <- oecd_isic_earn %>% select(c("OECD","emp_earn_ratio")) %>% filter(!is.na(oecd_isic_earn$OECD)) %>% arrange(OECD)
  v <- c(as.numeric(emp_earn_ratio$emp_earn_ratio), mean(emp_earn_ratio$emp_earn_ratio))
  employment_dr <- matrix(v,nrow = dim(earnings_dr)[1], ncol = dim(earnings_dr)[2])
  
  employment_dr_M <- earnnings_dr_M * employment_dr 
  type2_employmentM <- colSums(employment_dr_M)
  #write.csv( c , file =paste0("type2_employmentM",".csv"))
  
  ########### combine all the multipliers ###############
  combined_m <- cbind(type2_outputM, type2_earningsM, type2_valaddedM, type2_employmentM)
  sector <-  c(c,"household")
  combined_m <- cbind(sector, combined_m)
  
  write.csv( c , file =paste0("type2_multipliers",".csv"))
}

process_io_tables("Thailand")