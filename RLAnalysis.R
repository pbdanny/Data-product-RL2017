#### RL Analysis ####

## Load data & change column name to R compatible format ----
file <- file.choose()
rl2017 <- read.delim(file, header = T, na.strings = c("NA", ""), stringsAsFactors = F)
colnames(rl2017) <- make.names(colnames(rl2017))

## 1) Data sanitation check ----
# 1.1 check reasonable data type
# found Criteria_Code, Occupation_Code, Month need conversion to
# charactor type
rl2017$Criteria_Code <- as.character(rl2017$Criteria_Code)
rl2017$Occupation_Code <- as.character(rl2017$Occupation_Code)
rl2017$Month <- as.character(rl2017$Month)

# 1.2 Check outlier
# Found extreme outliner Monthly_Salary = 18MMTHB/Mth
# Directly Remove the record, since others varible
# shown irreasonable,
rl2017 <- rl2017[rl2017$Monthly_Salary < 5000000,]

# Found AGE <- 0, use mice to imputate age
library(mice)
# MICE work for data = NA , then convert AGE<0 to NA
rl2017[rl2017$AGE < 0,]$AGE <- NA
# Create new Dataframe only varible use to imputate
df <- rl2017[c("ZipCode", "Monthly_Salary", "AGE", "Occupation_Code")]
# Create imputated object name 'imp'
imp <- mice(df)
# use 'complete' command to createcompleted imputated DataFrame
# from imputate object
rl2017$AGE <- complete(imp)$AGE

# 1.3 Clear unused dataframe and save R Data file
rm(list = c("df", "imp"))
saveRDS(rl2017, file = "/Users/Danny/Share Win7/rl2017.RDA")

## 2) EDA check ----
# Load Data
rl2017 <- readRDS(file = "/Users/Danny/Share Win7/rl2017.RDA")

# 2.1 Univariate plot 
# Finalized by source code
# Create contingency table count of Source Code, sorted
tableSource <- sort(table(rl2017$Source_Code))
# Barplot , create matrix 'barCenterX' : center of each bar in coordinate-x
# Extend plot area with ylim + 20% of max y value
barCenterX <- barplot(tableSource, ylim = c(0, 1.2*max(tableSource)))
# Annotated plot wtih text at position x = barCenterX
# y position = count of Source Code, lable = count of Source Code
# pos = 3, for label above y position specified
text(x = barCenterX, y = tableSource, labels = tableSource, pos = 3)
title(main = "Count of finalized by Source Code", sub = "data Jan. - Sep. 2017")
# 2.2 Bivariate plot

# From initail plot show outliner 
# filter out Monthly_Salary > 10000
library(ggplot2)

ggplot(data = subset(rl2017, Monthly_Salary > 1000),
       aes(x = AGE, 
           y = log10(Monthly_Salary), 
                     color = as.factor(Result))) +
  geom_jitter(alpha = 0.5)

# 2.3 Geographical plot