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

## 2) EDA check ----
# 2.1 Univariate plot
# Finalized by source code

# Create contingency table count of Source Code, sorted
tt <- sort(table(rl2017$Source_Code))
# Barplot , create matrix 'barCenterX' : center of each bar in coordinate-x
barCenterX <- barplot(tt)
# Annotated plot wtih text at position x = barCenterX
# y position = count of Source Code, lable = count of Source Code 
text(x = barCenterX, tt, tt)
# 2.2 Bivariate plot
# 2.3 Geographical plot

