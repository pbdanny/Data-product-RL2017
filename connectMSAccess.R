#### Run on Windows environment ####

# Connect MS Accesss Database ----
library(RODBC)
dbFile <- file.choose()
con <- odbcConnectAccess2007(dbFile)
sqlTables(con)

# Preview data : query top 10 rows ----
queryTop10 <- "select top 10 *
		   from RL_2016_2017
		   ;"
topDF <- sqlQuery(con, queryTop10, as.is = TRUE)
write.table(topDF, "topDF.txt", row.names = FALSE, 
		col.names = TRUE, quote = TRUE, sep = "\t", 
		fileEncoding="UTF-8")

# Fetch RL Data approved in 2017 ----
queryRL2017 <- "select
		Source_Code, ZipCode, Region, Approve_Amount,
		Money_Transfer, Monthly_Salary, 
		DAY_CLOSE, blk_Code, BALANCE,
		Month, Result, Result_Description, Criteria_Code,
		AGE, Occupation_Code, Doc_Waive, BUNDLE_FLAG
		from RL_2016_2017 
		where Month between '201701' and '201709'
		;"

rl2017 <- sqlQuery(con, queryRL2017, as.is = TRUE)

write.table(rl2017, "RL2017.txt", row.names = FALSE, 
		col.names = TRUE, quote = TRUE, sep = "\t", 
		fileEncoding="UTF-8")

#### Run on OSX Environment ####

file <- file.choose()
read.delim(file, stringsAsFactors = F, na.strings = c("NA", ""))


