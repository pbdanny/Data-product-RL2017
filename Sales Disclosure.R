d <- "F:/Backup/OSS - Disclose Sales Data/Data"

library(xlsx)

excelToDF <- function(d) {
  x <- list.files(d)
  x <- x[!grepl("^~", x)]  # deselect hidden file
  x <- x[grepl("[xlsx|xls]", x)] # select only file with xlsx or xls
  out <- lapply(paste(d, x, sep = "/"), read.xlsx2, sheetName = "Template", stringsAsFactors = FALSE)
  # To combine row required to use same colname of each dataframe as combine index
  # Use first data.frame as start point
  y <- out[[1]]
  for(i in 2:length(out)){
    #  Cast list to dataframe with renames to the first colnames
    dummy <- as.data.frame.list(out[[i]], col.names = names(y))
    y <- rbind(y, dummy)
  }
  return(y)
}

z <- excelToDF(d)

write.xlsx2(z, file = "AllTeamSalesDeclare.xlsx", sheetName = "Sheet1", row.names = FALSE)