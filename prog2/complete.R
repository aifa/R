source("getmonitor.R")
complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
	  
	  header <- c("id", "nobs")
	  df <- data.frame(matrix(ncol=2, nrow=seq_along(id)))
	  colnames(df)<-header
 	  
	  if (length(id)==1){
	  	
		data <- getmonitor(id, directory)
		completeData <- data[complete.cases(data),]
		df[1, "id"]<-id
		df[1, "nobs"]<-nrow(completeData)
	  }else if (length(id)>1){
	  	for (i in seq_along(id)){
			data <- getmonitor(id[i], directory)
			completeData <- data[complete.cases(data),]
			df[i, "id"] <- id[i]
			df[i, "nobs"] <- nrow(completeData)
		}
	  } 	  
	  df
}