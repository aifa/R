source("getmonitor.R")
source("complete.R")
corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations

	df_monitor <- complete(directory)
	res <- vector()
	res[0]=0
	if (nrow(df_monitor)>0){
		rows <- nrow(df_monitor)
		j<-1
		for (i in 1:rows){
			if (df_monitor[i, "nobs"]>threshold){
				monitor <- getmonitor(df_monitor[i, "id"], directory)
				c <- cor(monitor[,"sulfate"],monitor[,"nitrate"], use="complete.obs")
				res[j] <- c
				j <- j+1		
			}
		}
	}
	
	res	
}