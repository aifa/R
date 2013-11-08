rankall <- function(outcome, num = "best") {
## Read outcome data
## Check that num and outcome are valid
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name

	rawdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	data<-rawdata
	
	if (nrow(data)==0){
		stop("invalid state")
	}
	
	columnNumber<-0
	if (outcome=="heart attack"){
		columnNumber<-11
	}else
	if (outcome=="heart failure"){
		columnNumber<-17		
	}else 
	if (outcome=="pneumonia"){
		columnNumber<-23		
	}else{
		stop("invalid outcome")
	}
	
	dRateValues <- split(data[,columnNumber], data$State)
	header <- c("hospital", "state")
	df <- data.frame(matrix(ncol=2, nrow=seq_along(dRateValues)-1))
	colnames(df)<-header
	
	for (i in seq_along(dRateValues)){
		localNum<-num
		dRateVal = dRateValues[i][[1]]
		state <- names(dRateValues)[i]
		dRateVal <- as.numeric(dRateVal)
		dRateVal<- na.omit(dRateVal)
		stateData<- subset(data, State==state)
		bestH <- subset(stateData, as.numeric(stateData[,columnNumber]) %in% dRateVal )
		bestH <- bestH[order(as.numeric(bestH[, columnNumber]), bestH[, 2]), 2]
		
		if (localNum=="best"){
			localNum=1
		}else if (localNum=="worst"){
			localNum=length(dRateVal)
		}	
		
		df[state, "hospital"] <- bestH[localNum]
		df[state, "state"] <- state
	}
	
	df
}
