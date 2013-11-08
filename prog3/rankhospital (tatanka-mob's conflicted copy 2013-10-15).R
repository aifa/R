rankhospital <- function(st, outcome, num = "best") {
## Read outcome data
## Check that state and outcome are valid
## Return hospital name in that state with the given rank
## 30-day death rate
	rawdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	t_State <- (rawdata$State)

	if (!(st%in% t_State))	
	{
		stop("invalid state")
	}
	
	data<-subset(rawdata, rawdata$State==st)
	
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

	dRateValues <- na.omit(as.numeric(data[,columnNumber]))
	print(length(data))
	dRate <- order(dRateValues)
	print(dRateValues)
	print(length(dRateValues))
	bestH <- subset(data, data[,columnNumber] %in% dRateValues)
	length(bestH)
	bestH<-bestH[order(as.numeric(bestH[, columnNumber]), bestH[, 2]), 2]
	
	if (num=="best"){
		num=1
	}else if (num=="worst"){
		num=length(bestH)
	}	
	print(num)
	bestH[num]

}
