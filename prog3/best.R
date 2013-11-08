best<-function(st, outcome){
	
	rawdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	t_State <- (rawdata$State)

	if (!(st%in%t_State))	
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
	dRate <- as.numeric(data[,columnNumber])
	dRate <- order(dRate)
	dRate <- data[dRate[1],columnNumber]
	bestH <- subset(data, data[,columnNumber]==dRate)
	bestH<-bestH[order(bestH[, 2]), 2]
	bestH[1]
}