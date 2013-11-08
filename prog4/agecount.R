agecount <- function(age = NULL) {
## Check that "age" is non-NULL; else throw error
## Read "homicides.txt" data file
## Extract ages of victims; ignore records where no age is
## given
## Return integer containing count of homicides for that age
	if (is.null(age)){
		stop ("Age cannot be null")
	}

	homicides <- readLines("homicides.txt")
	r<-regexec("([0-9]{1,3}) years old", homicides)	
	m<-regmatches(homicides, r)
	ages<-sapply(m, function(x) as.numeric(x[2]))
	length(which(ages==as.numeric(age)))
}
