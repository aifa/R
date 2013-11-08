count <- function(cause = NULL) {
## Check that "cause" is non-NULL; else throw error
## Check that specific "cause" is allowed; else throw error
## Read "homicides.txt" data file
## Extract causes of death
## Return integer containing count of homicides for that cause

	if (is.null(cause)){
		stop ("Cause cannot be null")
	}

	v_causes <-c ("asphyxiation", "blunt force", "other", "shooting", "stabbing", "unknown")
	patterns <-c ("[a|A]sphyxiation", "[B|b]lunt force", "[o|O]ther", "[s|S]hooting", "[s|S]tabbing", "[u|U]nknown")

	if (any(v_causes==cause)){
	
		homicides <- readLines("homicides.txt")
		r<-regexec("<dd>[C|c]ause: (.*?)</dd>", homicides)	
		m<-regmatches(homicides, r)
		actualCause<-sapply(m, function(x) x[2])
		length(grep(patterns[which(v_causes==cause)], actualCause))
	}else {
		stop (cat("cause must be one of :" , v_causes))
	}
}
