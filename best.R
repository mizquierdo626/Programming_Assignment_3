best <- function(state, outcome)			{
	
	#All of the possible outcomes
		outcomes = c("heart attack", "heart failure", "pneumonia")

	#Check if outcome is one of the strings in outcomes
	#-- %in% is an intuitive interface, which returns a vector if there is a match or not
		if (outcome %in% outcomes == FALSE) 	{
			stop("invalid outcome")
}

	#Read outcome data
		data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	#Retrieve the columns below from 'data' and place it in 'data' with new names:
	#--'Hospital.Name'
	#--'State'
    #--"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"  
    #--"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    #--"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
		data <- data[c(2, 7, 11, 17, 23)]
			names(data)[1] <- "name"
			names(data)[2] <- "state"
			names(data)[3] <- "heart attack"
			names(data)[4] <- "heart failure"
			names(data)[5] <- "pneumonia"

	#Get a vector of all of the states in 'data' now at column 2
	#--'Unique' returns a vector, data frame, or array but with duplicates removed
		states <- data[, 2] 
		states <- unique(states)
		if(state %in% states == FALSE) {
			stop("invalid state")
}

	#Get only the rows with our state value
		data <- data[data$state == state & data[outcome] != 'Not Available', ]
		vals <- data[, outcome]

	#RowNum = the index of the minimum value (lowest death rate)
		rowNum <- which.min(vals)

	#Return hospital name in that state with lowest 30-day death rate
		data[rowNum, ]$name
}

best("TX", "heart attack")




