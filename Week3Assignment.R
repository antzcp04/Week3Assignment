1. Write a function that takes a vector as input and returns the number of missing values in the vector.

v <- c("Anthony","Sheryl","","")
getVector <- function(v)  {
  vCount1 <- length(v)
  ctr <- 0
  
  for (i in 1:vCount1 ) {
    if ( v[i]==""  )  {
       ctr<- ctr + 1
        
    }
  }
  print(ctr)    
}


2. Write a function that takes a data frame as input and returns a named vector with the number of missing values in each column 
of the data frame. (The names of the entries should be the corresponding column names of the data frame.) You may use the function
from the previous question as part of your solution.


name <- c('Anthony','Sheryl','Sandee','Chris','Sam','Dennis','Danny','Tom','Christina','Abby')
age <- c(24, 30, 32, 30, 27, NA , 23, 25, 39 , NA )
Bio.data <- data.frame(name, age)

getVector <- function(f)  {
  vCount <- length(Bio.data$age)
  ctr <- 0 
  for (i in 1:vCount ) {
    if ( is.na(Bio.data$age[i]) )  {
      ctr<- ctr + 1
      
    }
  }
 print(ctr) 
    
}

 
3. Write a function that takes a numeric vector as input and uses it to determine the minimum, the maximum, the mean, the median, 
the first quartile, the third quartile, the standard deviation of the vector, and the number of missing values. Do not use any 
built-in functions to do this. Return a named list with the eight desired values in an order you deem best. (You may, if you like, 
                                                                                                             use the function you 
                                                                                                            wrote for question 1.)

Min, Max, mean, median, first quartile, third quartile, std, no of Missing Values

v <- c(4,2,3,1)
getVector <- function(v)  {
  vCount1 <- length(v)
  ctr <- 0
  
  newV <- sort(v, decreasing = FALSE)  
  
  # Min
  minContainer <- newV[1] 
  print(minContainer)
  
  # Max
  maxContainer <- newV[vCount1]
  print(maxContainer)
  
  #Mean
  meanContainer <- 0
  for (i in 1:vCount1 ) {
    meanContainer <-  meanContainer + newV[i]    
  }  
  meanValue <- ( meanContainer / vCount1 ) 
  print(meanContainer)
  
  #Median
  vMedian <- vCount1 / 2  
  medianContainer <- newV[vMedian]
  print(medianContainer)  
  
  #First Quartile
  vMedian <- vCount1 / 2  
  for (i in 1<vMedian ) {
    fQ1Container <-  fQ1Container + newV[i]    
  } 
  
  #third Quartile
  
  
  # Standard Deviation
  
  meanContainer <- 0
  for (i in 1:vCount1 ) {
    meanContainer <-  meanContainer + newV[i]    
  }  
  meanValue <- ( meanContainer / vCount1 ) 
  sumValue <- 0  
  for (i in 1:vCount1 ) {
    sumValue <- ( ( v[i] - vCount1 ) ^ 2 ) + sumValue  
  }
  varianceValue <-  ( 1 / vCount1)  * sumValue
  
  # of Missing Values
  ctr <- 0  
  for (i in 1:vCount1 ) {
    if ( v[i]==""  )  {
      ctr<- ctr + 1
      
    }
  }  
  print(ctr)  
  
}  


4. Write a function that takes a character or factor vector and determines the number of distinct elements in the vector, the most 
commonly occurring element, the number of times the most commonly occurring element occurs, and the number of missing values. (
  Be sure to handle ties gracefully.) Have the function return a named list with the desired information in a logical order.

getVector <- function(v)  {
  vCount1 <- length(v)
  # of Distinct Elements
  unique(v)
  
  # of commonly Occuring Elemnet
  names(sort(summary(as.factor(v)), decreasing=T)[1:3])
  
  # of Times the monst Commonly Occuring Element
  as.data.frame(table(v))
  
  # of Missing Values
  ctr <- 0  
  for (i in 1:vCount1 ) {
    if ( v[i]==""  )  {
      ctr<- ctr + 1
      
    }
  }  
  print(ctr) 
  
}  

5. Write a function that takes a logical vector and determines the number of true values, the number of false values, the 
proportion of true values, and the number of missing values. Have the function return a named list with the desired information 
in a logical order.


getVector <- function(v)  {
  vCount <- length(v)
   # Determines # of True Values
  ctrTrue <- 0 
  for (i in 1:vCount ) {
    if ( !is.na(v[i]) )  {
      ctrTrue<- ctrTrue + 1
      
    }
  }
  
  # Determines # of False Values
  ctrFalse <- 0 
  for (i in 1:vCount ) {
    if ( is.na(v[i]) )  {
      ctrFalse<- ctrFalse + 1
      
    }
  }
  
  #Proportional of True values
  portionTrue  = ctrTrue / vCoun
  
  # of Missing Values
  ctr <- 0  
  for (i in 1:vCount1 ) {
    if ( v[i]==""  )  {
      ctr<- ctr + 1
      
    }
  }  
  print(ctr) 
  
  
}

6. Write a function that takes as its input a data frame and returns a summary of its columns using the functions you write 
for questions 3-5. You may assume that all columns will be of the three types in those questions. You are expected to use the 
functions you have written in the previous questions, so you do not have to write them again by scratch. Return the desired 
information in a format that you deem best. (One suggestion would be a named list of lists, but I leave it to your judgment.)





