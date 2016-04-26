require(dplyr)
require(tidyr)
require(assertthat)


#takes in a structured list of lists:
#First: a list of dataframes
#Second: column indices for each dataframe to keep but not re-code
#Then each following list gives a common name for sets of columns
#And the columns to be summed from each.
#If just a single column index given, it'll just be re-named
#Spits out the recoded dataframes in a list
recode <- function(listz) {
  
  
  #Get number of dataframes to be coding to common names (to verify re-coding length values)
  df_num <- length(listz[[1]])
  
  #check all the recode columns are the correct length: name plus number of dataframes
  lens_mean <- mean( sapply(listz[c(3:length(listz))],function(x) length(x)) )
  #mean might plausibly be the same from getting it wrong several times...
  lens_sd <- sd( sapply(listz[c(3:length(listz))],function(x) length(x)) )
  #To give a sensible error message...
  column_recode_lists_are_all_correct_length <- (lens_mean == df_num + 1 & lens_sd == 0) 
  assert_that(column_recode_lists_are_all_correct_length == TRUE)
  
  #Used below in the dataframe loop... 
  #Takes in a string containing indices of columns to combine into one column
  #Will rename after.
  #Called from the df_index loop below so df_index'll be in scope
  reCodeCols <- function(colString){
    
    #columns to combine
    #select_ takes a string straight in...
    newcol <- listz[[1]][[df_index]] %>% select_(paste0("c(",colString,")") ) 
    #sum them...
    newcol <- data.frame(apply(newcol,1,sum))
    #return new column
    return(newcol)
    
  }
  
  #http://stackoverflow.com/a/26509347/5023561
  #Initialise a list to store processed dataframes
  dfs <- vector("list", df_num)
  
  #Iterate over each dataframe, create each in turn
  for(df_index in 1:(df_num)) {
    
    #Return a list with the values for the column recodings to pass to be recoded
    
    #Let's explain this in some detail in the hope it'll make sense later
    #It returns a list of new columns, each produced by the reCodeCols function above
    #The list that lapply iterates over is supplied by another lapply in the first arg
    #This pulls out the appropriate index from each new-variable list
    #E.g. if df_index is 1, it'll pull out the indices (in string form) for England/Scotland/Wales etc
    #For the first dataframe.
    #The result is a list of strings for re-coding columns
    #The reCodeCols function then uses df_index to apply the recoding/summing
    #to the correct dataframe
    newcols <- lapply(
      lapply(listz[c(3:length(listz))],function(x) x[[df_index + 1]]),
      function(x) reCodeCols(x)
    )
    #If this throws this error:
    #"Error in FUN(newX[, i], ...) : invalid 'type' (character) of argument"
    #The reCodeCol function's trying to sum non-numeric columns
    
    newcols <- data.frame(newcols)
    
    #Now rename the columns (common names stored in first index of re-coding lists)
    newnames <- sapply(listz[c(3:length(listz))],function(x) x[[1]])
    names(newcols) <- newnames
    
    #Combine with columns we want to keep
    #indicated in second list
    final <- listz[[1]][[df_index]] %>% select_(paste0("c(",listz[[2]][df_index],")") ) 
    
    dfs[[df_index]] <- cbind(final,newcols)
    
  }#end num_df loop
  
  
  #return list of re-coded dataframes
  return(dfs)
  
}#end function







#~~~~~~~~~~~~~
#Testybits----

# for(i in 1:num) dfs[[i]] <- data.frame(id = 1:nrow(listz[[1]][[i]]))
# 
# 
# test <- data.frame( select(listz[[1]][[1]],3,4,5,6) %>% apply(1,sum) ) 
# names(test) <- listz[[2]][[1]]
# 
# thingyo <- do.call(data.frame, list(test = 1:50), quote = T)

#Iterate over each column re-coding list (skipping first list containing the dataframes)
#   for(i in 2:length(listz)) {
# 
#     #Convert to nice easy to reference vector. Thx ta.    
#     commonName <- unlist(listz[[i]][1])
#     colRecodes <- unlist(listz[[i]])[2:length(listz[[i]])]
# 
#     #check it's got the right number of elements
#     #The code will break if the above aren't a name and integers anyway, so...
#     assert_that(length(colRecodes) == num)
#     
#     #So for this column recode
#     #Go through columns in each dataset in order
#     #Sum them into a new one, rename and stick in fresh dataframe
#     for(df_count in 1:num) {
#       
#       newc <- data.frame( newc = listz[[1]][[df_count]] )
#       
#     }
#     
#     
#   }

#return(0)




