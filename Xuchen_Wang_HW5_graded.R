#Stuart- Good job throughout, but many of the problems were intended to be written as individual
#functions.

data('diamonds')
my_data <- diamonds
# assine data frame diamonds to my_data, use my_data as input in the following questions.

# 1
methods(class=data.frame) # see all methods of the type data frame
attributes(my_data) # see all atrributes of a data frame
str(my_data) # see all variables and their type in a data frame
ncol(my_data) # show the number of columns in a data frame

# 2
nrow(my_data) # show the number of rows in a data frame

# 3
writeLines(colnames(my_data)) # show the colnames of a data frame in mutiple lines

# 4
lapply(my_data, class) # apply class function to each column to show their type

# 5
lapply(my_data[,sapply(my_data,is.numeric)],mean) 
# firstly, use sapply function to judge if each column is numeric
# then, if it is true, extract it from the original data frame and replace the old one
# use lapply function to calculate the mean of each column in the new data frame

# 6
lapply(my_data[,sapply(my_data,is.factor)],table)
# firstly, use sapply function to judge if each column is factor
# then, if it is true, extract it from the original data frame and replace the old one
# use lapply function to calculate the frequency(use table function) of each column in the new data frame

# 7(1)
lapply(as.data.frame(lapply(my_data,is.na)),sum)
# firstly, use lapply function to judge if each column is na(use is.na function) and it returns a list.
# then, convert the list to a data frame in order to calculate the sum of na
# use lapply function to calculate the total na in each column(use sum function since true stands for 1)

# 7(2)
cat(c(100*nrow(my_data[rowSums(is.na(my_data))>0,])/nrow(mydata),'%'),seq='')
# firstly, use is.na to judge if there is any na in the data frame and it returns a data frame with true and false
# then, if there are any na in each row(rowSums(true) >0), extract it and replace the old data frame
# calculate the row numbers of the new data frame and divided by the total row numbers
# use cat function to show the outcome in the form of percentage

# 8
Pearson_coeff <- function(data_frame){
  # this function can accept any dataframe as a parameter and returns a dataframe 
  # that contains each pair of column names in the first column in a single string
  # separated by a -, e.g. for the variables x and y, the string is “x-y”.
  # and calculate their corresponding Pearson correlation coefficient in the second column.
  
  # parameter: data_frame
  # type: any data frame
  
  # return: a two column data frame
  # type: data frame
  data_frame <- na.omit(data_frame) # omit any na
  data_frame <- data_frame[,sapply(data_frame,is.numeric)] # take out all numeric columns
  colna <- colnames(data_frame) # extract the column names
  pairwise_names <- c() # initial variable
  pairwise_cor <- c() # initial variable
  for(i in 1:(length(colna)-1))
  {
    for(j in (i+1):length(colna))
    {
      # use for loop to calculate the Pearson correlation for each pair
      temp <- cor(data_frame[,i],data_frame[,j],method="pearson")
      # paste each pair's name and add it to the old list
      pairwise_names <- c(pairwise_names,paste(colna[i],colna[j],sep="-"))
      # add each Pearson correlation to the old list
      pairwise_cor <- c(pairwise_cor,temp)
    }
  }
  # create a data frame of the two list as its column variables
  return (data.frame(pairwise_names,pairwise_cor))
}

# check
Pearson_coeff(diamonds)


# 9
scatter_plot <- function(data,pearson_coeff){
  # This function create and label a scatter plot for every pair of numeric variables. 
  # Add a title to the plot that contains the combined name of the pair from problem 8
  # and the calculated Pearson correlation coefficient of the pair.
  
  # parameter: 
  # data: any data frame  
  # type: data frame
  #  
  # pearson_coeff: a two column data frame, the first column is a string of pairs,
  #                the second is Pearson correlation (the return of problem 8)
  # type: data frame
  
  # return: scatter plot of each pair
  data <- na.omit(data) # omit any na
  data <- data[,sapply(data, is.numeric)] # extract numeric columns
  names <- colnames(data) # column names
  combos <- combn(names, 2) # choose two variables from all column names as one combination
  
  for (i in 1:nrow(pearson_coeff)){
    # use for loop to plot each pair
    p <- ggplot(data, aes(x = data[,combos[1,i]], y = data[,combos[2,i]])) + 
      geom_point(size = 0.3,alpha = 0.5) + 
      ggtitle(paste(pearson_coeff[i,1], pearson_coeff[i,2], sep = '  r = ')) +
      xlab(combos[1,i])+ylab(combos[2,i])
    # use ggplot, aes x is the first var. in the ith combinations, aes y is the second var. in the ith combination
    # use geom_point to draw the point and set its size
    # use ggtitle to add a title of the string, such 'x-y' and the corresponding pearson correlation
    # use xlab and ylab to label the aes
    print(p)
  }
}
# check
scatter_plot(diamonds,Pearson_coeff(diamonds))

