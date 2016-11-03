library(ggplot2)
library(grid)
library(cowplot)

#'@export
frequency_table <- function(data_frame){
  # This function is a frequency table for every categorical and logical variable.

  # parameter: data_frame
  # type: any data frame

  # return: frequancy tables
  # type: list
  lapply(data_frame[,sapply(data,is.logical)],table) # do frequency table with logical var.
  lapply(data_frame[,sapply(data,is.factor)],table)  # do frequency table with factor var.
}

#'@export
summary_numeric <- function(data_frame){
  # This function is a summary statistics table for each numerical variable

  # parameter: data_frame
  # type: any data frame

  # return: summary table
  # type: list
  summary(data_frame[,sapply(data,is.numeric)])
}

#'@export
R_squared <- function(data_frame){
  # this function can accept any dataframe as a parameter and returns a dataframe
  # that contains each pair of column names in the first column in a single string
  # separated by a -, e.g. for the variables x and y, the string is “x-y”.
  # and calculate their corresponding r-square in the second column.

  # parameter: data_frame
  # type: any data frame

  # return: a two column data frame
  # type: data frame
  data_frame <- na.omit(data_frame) # omit any na
  data_frame <- data_frame[,sapply(data_frame,is.numeric)] # take out all numeric columns
  colna <- colnames(data_frame) # extract the column names
  pairwise_names <- c() # initial variable
  pairwise_r_square <- c() # initial variable
  for(i in 1:(length(colna)-1))
  {
    for(j in (i+1):length(colna))
    {
      # use for loop to calculate the r square for each pair
      temp <- summary(lm(data_frame[,i]~data_frame[,j]))$r.squared
      # paste each pair's name and add it to the old list
      pairwise_names <- c(pairwise_names,paste(colna[i],colna[j],sep="-"))
      # add each r square to the old list
      pairwise_r_square <- c(pairwise_r_square,temp)
    }
  }
  # create a data frame of the two list as its column variables and add its colnames
  new_dataframe <- data.frame(pairwise_names,pairwise_r_square)
  colnames(new_dataframe) <- c("Variable Pairs","R-square")
  return (new_dataframe)
}

#'@export
Pearson_coeff <- function(data_frame,threshold=0.5){
  # this function can accept any dataframe as a parameter and returns a dataframe
  # that contains each pair of column names in the first column in a single string
  # separated by a -, e.g. for the variables x and y, the string is “x-y”.
  # and calculate their corresponding Pearson correlation coefficient in the second column.
  # And choose correlation coefficient (Pearson) for all coefficients whose absolute value
  # is greater than the correlation threshold

  # parameter: data_frame (data frame)
  #            threshold (numeric) with default 0.5

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
      # If the abs of correlation is greater than threshold, put it into data frame
      if(abs(temp)>threshold){
        # paste each pair's name and add it to the old list
        pairwise_names <- c(pairwise_names,paste(colna[i],colna[j],sep="-"))
        # add each Pearson correlation to the old list
        pairwise_cor <- c(pairwise_cor,temp)
      }
    }
  }
  # create a data frame of the two list as its column variables and add its colnames
  new_dataframe <- data.frame(pairwise_names,pairwise_cor)
  colnames(new_dataframe) <- c("Variable Pairs","Pearson Exceeds Threshold")
  return (new_dataframe)
}

#'@export

plot_density_count <- function(data_frame,switch="off",vector=NULL){
  # This function works like this: If the plot switch parameter is “on” or “grid”,
  # then plot a pair of blue histograms with a vertical red line at the mean (one
  # using counts and the other density) for every numerical variable at each number
  # of bins integer specified in the bin vector parameter. If the plot switch is set
  # to “grid”, there should be a grid for each count-bin combination and a separate
  # grid for each density-bin size combination. For example, given 5 numeric variables
  # and a vector of three bin number integers, the function should generate 30 individual
  # plots or a total of 6 grid plots (with each grid plot containing 5 subplots).

  # parameters: data_frame (type:data frame)
  #             switch (type:char,range:"on""off""grid") with default "off"
  #             vector (type:vector,range:integer vectors) with default NULL

  # return: plots
  num <- data_frame[,sapply(data_frame,is.numeric)]    # extract numeric var from data frame

  # vector != NULL condition
  if(!is.null(vector)){
    # "on" condition
    if(switch == "on"){
      # use for loops to plot each pair of plots with different vars and different bins
      for(j in 1:length(vector)){
        for(i in 1:ncol(num)){
          # plot histogram for count and density
          p1 <- ggplot(num,aes(x=num[i]),color = "blue")+
            geom_histogram(fill="blue",bins = vector[j])+
            ggtitle(paste(colnames(num[i]),vector[j],sep=" bins="))+
            xlab(colnames(num[i]))+
            geom_vline(xintercept = mean(num[,i]),col="red")

          p2 <- ggplot(num,aes(x=num[i],..density..))+
            geom_histogram(fill="blue",bins = vector[j])+
            ggtitle(paste(colnames(num[i]),vector[j],sep=" bins="))+
            xlab(colnames(num[i]))+
            geom_vline(xintercept = mean(num[,i]),col="red")
          # create a new page
          grid.newpage()
          # split the screen into 2 parts.
          pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 8), "null"))))
          # add title into specific location
          title <- paste(colnames(num[i]),vector[j],sep=" bin=")
          grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
          # print plots in pairs
          print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
          print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
        }
      }
    }
    else{
      # "grid" condition
      if(switch == "grid"){
        # use for loops to plot each grid(in each grid,subplots has the same var.) of plots with different vars and different bins
        for(j in 1:length(vector)){
          plot_count <- list()     # initial a list for count-plots
          plot_density <-  list()  # initial a list for density-plots
          for(i in 1:ncol(num)){
            # add plots to the count list and density list
            plot_count[[i]] <- ggplot(num, aes_string(colnames(num[i]))) +
              geom_histogram(bins=vector[j],fill="blue")+
              geom_vline(xintercept=mean(num[,i]),color="red")+
              labs(title= paste(vector[j], "bins"))+
              xlab(colnames(num)[i])
            plot_density[[i]] <- ggplot(num, aes_string(colnames(num[i]))) +
              geom_histogram(aes(y=..density..),bins=vector[j],fill="blue")+
              geom_vline(xintercept=mean(num[,i]),color="red")+
              labs(title= paste(vector[j], "bins"))+
              xlab(colnames(num)[i])
          }
          # plot subplots into one plot
          print(plot_grid(plotlist=plot_count))
          print(plot_grid(plotlist=plot_density))
        }
      }
    }
  }

  # vector == NULL condition, use the default bins in ggplot
  else{
    if(switch == "on"){
      # use for loop to plot each pair of plots with different vars
      for(i in 1:ncol(num)){
        # plot histogram for count and density
        p1 <- ggplot(num,aes(x=num[i]),color = "blue")+
          geom_histogram(fill="blue")+
          xlab(colnames(num[i]))+
          geom_vline(xintercept = mean(num[,i]),col="red")

        p2 <- ggplot(num,aes(x=num[i],..density..))+
          geom_histogram(fill="blue")+
          xlab(colnames(num[i]))+
          geom_vline(xintercept = mean(num[,i]),col="red")
        # create a new page
        grid.newpage()
        # split the screen into 2 parts.
        pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 8), "null"))))
        # print plots in pairs
        print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
        print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
        }
      }
    else{
      # "grid" condition
      if(switch == "grid"){
        # use for loops to plot each grid(in each grid,subplots has the same var.) of plots with different vars and different bins
        plot_count <- list()     # initial a list for count-plots
        plot_density <-  list()  # initial a list for density-plots
        for(i in 1:ncol(num)){
          # add plots to the count list and density list
          plot_count[[i]] <- ggplot(num, aes_string(colnames(num[i]))) +
            geom_histogram(fill="blue")+
            geom_vline(xintercept=mean(num[,i]),color="red")+
            xlab(colnames(num)[i])
          plot_density[[i]] <- ggplot(num, aes_string(colnames(num[i]))) +
            geom_histogram(aes(y=..density..),fill="blue")+
            geom_vline(xintercept=mean(num[,i]),color="red")+
            xlab(colnames(num)[i])
          }
          # plot subplots into one plot
          print(plot_grid(plotlist=plot_count))
          print(plot_grid(plotlist=plot_density))
        }
      }
   }
}

#'@export

is.binary <- function(v) {
  #The is.binary function determines whether a vector is binary
  #Input: a vector
  #Output: TRUE or FALSE
  x <- unique(v)                    #check all the distinct and put those in a vector x
  length(x) - sum(is.na(x)) == 2L         #check to see if x only contains 2 distinct values
}
#'@export
plot_categorical <- function(data_frame,switch="off"){
  # This function works like this: if the plot switch parameter is “on” or “grid”,
  # plot a gray bar graph for every categorical and binary variable.

  # parameters: data_frame (type:data frame)
  #             switch (type:char,range:"on""off""grid") with default "off"

  # return: plots
  data_frame1 <- data_frame[,sapply(data_frame,is.factor)]
  data_frame2 <- data_frame[,sapply(data_frame,is.logical)]
  data_frame3 <- data_frame[,sapply(data_frame,is.binary)]
  new_data <- data.frame(data_frame1,data_frame2,data_frame3)
  # switch/on condition
  if(switch=="on"|switch=="grid"){
    for(i in 1:ncol(new_data)){
      # bar plot
      p <- ggplot(new_data,aes(x=new_data[,i]))+
        geom_bar(fill='gray')+
        xlab(colnames(new_data)[i])
      print(p)
    }
  }
}
#'@export
explore_1.0<- function(data_frame,switch="off",threshold=0.5,vector=NULL){
  # main function: do things all above with default values-swithch is "off",threshold is 0.5,
  #                bin_integer is the default bins of ggplot, which is 30.


  # parameters: data_frame (type:data frame)
  #             switch (type:char,range:"on""off""grid")
  #             threshold (type:integer,range:(0,1))
  #             vector (type:vector,range:integers)
  # return: r list

  # add outcome of each function into a list
  mylist <- list(Frequency = frequency_table(data_frame),
                 Summary = summary_numeric(data_frame),
                 R_squared = R_squared(data_frame),
                 Pearson_coeff = Pearson_coeff(data_frame,threshold),
                 Plot_histogram = plot_density_count(data_frame,switch,vector),
                 Plot_bar = plot_categorical(data_frame,switch))

  return(mylist)
}

#'@export
explore <- function(data_frame,switch="off", threshold=0.5, vector=NULL, ...) UseMethod("explore")
#'@export
explore.default <- function(data_frame,switch="off", threshold=0.5, vector=NULL, ...){
  # This function work with defensive conditions of explore_1.0

  # parameter: same as explore_1.0
  # return: explore_1.0 with fine parameter as inputs

  # omit the whole row if there are any nas
  data_frame <- na.omit(data_frame)

  # if the first parameter is not a dataframe, change it into a dataframe
  if(!is.data.frame(data_frame)){
    data_frame <- as.data.frame(data_frame)
  }

  # if the second parameter is not what we required, ask users to reinput it
  while(switch != "off" && switch != "on" && switch != "grid"){
    print("invalid input for switch")
    switch <- readline(prompt="Enter your option(off / on / grid): ")  #re-enter the input
  }

  # if the second parameter is not in [0,1], ask users to reinput it
  while(!is.numeric(threshold) || threshold < 0 || threshold >1 ){    #check to see if threshold is a valid input
    print("correlation threshold must be numeric and in range [0,1]")
    threshold <- as.numeric(readline(prompt="Enter your correlation threshold: "))   #re-enter the input
  }

  # check if bin vector is all numeric and all not less than 0, if so, ask users to reinput it
  if(!is.null(vector)){
    if(!is.numeric(vector)||(is.numeric(vector) && (TRUE %in% (vector <= 0)))){
      print("the bins vector must be numeric vector and not less than 0, please enter new bins one by one and press 'return' to finish")
      vector <- c()
      bin <- 1
      #input "return"  to finish loop
      while(bin != ""){
        #re-enter the bin vector
        bin <- readline(prompt="Enter the number of bins: ")->bin1
        bin1 <- as.numeric(bin1)
        vector <- c(vector, bin1)
      }
      vector <- na.omit(vector) #cancel the NA
    }

    # if the bin vector is not integer, round it
    if (!is.integer(vector)) {
      vector <- round(vector)
    }
  }
  x <- explore_1.0(data_frame,switch,threshold,vector)
  class(x) <- "explore"
  return(x)
}
#'@export
print.explore <- function(x, ...){
  cat("Frequency table for every categorical and logical variable:\n")
  print(x$Frequency)
  cat("\n")
  cat("Summary statistics table for each numerical variable:\n")
  print(x$Summary)
  cat("\n")
  cat("R-square of each pair of column:\n")
  print(x$R_squared)
  cat("\n")
  cat("Pearson correlation of each pair of column:\n")
  print(x$Pearson_coeff)
  cat("\n")
  cat("Histogram plots of each numeric variables:\n")
  print(x$Plot_histogram)
  cat("\n")
  cat("Bar plots of each categorical variables:\n")
  print(x$Plot_bar)
}

