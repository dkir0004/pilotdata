# Code from Ariel and Yusuke 

# live dangerously, get rid of pesky warnings
oldw <- getOption("warn")
options(warn = -1)

shhh <- suppressPackageStartupMessages # stops annoying warnings when loading libraries
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(MASS)
library(Matrix)
library(reshape2)
library(ape) # stats
library(vegan) # stats
library(RColorBrewer)
library(cocor)
library(DescTools)
library(reshape2)
library(grid)
library(ggplotify)



# read the csv data files into a dataframe
files = list.files(pattern="*.csv")
data = sapply(files, read.csv, simplify=FALSE) %>% bind_rows(.id = "id")

colnames(data)

# Select variables we need for analysis 
trial_vars<- c( "participant",
                "Colour_1", "Colour_2", "Colour1", "Colour2", 
                "similarity", "response_time", "catchnumber", "Ecc", "catchnumberprac", "catchresponse", "catchtrialorder", "screen_size_x","screen_size_y","viewerdistancecm", 'viewer_distance',"trialnumber","Ecc")

data <- subset(data, select = trial_vars)


# Select relevant trials for catch trials 

get.catchtrial.info <- function(df.catchtrialorder){
  info <- (unique(df.catchtrialorder)[2])
  info <- as.character(info) # convert to string
  info <- str_sub(info, 2, -2) # remove the square brackets
  info <- str_split(info, pattern = fixed(',')) # get a vector of the catch trials in string format
  info <- info[[1]]
  #print(info) # testing
  info <- as.numeric(info) # convert to numeric
  return(info)
}

add.catchtrial.info <- function(df){
  IDs <- unique(df$participant)
  colnames <- colnames(df)
  output.df <- df[FALSE,]
  for(ID in IDs){
    tempdf <- subset(df, participant == ID)
    catch.trials <- get.catchtrial.info(tempdf$catchtrialorder)
    tempdf$catch.trial <- ifelse(is.element(tempdf$trialnumber,catch.trials),TRUE,FALSE)
    #print(colnames(tempdf)) #testing
    output.df <- rbind(output.df,tempdf)
  }
  return(output.df)
  

data$catch.trials <- NA # need to add this here to make stuff work nicely later
test <- add.catchtrial.info(data)
  
}

# Check catch scores 
catch_trial_checker <- function(datadf){
  
  subjectlist <- sort(unique(test$participant))
  print("Catch scores")
  for (participant in subjectlist){
    subjectdf <- test[which(test$participant == participant),] 
    catch_trials <- subset(subjectdf, catch.trial == TRUE)
    catch_num = nrow(catch_trials)
    catch_correct = nrow(subset(catch_trials, catchnumber == catchresponse))
    
    print(paste("Subject",participant,":",catch_correct,"/",catch_num))
  }
}


# Screen parameters 

# Screen size function 

screen_size <- function(dftrials){
  
  dftrials<- subset(dftrials, !is.na(screen_size_x), !is.na(screen_size_y))

  width <- as.numeric(substr(as.character(dftrials$screen_size_x)[1],1,6))
  height <- as.numeric(substr(as.character(dftrials$screen_size_y)[1],1,6))
  
  # use pythagoras to just get the hypotenuse. Subjects have a fixed 16/9 aspect ratio so these are all comparable
  return(sqrt(width*width + height*height))
}

# View distance function 

view_distance <- function(datadf){
  return(as.numeric(substr(as.character(datadf$viewer_distance)[1],1,6)))
}

# Calculate screen parameters for each participant 

screen_parameters <- function(dftrials,individual=FALSE){
  
  subjectlist <- sort(unique(dftrials$participant))
  print("Screen Parameters")
  screen_fail = 0
  viewing_fail = 0
  for (participant in subjectlist){

    subjectdf <- dftrials[which(dftrials$participant == participant),] 
    
    
    screen_size <- round(screen_size(subjectdf)/10,1)
    viewing_distance <- round(view_distance(subjectdf)/10,1)
    
    if(screen_size < 20){screen_fail = screen_fail + 1}
    if(viewing_distance < 30){viewing_fail = viewing_fail + 1}
    
    if(individual){
      print(paste("Subject",participant,":"))
      print(paste("Screen size:",screen_size,"cm"))
      print(paste("Viewing distance:",viewing_distance,"cm"))
      print("")
    }
    
    
  }
  print("")
  print(paste("Screen size issues:",screen_fail,"/",length(subjectlist)))
  print(paste("Viewing distance issues:",viewing_fail,"/",length(subjectlist)))
}  


# Create data frame for trials 
dftrials <- subset(data, !is.na(Colour1))

# Label participant number from 1 - 15 
dftrials$ID <- NA
subjectlist <- unique(dftrials$participant)
k= 0
for (participant in subjectlist){
  k = k + 1
  dftrials$ID[dftrials$participant == participant] <- k
}

dftrials <- subset(data, !is.na(Colour1))

# Remove participant 10 
dftrials <- dftrials[which(dftrials$ID != 10),]


# Check average Response Time
rt_cutoff = 700 # mean reaction times must be above this

rt_avg <- function(data){
  return(median(data$response_time))
}


rt_avg_check <- function(dftrials){
  
  subjectlist <- sort(unique(dftrials$participant))
  print("RT avg")
  for (participant in subjectlist){
    subjectdf <- dftrials[which(dftrials$participant == participant),] 
    rt = rt_avg(subjectdf)
    print(paste("Subject:",participant,"mean rt",rt))
  }
}

rt_avg_check(dftrials)

# changing color values from RGB to hex for graphing purpose
dftrials$Colour1 <- as.character(dftrials$Colour1)
dftrials$Colour1 <- revalue(dftrials$Colour1, 
                            c(  "1" = '#FF0000',
                                "2" = '#FFAA00',
                                "3" = '#AAFF00',
                                "4" = '#00FF00',
                                "5" = '#00FFA9',
                                "6" = '#00A9FF',
                                "7" = '#0000FF',
                                "8" = '#AA00FF',
                                "9" = '#FF00AA'))
dftrials$Colour2 <- as.character(dftrials$Colour2)
dftrials$Colour2 <- revalue(dftrials$Colour2, 
                            c(  "1" = '#FF0000',
                                "2" = '#FFAA00',
                                "3" = '#AAFF00',
                                "4" = '#00FF00',
                                "5" = '#00FFA9',
                                "6" = '#00A9FF',
                                "7" = '#0000FF',
                                "8" = '#AA00FF',
                                "9" = '#FF00AA'))

# colors for the labels
# red, orange, yellow, green, cyan, cyan-blue, blue, purple, pink
colors <- c('#FF0000','#FFAA00','#AAFF00','#00FF00','#00FFA9','#00A9FF','#0000FF','#AA00FF','#FF00AA')
# can change the way the plot line up
# red, pink, orange, purple, yellow, blue, green, cyan-blue, cyan
#colors <- c('#FF0000','#FF00AA','#FFAA00','#AA00FF','#AAFF00','#0000FF','#00FF00','#00A9FF','#00FFA9')
abcolors <- sort(colors) # this was messing up the asymmetry plot, maybe useful for some other stuff

# changing from int indicators in the .csv file to more readable labels for eccentricity
foveal = -1
peripheral = 1

# set the maximum and minimum dissimilarity values for later analysis
min_val = 0
max_val = 6


trace_cutoff = 2 # mean dissimilarity for physically identical colours must be below this
antitrace_cutoff = 3.5 # mean dissimilarity accepted for maximally physically different colours must be above this
rt_cutoff = 700 # mean reaction times must be above this

exclude_noncompliant = FALSE

plotsubjects = FALSE
plot_within_between = FALSE
plotexpsummary = FALSE
across = FALSE
population = FALSE



# rainbowcloud theme for plotting, stolen from: 
# https://micahallen.org/2018/03/15/introducing-raincloud-plots/?utm_campaign=News&utm_medium=Community&utm_source=DataCamp.com
raincloud_theme = theme(
text = element_text(size = 10),
axis.title.x = element_text(size = 16),
axis.title.y = element_text(size = 16),
axis.text = element_text(size = 14),
axis.text.x = element_text(angle = 45, vjust = 0.5),
legend.title=element_text(size=16),
legend.text=element_text(size=16),
legend.position = "right",
plot.title = element_text(lineheight=.8, face="bold", size = 16),
panel.border = element_blank(),
panel.grid.minor = element_blank(),
panel.grid.major = element_blank(),
axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

# stealing ability to make flat violin plots
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")


# Similarity judgment histogram
simhistplot <- function(datadf){
    
   plot <- ggplot(dftrials, aes(x = similarity)) + geom_bar(aes(y = ..prop..)) +
    scale_x_discrete(limits=c(0,1,2,3,4,5,6,7), name = 'Dissimilarity') +
    ylab('Frequency') + ylim(0,0.8)
    return(plot)
}


simhistplot_summary <- function(datadf){
    
    datadf$subject <- as.character(datadf$subject) # necessary for visualisation
    
    plot <- ggplot(dftrials, aes(x = similarity)) + 
    geom_line(stat='count',aes(y = ..prop..,group = subject),color='#CC9933') +
    geom_line(stat='count',aes(y = ..prop..),size=1.5) +
    scale_x_discrete(limits=c(0,1,2,3,4,5,6,7), name = 'Dissimilarity') +
    ylab('Frequency') + ylim(0,0.8)
    return(plot)
    
}


# reaction time for each similarity

rsplot <- function(datadf){
    
    plot <- ggplot(dftrials, aes(x= similarity, y=response_time)) + 
    stat_summary(fun.y = mean, geom = "bar") + 
    stat_summary(fun.data = mean_se, geom = "errorbar", size =0.5, aes(width=0.5)) +
    scale_x_discrete(limits=c(0,1,2,3,4,5,6,7), name = 'Dissimilarity') + ylab('Reaction Time (s)') +
    theme(legend.position = "none") +
    ylim(0,4) # anyone taking more than 4 seconds has probably mindwandered
    
    return(plot)
}


rsplot_all <- function(data){
  subjectlist <- sort(unique(dftrials$ID))
  par(mfrow=c(3,5))
  for (ID in subjectlist){
    subjectdf <- dftrials[which(dftrials$ID==ID),]
    plot <- rsplot(subjectdf)
    return(plot)
  }
}




# correlation between reaction times and similarity judgements
# grouping at individual trial, individual participant, experiment or entire population level
rt_similarity_cor <- function(dftrials,level='participant'){
  
  if(level=="participant"){
    dftrials<- dftrials%>% 
      group_by(ID) %>% 
      mutate(rt_similarity_correlation = cor(similarity,response_time))
    dftrials <- aggregate(dftrials, by=list(dftrials$ID, FUN = mean))
    
    
  }
  return(dftrials)
  
}

rt_similarity_cor(dftrials,level='participant')

rt_similarity_cor <- function(datadf,level='participant'){
  
  if(level=="participant"){
    datadf <- datadf %>% 
      group_by(subject) %>% 
      mutate(rt_similarity_correlation = cor(similarity,response_time))
    datadf <- aggregate(datadf, by=list(datadf$subject), FUN = mean)
    
    
  }
  return(datadf)
  
}



rt_similarity_cor(dftrials,level='participant')

rt_similarity_plot <- function(dftrials,xlabel='BLANK'){
  
  datadf <- rt_similarity_cor(dftrials)
  
  datadf[xlabel] = xlabel
  
  plot <- ggplot(datadf,aes(x=xlabel,y=rt_similarity_correlation)) + 
    geom_boxplot() + 
    geom_dotplot(binaxis='y',stackdir='center',dotsize=0.75) +
    theme(text = element_text(size=15)) + xlab("")
  ggtitle(title)
  
  plot <- plot + ylab("Correlation (Spearman)") + ylim(-1,1)
  plot <- plot + geom_hline(yintercept=0, linetype="dashed", color = "blue")
  return(plot)
}

rt_similarity_plot(dftrials,xlabel='BLANK')

# reaction time raincloud plot
rsplot_raincloud <- function(datadf,xtype='linear'){
    
    datadf$subject <- as.character(datadf$subject) # necessary for visualisation  
    datadf$similarity <- as.character(datadf$similarity) # necessary for visualisation
    
    ylabtext = 'Reaction Time (ms)'
    
    plot <- ggplot(datadf, aes(y = response_time, x = similarity, fill = similarity)) +
            geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
            geom_point(aes(y = response_time, color = similarity),
                   position = position_jitter(width = .15), size = .5, alpha = 0.8) +
            geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
            expand_limits(x = 5.25) +
            guides(fill = FALSE) +
            guides(color = FALSE) +
            scale_color_brewer(palette = "Spectral") +
            scale_fill_brewer(palette = "Spectral") +
            xlab('Dissimilarity') + ylab("Reaction Time (ms)")
            # coord_flip() +
            theme_bw() +
            raincloud_theme
    
    if(xtype == 'log'){
        plot <- plot + scale_y_continuous(trans='log10')
    } else{
        plot <- plot + ylim(0,5000)
    }
    
    return(plot)
}

rsplot_raincloud(dftrials,xtype='linear')

# correlation between reaction times and similarity judgements
# grouping at individual trial, individual participant, experiment or entire population level
rt_similarity_cor <- function(datadf,level='participant'){
        
    if(level=="participant"){
        datadf <- datadf %>% 
                group_by(subject) %>% 
                mutate(rt_similarity_correlation = cor(similarity,response_time))
        datadf <- aggregate(datadf, by=list(datadf$subject), FUN = mean)

                
    }
    return(datadf)
    
}
rt_similarity_cor(dftrials,level='participant')

rt_similarity_plot <- function(datadf,xlabel='BLANK'){
    
    datadf <- rt_similarity_cor(datadf)
    
    datadf[xlabel] = xlabel
    
    plot <- ggplot(datadf,aes(x=xlabel,y=rt_similarity_correlation)) + 
                geom_boxplot() + 
                geom_dotplot(binaxis='y',stackdir='center',dotsize=0.75) +
                theme(text = element_text(size=15)) + xlab("")
                ggtitle(title)
    
    plot <- plot + ylab("Correlation (Spearman)") + ylim(-1,1)
    plot <- plot + geom_hline(yintercept=0, linetype="dashed", color = "blue")
    return(plot)
}

rt_similarity_plot(dftrials,xlabel='BLANK')


# subject info
sumplot <- function(datadf){
    
    # change ms to s, add the delay for each trial
    datadf$response_time <- ((datadf$response_time + 0.125*nrow(datadf)) / 1000)
    
    plot <- ggplot(datadf, aes(x=subject, y = response_time)) + 
    stat_summary(fun.y = sum, geom = "bar") + ylim(0,1000) +
    ylab('Response Time Total') + theme(axis.title.x=element_blank(), axis.text.x = element_text(size=20))
    
    return(plot)
}


# get median reaction time
rt_avg <- function(datadf){
    return(median(datadf$response_time))
}


# function to aggregate everyone's data together
aggregate_df <- function(datadf,dependent='color'){

    # aggregate everyone's data together for the matrices
    everyonedata <- aggregate(datadf, by=list(
        datadf$Color_1,
        datadf$Color_2,
        datadf$Circle_1,
        datadf$Circle_2,
        datadf$bin1,
        datadf$bin2
        ), FUN=mean, 
    )

    # correct the column names
    everyonedata$Color_1 <- everyonedata$Group.1
    everyonedata$Color_2 <- everyonedata$Group.2
    everyonedata$Circle_1 <- everyonedata$Group.3
    everyonedata$Circle_2 <- everyonedata$Group.4
    everyonedata$bin1 <- everyonedata$Group.5
    everyonedata$bin2 <- everyonedata$Group.6
    
    return(everyonedata)
}




# factor the dataframes for the plot function
dissimdata2 <- function(dftrials, colors){
    
    # refactor the levels so they can be plotted properly later if need be
    dftrials$Colour1 <- with(dftrials, factor(Colour1, levels = colors))
    dftrials$Colour2 <- with(dftrials, factor(Colour2, levels = colors))
    
    return(dftrials)
}

quantify_asymmetry <- function(dftrials){
    
    data <- dissimdata2(dftrials, colors)
    
    # aggregate over the remaining columns of interest
    nmdsdata <- aggregate(data, by = list(data$Colour1, data$Colour2),FUN=mean)
    nmdsdata$Colour1 <- nmdsdata$Group.1
    nmdsdata$Colour2 <- nmdsdata$Group.2

    nmdsdata = subset(nmdsdata, select = c("Colour1","Colour2","similarity"))  # get rid of unnecessary columns
    
    nmdsmatrix <- spread(nmdsdata, Colour1, similarity) # convert the dataframe to a matrix
    nmdsmatrix <- data.matrix(nmdsmatrix) # change first column from colour to number (just some label stuff) 
    nmdsmatrix <- nmdsmatrix[,-1] # get rid of the labels in the first column, it messes up the code
    nmdsmatrix[is.na(nmdsmatrix)] <- 0  # change NA to 0 so sum can be calculated.
    
    matdf <- as.data.frame(as.vector(abs(nmdsmatrix - t(nmdsmatrix)))) # calculate the asymmetry
    asymmery_value <- sum(matdf)/2 # need to divide by 2 to get rid of the duplicates

    return(asymmery_value)
}




# return a list of the asymmetrical values for each subject
asymValues_list2 <- function(datadf){
    
    subjectlist <- sort(unique(dftrials$ID)) # obtain a list of all the subjects
    
    asymValues_list <- vector() # array to store the values in
    
    for (ID in subjectlist){ # go through subject by subject
        subjectdf <-  dftrials[which(dftrials$ID == ID),] 
        # select the ID for subject of interest
        asymValues_list <- c(asymValues_list, quantify_asymmetry(subjectdf))
    }
    return(asymValues_list)
}


df2mat.full <- function(dftrials){
  
  
  # aggregate over the remaining columns of interest
  datadf <- aggregate(dftrials, by = list(dftrials$Colour1, dftrials$Colour2),FUN=mean)
  datadf$Colour1 <- datadf$Group.1
  datadf$Colour2 <- datadf$Group.2
  
  datadf = subset(datadf, select = c("Colour1","Colour2","similarity"))  # get rid of unnecessary columns
  datadf <- spread(datadf, Colour1, similarity)
  
  # convert the dataframe to a matrix
  datamatrix <- data.matrix(datadf)
  datamatrix <- datamatrix[,-1] # get rid of the labels in the first column, it messes up the code
  rownames(datamatrix) <- colnames(datamatrix)
  return(datamatrix)
  
}


# Dissimplot for all data

# Remove participant 10 as they did not understand the task


dissimplot_temporal <- function(subjectdf,colors,dependent='color'){
    
    # refine data using function "dissimdata2 "
    datatemp <- dissimdata2(subjectdf, colors)
    datatemp <- aggregate(datatemp, by = list(datatemp$Colour1, datatemp$Colour2),FUN=mean)
    
    plot <- ggplot(datatemp, aes(x = Group.1, y = Group.2)) +
    theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
                      axis.title.x = element_blank(), axis.title.y = element_blank(),
                      plot.title = element_text(hjust = 0.5))
    
    # stuff that's standard across plot types
        plot <- plot + geom_raster(aes(fill = similarity)) +
                labs(title = 'Presented - Response Screen') +
                scale_fill_gradientn(colours = c("white","black")) +
                guides(fill=guide_legend(title="Dissimilarity"))
    return(plot)
}



# Plot within subject response variance 

# Calculate variance in similarity within subject
datavar <-  group_by(dftrials, Colour1, Colour2, participant) %>% 
  summarise(
    count = n(), 
    varsim = var(similarity, na.rm = TRUE),
 )

# Calculate the mean variance across subjects
datavar  <-  group_by(datavar, Colour1, Colour2) %>% 
  summarise(
    count = n(), 
    varmean = mean(varsim, na.rm = TRUE),
  )

# Plot the mean variance accross subjects

plot <- ggplot(datavar, aes(x = Colour1, y = Colour2)) +
  theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

# stuff that's standard across plot types
plot <- plot + geom_raster(aes(fill = varmean)) +
  labs(title = 'Presented - Response Screen') +
  scale_fill_gradientn(colours = c("white",'orange')) +
  guides(fill=guide_legend(title="Variance"))
(plot)


# Variance plot for between subject variance 

# Calculate the mean similarity for each participant 

datavar1 <-  group_by(dftrials, Colour1, Colour2, participant) %>% 
  summarise(
    count = n(), 
    meansim = mean(similarity, na.rm = TRUE),
  )

# Calculate the mean variance across subjects
datavar1  <-  group_by(datavar1, Colour1, Colour2) %>% 
  summarise(
    count = n(), 
    varsim = var(meansim, na.rm = TRUE),
  )


plot <- ggplot(datavar1, aes(x = Colour1, y = Colour2)) +
  theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

# stuff that's standard across plot types
plot <- plot + geom_raster(aes(fill = varsim)) +
  labs(title = 'Presented - Response Screen') +
  scale_fill_gradientn(colours = c("white",'orange')) +
  guides(fill=guide_legend(title="Variance"))
(plot)




dissimplot_temporal <- function(subjectdf,colors,dependent='color'){
  
  # refine data using function "dissimdata2 "
  datatemp <- dissimdata2(subjectdf, colors)
  datatemp <- aggregate(datatemp, by = list(datatemp$Colour1, datatemp$Colour2),FUN=mean())
  
  plot <- ggplot(datatemp, aes(x = Group.1, y = Group.2)) +
    theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  # stuff that's standard across plot types
  plot <- plot + geom_raster(aes(fill = similarity)) +
    labs(title = 'Presented - Response Screen') +
    scale_fill_gradientn(colours = c("white","black")) +
    guides(fill=guide_legend(title="Dissimilarity"))
  return(plot)
}

dissimplot_temporal(dftrials,colors,dependent='color')

# CORRELATION BETWEEN PASSES 

pass_compare_list_Fisher <- function(dftrials){
  
  subjectlist <- sort(unique(dftrials$ID)) # obtain a list of all the subjects
  
  correlation_list <- vector() # array to store the values in
  
  for (ID in subjectlist){ # go through subject by subject
    subjectdf <-  dftrials[which(dftrials$ID == ID),]  # select the ID for subject of interest
    correlation_list <- c(correlation_list, FisherZ((matrixcor_pass(subjectdf))))
    
    plot <- plot(correlation_list, main = '1st and 2nd pass correlation - z',
                 xlab='Participant',ylab='z',xlim=c(1,14))
    axis <- axis(1,seq(1,14,1))
  }
  return(correlation_list)
  return(plot)
  return(axis)
}

matrixcor_pear <- function(dftrials){
  
  matrix1 <- df2mat.full(dftrials[which(dftrials$trialnumber<=162),])
  matrix2 <- df2mat.full(dftrials[which(dftrials$trialnumber>=163),])
  return(cor(c(matrix1), c(matrix2), method = "pearson"))
}

pass_compare_list_plot <- function(dftrials){
  
  subjectlist <- sort(unique(dftrials$ID)) # obtain a list of all the subjects
  
  correlation_list <- vector() # array to store the values in
  
  for (ID in subjectlist){ # go through subject by subject
    subjectdf <-  dftrials[which(dftrials$ID == ID),]  # select the ID for subject of interest
    correlation_list <- c(correlation_list, (matrixcor_pear(subjectdf)))
    
    plot <- plot(correlation_list, main = '1st and 2nd pass Pearson correlation - r',
                 xlab='Participant',ylab='r',xlim=c(1,14),pch = 21, col="black")
    axis <- axis(1,seq(1,14,1))
  }
  return(correlation_list)
  return(plot)
  return(axis)
}



# Plot a dissmiliarity matrix for all subjects 
dissimplot_temporal_subject <- function(dftrials, colors){
  
  IDs <- unique(dftrials$ID)
  plot_list <- list()
  
  for (ID in IDs){
  #Subset data for the subject
    
  subjectdf = dftrials[which(dftrials$ID == ID),] 
  
  # refine data using function "dissimdata2 "
  datatemp <- dissimdata2(subjectdf, colors)
  
  plot <- ggplot(datatemp, aes(x = Colour1, y = Colour2)) +
    theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5))+
    ggtitle(paste("Subject ID:", ID))
  
  
  # stuff that's standard across plot types
  plot <- plot + geom_raster(aes(fill = similarity)) +
    scale_fill_gradientn(colours = c("white","black")) +
    guides(fill=guide_legend(title="Dissimilarity"))
  
  plot_list[[ID]] <- plot

  }
  plot_grob <- arrangeGrob(grobs=plot_list)
  return(grid.arrange(plot_grob))
}




# Asymmtery matrix temporal


df2mat_asymmetry_temporal <- function(subjectdf){
    
    datatemp <- dissimdata2(subjectdf, colors)
    
    # aggregate over the remaining columns of interest
    nmdsdata <- aggregate(datatemp, by = list(datatemp$Colour1, datatemp$Colour2),FUN=mean)
    nmdsdata$Colour1 <- nmdsdata$Group.1
    nmdsdata$Colour2 <- nmdsdata$Group.2

    nmdsdata = subset(nmdsdata, select = c("Colour1","Colour2","similarity",'participant'))  # get rid of unnecessary columns
    nmdsmatrix <- spread(nmdsdata, Colour1, similarity) # convert the dataframe to a matrix
    nmdsmatrix <- data.matrix(nmdsmatrix) # change first column from colour to number(just some label stuff) 
    nmdsmatrix <- nmdsmatrix[,-1] # get rid of the labels in the first column, it messes up the code
    
    matdf<-  as.data.frame(nmdsmatrix - t(nmdsmatrix)) # calculate the asymmetry
    matdf$colorset <- c(colors) # adding additional column "colorset"
    num_colors <- length(colors)
    matdf <- matdf %>% gather(othercolor,asymmetry ,1:num_colors) # convert the matrix back to the data frame which has the 
                                                                  # column "colortset", "othercolor", "asymmetry"
    return(matdf)
}





# Create asymmetry dataframes for each subject 

IDs <- unique(dftrials$ID) # Create list of participants to loop through 

for (ID in IDs){
  #Subset data for the subject
  
  subjectdf = dftrials[which(dftrials$ID == ID),] 

datatemp <- dissimdata2(subjectdf, colors)
  
nmdsdata <- aggregate(datatemp, by = list(datatemp$Colour1, datatemp$Colour2),FUN=mean)
nmdsdata$Colour1 <- nmdsdata$Group.1
nmdsdata$Colour2 <- nmdsdata$Group.2
nmdsdata = subset(nmdsdata, select = c("Colour1","Colour2","similarity"))  # get rid of unnecessary columns
nmdsmatrix <- spread(nmdsdata, Colour1, similarity) # convert the dataframe to a matrix
nmdsmatrix <- data.matrix(nmdsmatrix) # change first column from colour to number(just some label stuff) 
nmdsmatrix <- nmdsmatrix[,-1] # get rid of the labels in the first column, it messes up the code
matdf <- as.data.frame(nmdsmatrix - t(nmdsmatrix)) # calculate the asymmetry
matdf$colorset <- c(colors) # adding additional column "colorset"
num_colors <- length(colors)
matdf <- matdf %>% gather(othercolor,asymmetry,1:num_colors)
matdf$ID <- ID # convert the matrix back to the data frame which has the 
  # column "colortset", "othercolor", "asymmetry"
assign(paste("matdf", ID, sep = ""), matdf) # Rename for the subject number 
}


# Create a data frame with all asymmetry data
matdfall <- rbind(matdf1, matdf2, matdf3, matdf4, matdf5, matdf6, matdf7, matdf8, matdf9, matdf11, matdf12, matdf13, matdf14, matdf15)


# Perform one paired t test for each asymmetry by

color1 <- unique(matdfall$colors) # Create list of colours to loop through

for (colors in color1){

#FF0000 

test1 <- t.test(matdfall$asymmetry[which(matdfall$colorset == colors & matdfall$othercolor =='#FF0000')], mu = 0)
print(paste("Colour #FF0000 and ", colors))
print(test1)

#FFAA00

test2 <- t.test(matdfall$asymmetry[which(matdfall$colorset == colors & matdfall$othercolor =='#FFAA00')], mu = 0)
print(paste("Colour #FFAA00 and ", colors))
print(test2)


#AAFF00 

test3 <- t.test(matdfall$asymmetry[which(matdfall$colorset == colors & matdfall$othercolor =='#AAFF00')], mu = 0)
print(paste("Colour #AAFF00  and ", colors))
print(test3)


#00FF00

test4 <- t.test(matdfall$asymmetry[which(matdfall$colorset == colors & matdfall$othercolor =='#00FF00')], mu = 0)
print(paste("Colour #00FF00 and ", colors))
print(test4)


#00FFA9 

test5 <- t.test(matdfall$asymmetry[which(matdfall$colorset == colors & matdfall$othercolor == '#00FFA9')], mu = 0)
print(paste("Colour #00FFA9  and ", colors))
print(test5)

#00A9FF 

test6 <- t.test(matdfall$asymmetry[which(matdfall$colorset == colors & matdfall$othercolor =='#00A9FF')], mu = 0)
print(paste("Colour #00A9FF and ", colors))
print(test6)


#0000FF 

test7 <- t.test(matdfall$asymmetry[which(matdfall$colorset == colors & matdfall$othercolor =='#0000FF')], mu = 0)
print(paste("Colour #0000FF and ", colors))
print(test7)


#AA00FF 

test8 <- t.test(matdfall$asymmetry[which(matdfall$colorset == colors & matdfall$othercolor =='#AA00FF')], mu = 0)
print(paste("Colour #AA00FF  and ", colors))
print(test8)

#FF00AA

test9 <- t.test(matdfall$asymmetry[which(matdfall$colorset == colors & matdfall$othercolor =='#FF00AA')], mu = 0)
print(paste("Colour #FF00AA and ", colors))
print(test9)
}

# Variance of asymmetry 

color1 <- unique(matdfall$colors) # Create list of colours to loop through

for (colors in color1){
  
#FF0000 

var1 <- var.test(matdfall$asymmetry[which(matdfall$colorset == colors & matdfall$othercolor =='#FF0000')], matdfall$asymmetry[which(matdfall$colorset != colors | matdfall$othercolor !='#FF0000')], alternative = "greater")
print(paste("Colour #FF0000 and ", colors))
print(var1)


#FFAA00

var2 <- var.test(matdfall$asymmetry[which(matdfall$colorset == colors & matdfall$othercolor =='#FFAA00')],matdfall$asymmetry[which(matdfall$colorset != colors | matdfall$othercolor !='#FFAA00')], alternative = "greater")
print(paste("Colour #FFAA00 and ", colors))
print(var2)

#AAFF00 

var3 <- var.test(matdfall$asymmetry[which(matdfall$colorset == colors & matdfall$othercolor =='#AAFF00')], matdfall$asymmetry[which(matdfall$colorset != colors | matdfall$othercolor !='#AAFF00')], alternative = 'greater')
print(paste("Colour #AAFF00 and ", colors))
print(var3)


#00FF00

var4 <- var.test(matdfall$asymmetry[which(matdfall$colorset == colors & matdfall$othercolor =='#00FF00')], matdfall$asymmetry[which(matdfall$colorset != colors | matdfall$othercolor !='#00FF00')], alternative = 'greater')
print(paste("Colour #00FF00 and ", colors))
print(var4)

#00FFA9 

var5 <- var.test(matdfall$asymmetry[which(matdfall$colorset == colors & matdfall$othercolor =='#00FFA9')], matdfall$asymmetry[which(matdfall$colorset != colors | matdfall$othercolor !='#00FFA9')], alternative = 'greater')
print(paste("Colour #00FFA9 and ", colors))
print(var5)

#00A9FF 

var6 <- var.test(matdfall$asymmetry[which(matdfall$colorset == colors & matdfall$othercolor =='#00A9FF')], matdfall$asymmetry[which(matdfall$colorset != colors | matdfall$othercolor !='#00A9FF')], alternative = 'greater')
print(paste("Colour #00A9FF and ", colors))
print(var6)


#0000FF 

var7 <- var.test(matdfall$asymmetry[which(matdfall$colorset == colors & matdfall$othercolor =='#0000FF')], matdfall$asymmetry[which(matdfall$colorset != colors | matdfall$othercolor !='#0000FF')], alternative = 'greater')
print(paste("Colour ##0000FF and ", colors))
print(var7)


#AA00FF 

var8 <- var.test(matdfall$asymmetry[which(matdfall$colorset == colors & matdfall$othercolor =='#AA00FF')], matdfall$asymmetry[which(matdfall$colorset != colors | matdfall$othercolor !='#AA00FF')], alternative = 'greater')
print(paste("Colour #AA00FF  and ", colors))
print(var8)

#FF00AA

var9 <- var.test(matdfall$asymmetry[which(matdfall$colorset == colors & matdfall$othercolor =='#FF00AA')], matdfall$asymmetry[which(matdfall$colorset != colors | matdfall$othercolor !='#FF00AA')], alternative = 'greater')
print(paste("Colour #FF00AA and ", colors))
print(var9)
}



# plot an asymmetry matrix for all data
asymmetry_plot_temporal <- function(subjectdf, colors){

  datatemp <- df2mat_asymmetry_temporal(subjectdf)
  
  # refactor the levels so they can be plotted properly later if need be
  datatemp$colorset <- with(datatemp, factor(colorset, levels = colors))
  datatemp$othercolor <- with(datatemp, factor(othercolor, levels = colors))
  
  plot <- ggplot(datatemp, aes(x = colorset, y = othercolor)) +
    theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          #axis.title.x = element_text("left"), axis.title.y = element_text("right"),
          plot.title = element_text(hjust = 0.5))
  
  # stuff that's standard across plot types
  plot <- plot + geom_raster(aes(fill = asymmetry)) +
    labs(title = 'Presented - Response Screen') +
    scale_fill_gradientn(colours = c("blue","white","red"), limits = c(-4,4)) +
    guides(fill=guide_legend(title="Dissimilarity\nAsymmetry"))
  return(plot)
}


# Plot variance of asymmetry 

asymvar <- aggregate(matdfall, by = list(matdfall$colors, matdfall$othercolor),FUN=var) # Calculate the variance of the asymmtery 

plot <- ggplot(asymvar, aes(x = Group.1, y = Group.2)) +
  theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
        axis.title.x = element_blank(), axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5))

# stuff that's standard across plot types
plot <- plot + geom_raster(aes(fill = asymmetry)) +
  labs(title = 'Presented - Response Screen') +
  scale_fill_gradientn(colours = c("white",'orange')) +
  guides(fill=guide_legend(title="Variance"))
(plot)



# Plot an asymmetry matrix for all subjects 
asymmetry_plot_temporal_subject <- function(dftrials, colors){
  
  IDs <- unique(dftrials$ID)
  plot_list <- list()
  
  for (ID in IDs){
    #Subset data for the subject
    
  subjectdf = dftrials[which(dftrials$ID == ID),] 
  datatemp <- df2mat_asymmetry_temporal(subjectdf)
  
  # refactor the levels so they can be plotted properly later if need be
  datatemp$colorset <- with(datatemp, factor(colorset, levels = colors))
  datatemp$othercolor <- with(datatemp, factor(othercolor, levels = colors))
  
  plot <- ggplot(datatemp, aes(x = colorset, y = othercolor)) +
    theme(axis.text.x = element_text(colour = colors), axis.text.y = element_text(colour = colors),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5))+
    ggtitle(paste("Subject ID:", ID))
  
  # stuff that's standard across plot types
  plot <- plot + geom_raster(aes(fill = asymmetry)) +
    scale_fill_gradientn(colours = c("blue","white","red"), limits = c(-4,4)) +
    guides(fill=guide_legend(title="Dissimilarity\nAsymmetry"))

  plot_list[[ID]] <- plot
  }
  plot_grob <- arrangeGrob(grobs=plot_list)
  return(grid.arrange(plot_grob))
}
  







