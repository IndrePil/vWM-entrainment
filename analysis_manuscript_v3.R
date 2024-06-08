# this code was used to perform analysis reported in [REF] paper,
# as well as to generate all related figures.

# The analysis steps roughly follows the order in which the results (or figures) were presented.

#==================================
#=========== One sample t test ======
#====================================
library(tidyverse)
library(ggplot2)
library(plyr)
library(meta)
library(ez)
library(lsr)
library(car)
library(seewave)
library(data.table)

# data path
setwd("C:\\Users\\IndrePil\\Documents\\vWM Entrainment\\paper vWM entrainment\\data_files\\")

# read in data files
dV_all <- read.table("Exp1_allRespTable.csv", header=TRUE, sep=",", dec='.')
dBB1_all <- read.table("Exp2_allRespTable.csv", header=TRUE, sep=",", dec='.')
dMB_all <- read.table("Exp3_allRespTable.csv", header=TRUE, sep=",", dec='.')
dBB2_all <- read.table("Exp4_allRespTable.csv", header=TRUE, sep=",", dec='.')
dBB3_all <- read.table("Exp5_allRespTable.csv", header=TRUE, sep=",", dec='.')
dOn_all <- read.table("Exp6_allRespTable.csv", header=TRUE, sep=",", dec='.')



# summary data files, K values already calculated per condition
dV <- read.table("Exp1_summaryTable.csv", header=TRUE, sep=",", dec='.') 
dBB1 <- read.table("Exp2_summaryTable.csv", header=TRUE, sep=",", dec='.') 
dMB <- read.table("Exp3_summaryTable.csv", header=TRUE, sep=",", dec='.') 
dBB2 <- read.table("Exp4_summaryTable.csv", header=TRUE, sep=",", dec='.') 
dBB3 <- read.table("Exp5_summaryTable.csv", header=TRUE, sep=",", dec='.') 
dOn <- read.table("Exp6_summaryTable.csv", header=TRUE, sep=",", dec='.') 

# go back to analysis folder
setwd("C:\\Users\\IndrePil\\Documents\\vWM Entrainment\\paper vWM entrainment\\analysis_files\\")

  
#---- exclude outliers, get data ---------
# NOTE: identification of outliers was performed for each experiment separately, before this joint analysis.

# dV 
out <- c('S09', 'S19')
dV_all <- filter(dV_all, !ID %in% out)
dV <- filter(dV, !ID %in% out)

# dMB
out <- c('S06', 'S22')
dMB_all <- filter(dMB_all, !ID %in% out)
dMB <- filter(dMB, !ID %in% out)

# dBB1
out <- c('S08','S16','S18','S29','S21','S22','S23')
#out <- c("S07", "S08", "S09", "S12", "S15", "S16", "S18", "S21", "S22", "S23", "S24", "S25", "S26", "S27", "S30", "S31", "S33", "S42", "S46")
dBB1_all <- filter(dBB1_all, !ID %in% out)
dBB1 <- filter(dBB1, !ID %in% out)

# dBB2
out <- c('S05','S07','S18')
dBB2_all <- filter(dBB2_all, !ID %in% out)
dBB2 <- filter(dBB2, !ID %in% out)

# dBB3
out <- c('S03', 'S06', 'S22')
dBB3_all <- filter(dBB3_all, !ID %in%  out)
dBB3 <- filter(dBB3, !ID %in%  out)

# dOn
out <- c(3,7,9,17,18,21,23,26,28,32,34,35,36,41,50,57,61,62,70,73,74,77,82,83,85,93,98,99)
dOn_all <- filter(dOn_all, !ID %in% out)
dOn <- filter(dOn, !ID %in% out)

# Split into low and high performers

# visual
k <- ddply(dV[which(dV$frequency == 0),], ~ID, summarise, mean_k = mean(K))
k$performer <- k$ID

k[which(k$mean_k < median(k$mean_k)), 'performer'] <- 'low'
k[which(k$mean_k >= median(k$mean_k)), 'performer'] <- 'high'

dV$performer <- rep(k$performer, each = 15)

# MB
k <- ddply(dMB[which(dMB$frequency == 0),], ~ID, summarise, mean_k = mean(K))
k$performer <- k$ID

k[which(k$mean_k < median(k$mean_k)), 'performer'] <- 'low'
k[which(k$mean_k >= median(k$mean_k)), 'performer'] <- 'high'

dMB$performer <- rep(k$performer, each = 15)

# BB1
k <- ddply(dBB1[which(dBB1$frequency == 0),], ~ID, summarise, mean_k = mean(K))
k$performer <- k$ID

k[which(k$mean_k < median(k$mean_k)), 'performer'] <- 'low'
k[which(k$mean_k >= median(k$mean_k)), 'performer'] <- 'high'

dBB1$performer <- rep(k$performer, each = 9)

# BB2
k <- ddply(dBB2[which(dBB2$frequency == 0),], ~ID, summarise, mean_k = mean(K))
k$performer <- k$ID

k[which(k$mean_k < median(k$mean_k)), 'performer'] <- 'low'
k[which(k$mean_k >= median(k$mean_k)), 'performer'] <- 'high'

dBB2$performer <- rep(k$performer, each = 9)

# BB3
k <- ddply(dBB3[which(dBB3$frequency == 0),], ~ID, summarise, mean_k = mean(K))
k$performer <- k$ID

k[which(k$mean_k < median(k$mean_k)), 'performer'] <- 'low'
k[which(k$mean_k >= median(k$mean_k)), 'performer'] <- 'high'

dBB3$performer <- rep(k$performer, each = 9)

#Online
k <- ddply(dOn[which(dOn$frequency == 0),], ~ID, summarise, mean_k = mean(K))
k$performer <- k$ID

k[which(k$mean_k < median(k$mean_k)), 'performer'] <- 'low'
k[which(k$mean_k >= median(k$mean_k)), 'performer'] <- 'high'

dOn$performer <- rep(k$performer, each = 9)

# Joint Experiments, 4 Hz and 7 hz separated

d_joint <- data.frame('frequency' = c(dBB1$frequency,dBB2$frequency, dBB3$frequency, dV$frequency, dMB$frequency, dOn$frequency),
                      'K' = c(dBB1$K,dBB2$K, dBB3$K, dV$K, dMB$K, dOn$K),
                      'K_bsl' =  c(dBB1$K_bsl,dBB2$K_bsl, dBB3$K_bsl, dV$K_bsl, dMB$K_bsl, dOn$K_bsl),
                      'dPrime' = c(dBB1$dPrime,dBB2$dPrime, dBB3$dPrime, dV$dPrime, dMB$dPrime, dOn$dPrime),
                      'criterion' = c(dBB1$criterion,dBB2$criterion, dBB3$criterion, dV$criterion, dMB$criterion, dOn$criterion),
                      'HR'= c(dBB1$HR,dBB2$HR, dBB3$HR, dV$HR, dMB$HR, dOn$HR),
                      'FAR'  = c(dBB1$FAR,dBB2$FAR, dBB3$FAR, dV$FAR, dMB$FAR, dOn$FAR),
                      'Acc' = c(dBB1$Acc,dBB2$Acc, dBB3$Acc, dV$Acc, dMB$Acc, dOn$Acc),
                      'RT' = c(dBB1$RT,dBB2$RT, dBB3$RT, dV$RT, dMB$RT, dOn$RT),
                      'original_ID' =  c(dBB1$ID,dBB2$ID, dBB3$ID, dV$ID, dMB$ID, dOn$ID),
                      'Load' = c(dBB1$Load,dBB2$Load, dBB3$Load, dV$Load, dMB$Load, dOn$load),
                      'performer' = c(dBB1$performer,dBB2$performer, dBB3$performer, dV$performer, dMB$performer, dOn$performer),
                      'Experiment' = c( rep('Binaural (i)', length(dBB1$frequency)),
                                        rep('Binaural (ii)', length(dBB2$frequency)),
                                        rep('Binaural (iii)', length(dBB3$frequency)),
                                        rep('Visual', length(dV$frequency)),
                                        rep('Monaural (i)', length(dMB$frequency)),
                                        rep('Monaural (ii)', length(dOn$frequency))))

d_joint$unique_ID <- c(rep(1:(nrow(dBB1)/9), each = 9),
                       rep(42:(41+nrow(dBB2)/9), each = 9),
                       rep(60:(59+nrow(dBB3)/9), each = 9),
                       rep(80:(79+nrow(dV)/15), each = 15),
                       rep(119:(118+nrow(dMB)/15), each = 15),
                       rep(139:(138+nrow(dOn)/9), each = 9))



cols <- c('frequency', 'unique_ID', 'Experiment', 'Load', 'performer')
d_joint[cols] <- lapply(d_joint[cols], as.factor)

d_joint$condition <- d_joint$frequency
levels(d_joint$condition)
levels(d_joint$condition) <- c('control', 'entr', 'entr')



#================ function to calculate K for joined 4 and 7 Hz ==============

d_all <- data.table(dBB1_all)

#d_all$loads <- as.factor(d_all$loads)


k_entr <- function(d_all){
  
  ids <- unique(d_all$ID)
  Myloads <- sort(unique(d_all$loads))
  #loads <- as.numeric(as.character(unique(d_all$loads)))
  d_cond <- data.frame(ID = c(),
                       loads = c(),
                       K = c(),
                       K_bsl = c(),
                       condition = c())
  
  for(i in 1:length(ids)){ 
    #d_temp <- filter(d_test, ID == ids[i]) 
    d_0 <- filter(d_all, ID == ids[i] & frequency == 0)
    
    for(l in 1:length(Myloads)){
      d_0_load <- filter(d_0, loads == Myloads[l])
      
      HR <- nrow(d_0_load[which(d_0_load$resp_type == 1),])/
        nrow(d_0_load[which(d_0_load$change == 1),])
      
      FAR <- nrow(d_0_load[which(d_0_load$resp_type == 3),])/
        nrow(d_0_load[which(d_0_load$change == 0),])
      
      K <- as.numeric(Myloads[l])*((HR-FAR)/(1-FAR))
      
      row <- data.frame(ID = ids[i], 
                        loads = Myloads[l], 
                        K = K, 
                        K_bsl = 0,
                        condition = 'control')
      
      d_cond <- rbind(d_cond, row)
      
    } 
    
    
    d_temp <- filter(d_all, ID == ids[i] & d_all$frequency != 0) 
    
    for(l in 1:length(Myloads)){
      d_temp_load <- filter(d_temp, loads == Myloads[l])
      
      HR <- nrow(d_temp_load[which(d_temp_load$resp_type == 1),])/
        nrow(d_temp_load[which(d_temp_load$change == 1),])
      
      FAR <- nrow(d_temp_load[which(d_temp_load$resp_type == 3),])/
        nrow(d_temp_load[which(d_temp_load$change == 0),])
      
      K <- (Myloads[l])*((HR-FAR)/(1-FAR))
      
      K_bsl_0 <- d_cond[which(d_cond$ID == ids[i] & 
                                d_cond$condition == 'control' & 
                                d_cond$loads == Myloads[l]), 'K']
      
      
      K_bsl <- (K - K_bsl_0)/K_bsl_0
      
      row <- data.frame(ID = ids[i], 
                        loads = Myloads[l], 
                        K = K, 
                        K_bsl = K_bsl,
                        condition = 'entr')
      
      d_cond <- rbind(d_cond, row)
      
    } 
    
  }
  return(d_cond)
}

# online experiment separate (diff data structure)
k_entr_online <- function(d_all){
  
  ids <- unique(d_all$ID)
  Myloads <- as.numeric(as.character(unique(d_all$load)))
  d_cond <- data.frame(ID = c(),
                       loads = c(),
                       K = c(),
                       K_bsl = c(),
                       condition = c())
  
  for(i in 1:length(ids)){ 
    #d_temp <- filter(d_test, ID == ids[i]) 
    d_0 <- filter(d_all, ID == ids[i] & d_all$frequency == 0)
    
    for(l in 1:length(Myloads)){
      d_0_load <- filter(d_0, load == Myloads[l])
      
      HR <- nrow(d_0_load[which(d_0_load$respType == 'Hit'),])/
        nrow(d_0_load[which(d_0_load$change == 1),])
      
      FAR <- nrow(d_0_load[which(d_0_load$respType == 'FA'),])/
        nrow(d_0_load[which(d_0_load$change == 0),])
      
      K <- as.numeric(Myloads[l])*((HR-FAR)/(1-FAR))
      
      row <- data.frame(ID = ids[i], 
                        loads = Myloads[l], 
                        K = K, 
                        K_bsl = 0,
                        condition = 'control')
      
      d_cond <- rbind(d_cond, row)
      
    } 
    
    
    d_temp <- filter(d_all, ID == ids[i] & d_all$frequency != 0) 
    
    for(l in 1:length(Myloads)){
      d_temp_load <- filter(d_temp, load == Myloads[l])
      
      HR <- nrow(d_temp_load[which(d_temp_load$respType == 'Hit'),])/
        nrow(d_temp_load[which(d_temp_load$change == 1),])
      
      FAR <- nrow(d_temp_load[which(d_temp_load$respType == 'FA'),])/
        nrow(d_temp_load[which(d_temp_load$change == 0),])
      
      K <- as.numeric(Myloads[l])*((HR-FAR)/(1-FAR))
      
      K_bsl_0 <- d_cond[which(d_cond$ID == ids[i] & 
                                d_cond$condition == 'control' & 
                                d_cond$loads == Myloads[l]), 'K']
      
      
      K_bsl <- (K - K_bsl_0)/K_bsl_0
      
      row <- data.frame(ID = ids[i], 
                        loads = Myloads[l], 
                        K = K, 
                        K_bsl = K_bsl,
                        condition = 'entr')
      
      d_cond <- rbind(d_cond, row)
      
    } 
    
  }
  return(d_cond)
}


#extract values

dBB1_sum <- k_entr(dBB1_all)
dBB2_sum <- k_entr(dBB2_all)
dBB3_sum <- k_entr(dBB3_all)
dMB_sum <- k_entr(dMB_all)
dV_sum <- k_entr(dV_all)
dOn_sum <- k_entr_online(dOn_all)


# All experiments, 4 and 7 Hz merged

d_joint_entr <- rbind(dBB1_sum, dBB2_sum, dBB3_sum, dV_sum, dMB_sum, dOn_sum)

d_joint_entr$unique_ID <- c(rep(1:(nrow(dBB1_sum)/6), each = 6),
                            rep(42:(41+nrow(dBB2_sum)/6), each = 6),
                            rep(60:(59+nrow(dBB3_sum)/6), each = 6),
                            rep(80:(79+nrow(dV_sum)/6), each = 6),
                            rep(119:(118+nrow(dMB_sum)/6), each = 6),
                            rep(139:(138+nrow(dOn_sum)/6), each = 6))
                           

d_joint_entr$Experiment <- c(rep('Binaural (i)', nrow(dBB1_sum)),
                             rep('Binaural (ii)', nrow(dBB2_sum)),
                             rep('Binaural (iii)', nrow(dBB3_sum)),
                             rep('Visual', nrow(dV_sum)),
                             rep('Monaural (i)', nrow(dMB_sum)),
                             rep('Monaural (ii)', nrow(dOn_sum)))
                        


d_final <- ddply(d_joint_entr, ~unique_ID* condition*Experiment, summarise, mean = mean(K_bsl)) 

d_final <- filter(d_final, condition == 'entr')


# color palette
palette.colors(palette = "Okabe-Ito")
myColors = c("black", "#DF536B", "#61D04F", "#2297E6", "#28E2E5", "#CD0BBC", "#F5C710", "gray62")

 
 
 
 #========================
 # forest plots 
 #========================
 
 
 d_joint$frequency <- as.factor(d_joint$frequency)
d_joint$K_bsl <-  d_joint$K_bsl*100

 # 4 vs 7
 # function to do multiple t-tests
 t.paired.test.plyr <- function(x, var){
   f4 <- x[which(x$frequency == 4), var]
   f7 <- x[which(x$frequency == 7), var]
   y <- rep(NA,10)
   y[6] <- nrow(x)[1]              # count observations
   if(nrow(x) < 2) return(y)       # exits if too less observations
   res <- t.test(f4, f7, paired = TRUE, alternative = "two.sided" )  # doing the test
   
   y[1] <- res$statistic           # extract values of interest
   y[2] <- res$p.value      
   y[3] <- res$estimate     
   y[4] <- res$conf.int[1]  
   y[5] <- res$conf.int[2]  
   y[7] <- res$parameter    
   y[8] <- res$method       
   y[9] <- res$alternative  
   y[10] <- res$null.value   
   
   names(y) <- c("statistic","p.value","estimate","conf.int1", "conf.int2", "nobs","dof","method","alternative","null.value")
   y 
 }
 
 all_K <- ddply(d_joint, ~Experiment*original_ID*frequency, summarize, mean = mean(K_bsl), sd = sd(K_bsl))

 all_K <- all_K[which(all_K$Experiment != 'Monaural 3'),]
 
 ttest.result <- ddply(all_K, .(Experiment), t.paired.test.plyr, "mean")
 ttest.result
 
 # function for standard errors and weights
 se.test <- function(df, var){
   y <- rep(NA, 5)
   y[1] <- length(unique(df$original_ID)) # n of observations
   y[2] <- mean(df[which(df$frequency == 4), var]) - mean(df[which(df$frequency == 7), var]) # mean
   #if(nrow(df) < 2) return(0) 
   y[3] <-  sd((df[which(df$frequency == 4), var]) - (df[which(df$frequency == 7), var])) # sd
   y[4] <- y[3]/(sqrt(y[1])) # se
   y[5] <- 1/(y[4]^2) # weight
   names(y) <- c("nobs","mean", "sd", "se", "weight")
   y
 }
 
 
 sd.and.se <- ddply(all_K, .(Experiment), se.test, "mean")
 
 m.gen <- metagen(TE = sd.and.se$mean,
                  seTE = sd.and.se$se,
                  studlab = sd.and.se$Experiment,
                  data = sd.and.se,
                  sm = "MD",
                  fixed = TRUE,
                  random = FALSE,
                  method.tau = "REML",
                  hakn = TRUE,
                  title= 'test')
 summary(m.gen)
 m.gen <- update(m.gen, prediction = TRUE)
 summary(m.gen)
 
 forest(m.gen, 
             sortvar = c(2, 4, 5, 3, 6, 1),
             prediction = TRUE,
             colgap.forest.left = "3cm",
             label.left = "favors 7 Hz",
             label.right = "favors 4 Hz",
             leftcols = c("studlab"),
             print.tau2 = TRUE,
             col.square = c("black", "#DF536B", "#61D04F", "#2297E6", "#28E2E5", "#CD0BBC"),
             col.study = c("black", "#DF536B", "#61D04F", "#2297E6", "#28E2E5", "#CD0BBC"),
             addrow.overall = FALSE,
             lwd = 3,
             print.I2 = TRUE,
             hetstat = TRUE,
             fontsize = 11,
             spacing = 1.5,
             leftlabs = c("Experiment", "MD", "SE"),
             rightlabs = FALSE,
             at = c(-15, -10, -5, 0, 5, 10, 15)
        
 )
 # exported at 800 x 400 png
 #=========================================
 # one sample t test difference forest plot
 #========================================
 d_final$mean <- d_final$mean*100
 
 # function to do multiple t-tests
 t.test.plyr <- function(x, var, mean=0 ){
   y <- rep(NA,10)
   y[6] <- nrow(x)[1]              # count observations
   if(nrow(x) < 2) return(y)       # exits if too less observations
   res <- t.test(x[var], mu=mean, alternative = "two.sided")  # doing the test
   
   y[1] <- res$statistic           # extract values of interest
   y[2] <- res$p.value      
   y[3] <- res$estimate     
   y[4] <- res$conf.int[1]  
   y[5] <- res$conf.int[2]  
   y[7] <- res$parameter    
   y[8] <- res$method       
   y[9] <- res$alternative  
   y[10] <- res$null.value   
   
   names(y) <- c("statistic","p.value","estimate","conf.int1", "conf.int2", "nobs","dof","method","alternative","null.value")
   y 
 }
 
 # get collapsed (by load, frequency) data frame, so DFs are correct
 all_K <- ddply(d_final, ~Experiment*unique_ID*condition, summarize, mean = mean(mean), sd = sd(mean))
 all_K <- all_K[which(all_K$Experiment != 'Monaural 3'),]
 
 
 # store all ttest results in one variable
 ttest.result <- ddply(all_K[which(all_K$condition == 'entr'),], .(Experiment), t.test.plyr, "mean")
 ttest.result
 
 # function for standard errors and weights
 se.test <- function(df, var){
   y <- rep(NA, 5)
   y[1] <- nrow(df)[1] # n of observations
   y[2] <- mean(df[var][[1]]) # mean
   #if(nrow(df) < 2) return(0) 
   y[3] <- sd(df[var][[1]]) # sd
   y[4] <- y[3]/(sqrt(y[1])) # se
   y[5] <- 1/(y[4]^2) # weight
   names(y) <- c("nobs","mean", "sd", "se", "weight")
   y
 }
 
 
 sd.and.se <- ddply(all_K[which(all_K$condition == 'entr'),], .(Experiment), se.test, "mean")
 
 m.gen <- metagen(TE = sd.and.se$mean,
                  seTE = sd.and.se$se,
                  studlab = sd.and.se$Experiment,
                  data = sd.and.se,
                  sm = "MD",
                  fixed = TRUE,
                  random = FALSE,
                  method.tau = "REML",
                  hakn = TRUE,
                  title= 'test')
 summary(m.gen)
 m.gen <- update(m.gen, prediction = TRUE)
 summary(m.gen)
 # 
 
 
 forest(m.gen, 
        sortvar = c(2, 4, 5, 3, 6, 1),
        prediction = TRUE,
        colgap.forest.left = "3cm",
        label.left = "favors control",
        label.right = "favors entrainment",
        leftcols = c("studlab"),
        print.tau2 = TRUE,
        col.square = c("black", "#DF536B", "#61D04F", "#2297E6", "#28E2E5", "#CD0BBC"),
        col.study = c("black", "#DF536B", "#61D04F", "#2297E6", "#28E2E5", "#CD0BBC"),
        addrow.overall = FALSE,
        lwd = 3,
        print.I2 = TRUE,
        hetstat = TRUE,
        fontsize = 11,
        spacing = 1.5,
        leftlabs = c("Experiment", "MD", "SE"),
        rightlabs = FALSE,
     #   at = c(-15, -10, -5, 0, 5, 10, 15)
        
 )


 
 #======================================================
 #====== generate up- and down-regulation sine wave ====
 #======================================================
 

 ##  adapted chirp function from matlab

 tl = 2 # max time (in seconds)
 time = seq(from = 0, to = tl-0.001, by = 0.001) # time vector
 t1 = 1; # when the frequency reaches the desired value, in seconds
 p = 1 # method, 1 - linear, 2 - quadratic
 phi = 0; # start phase
 
 off1 = 4 # position of upper sine wave (labels addjust accordingly)
 off2 = -1 # position of lower sine wave (labels adjust accordingly)
 
 # slowing down
 f0 = 4 # start (doesn't work correctly if f0 and f1 inverted)
 f1 = 5.5 # end
 beta1   = (f1-f0)*(t1^(-p))
 yvalue1 = rev(cos(2*pi*(beta1/(1+p)*(time^(1+p))+f0*time+phi/360))) + off1 # rev to reverse order
 
 #speeding up
 f0 = 5.5
 f1 = 7 
 beta2   = (f1-f0)*(t1^(-p))
 yvalue2 = cos(2*pi*(beta2/(1+p)*(time^(1+p))+f0*time+phi/360)) + off2
 
entr = data.frame(t = time,
                  s1 = yvalue1,
                  s2 = yvalue2)
 
 # plot
 ggplot()+
   geom_path(data = entr, aes(t, s1), size = 2, color = "blue", alpha = 0.5)+
   geom_path(data = entr, aes(t, s2), size = 2, color = "red", alpha = 0.5)+
   ylim(-5,5)+
   geom_segment(aes(x = 0, y = off2-2, xend = tl, yend = off2-2),
                arrow = arrow(length = unit(0.2, "cm")), size = 1.5, color = "grey40")+
  # annotate(geom = "text", x=1, y=off2-3, label="time", color = "grey30", size = 10)+
  # annotate(geom = "text", x=-0.2, y=off1, label="4 Hz", color = "grey30", size = 10)+
  # annotate(geom = "text", x=-0.2, y=off2, label="7 Hz", color = "grey30", size = 10)+
   theme_classic()+
   theme(axis.title.x = element_blank(),
                 axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 axis.title.y = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks.y = element_blank(),
                 axis.line = element_blank())

 #===================================
 # ====== High and low performers ===
 #===================================
 
 # anova (freq + group + freq x group)
 

 dt <- ddply(d_joint, ~Experiment*frequency*performer*unique_ID, summarise, mean = mean(K_bsl))
 dt <- filter(dt, frequency != 0)
 
 a <- ezANOVA(
   data = dt[which(dt$Experiment == "Visual"),]
   , dv = mean
   , wid = unique_ID
   , between = performer
   , within = .(frequency)
   , type = 2
   , detailed = TRUE
 )
 
 print(a)
 
 a <- ezANOVA(
   data = dt[which(dt$Experiment == "Binaural (i)"),]
   , dv = mean
   , wid = unique_ID
   , between = performer
   , within = .(frequency)
   , type = 2
   , detailed = TRUE
 )
 
 print(a)
 
 a <- ezANOVA(
   data = dt[which(dt$Experiment == "Monaural (i)"),]
   , dv = mean
   , wid = unique_ID
   , between = performer
   , within = .(frequency)
   , type = 2
   , detailed = TRUE
 )
 
 print(a)
 
 a <- ezANOVA(
   data = dt[which(dt$Experiment == "Binaural (ii)"),]
   , dv = mean
   , wid = unique_ID
   , between = performer
   , within = .(frequency)
   , type = 2
   , detailed = TRUE
 )
 
 print(a)
 
 a <- ezANOVA(
   data = dt[which(dt$Experiment == "Binaural (iii)"),]
   , dv = mean
   , wid = unique_ID
   , between = performer
   , within = .(frequency)
   , type = 2
   , detailed = TRUE
 )
 
 print(a)
 
 a <- ezANOVA(
   data = dt[which(dt$Experiment == "Monaural (ii)"),]
   , dv = mean
   , wid = unique_ID
   , between = performer
   , within = .(frequency)
   , type = 2
   , detailed = TRUE
 )
 
 print(a)
 
 
 #add low/high to entr vs bsl data
 dp <- ddply(d_joint, ~unique_ID*performer,summarise, mean = mean(K_bsl))
 d_joint_entr$performer <-rep(dp$performer, each = 6) 
 
 
 # check 
 dt <- ddply(d_joint, ~Experiment*unique_ID*frequency*performer, summarise, mean = mean(K_bsl))
 dt <- filter(dt, frequency != 0)
 
 dt$Experiment <- factor(dt$Experiment, levels=c('Visual', 'Binaural (i)', 'Monaural (i)', 'Binaural (ii)', 'Binaural (iii)', 'Monaural (ii)'))
 
 ggplot(dt, aes(Experiment, mean, fill = performer))+
   #geom_hline(yintercept=0, linetype = 1, color = '#F8766D', size = 2)+
   geom_boxplot()+
   xlab("")+
   ylab("capacity index K (%) ")+
   
   theme_classic()+
   theme(
       #axis.text=element_text(size=15),
       axis.title=element_text(size=15),
       legend.title = element_text(size=13),
       #legend.position = "none",
       legend.text = element_text(size = 13),
       axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 15),
       axis.text.y = element_text(size = 15))+
   facet_wrap(~frequency)
 
 
 # use only entr vs control
 
 dt <- ddply(d_joint_entr, ~Experiment*unique_ID*condition*performer, summarise, mean = mean(K_bsl))
 dt <- filter(dt, condition != "control")
 dt$Experiment <- factor(dt$Experiment, levels=c('Visual', 'Binaural (i)', 'Monaural (i)', 'Binaural (ii)', 'Binaural (iii)', 'Monaural (ii)'))
 
 
 ggplot(dt, aes(Experiment, mean*100, fill = performer))+
   geom_hline(yintercept=0, linetype = 2, color = 'grey', size = 1)+
   geom_boxplot(width = 0.5)+
   xlab("")+
  # ylab("capacity index K (%) ")+
   ylab(expression(atop("capacity index K",atop("(change from baseline, %)"))))+
   theme_classic()+
   theme(
     #axis.text=element_text(size=15),
     axis.title=element_text(size=15),
     legend.title = element_text(size=13),
     #legend.position = "none",
     legend.text = element_text(size = 13),
     axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 12),
     axis.text.y = element_text(size = 15))+
   scale_fill_discrete(name = "capacity", labels = c("high", "low"))
#600 x350
 #======= add meta for high and low =======
 
 all_K <- ddply(d_joint_entr, ~Experiment*unique_ID*condition*performer, summarize, mean = mean(K_bsl), sd = sd(K_bsl))
 all_K <- all_K[which(all_K$Experiment != 'Monaural 3' & all_K$performer == "high"),]
 
 
 # store all ttest results in one variable
 ttest.result <- ddply(all_K[which(all_K$condition == 'entr'),], .(Experiment), t.test.plyr, "mean")
 ttest.result
 
 # function for standard errors and weights
 se.test <- function(df, var){
   y <- rep(NA, 5)
   y[1] <- nrow(df)[1] # n of observations
   y[2] <- mean(df[var][[1]]) # mean
   #if(nrow(df) < 2) return(0) 
   y[3] <- sd(df[var][[1]]) # sd
   y[4] <- y[3]/(sqrt(y[1])) # se
   y[5] <- 1/(y[4]^2) # weight
   names(y) <- c("nobs","mean", "sd", "se", "weight")
   y
 }
 
 
 sd.and.se <- ddply(all_K[which(all_K$condition == 'entr'),], .(Experiment), se.test, "mean")
 
 m.gen <- metagen(TE = sd.and.se$mean,
                  seTE = sd.and.se$se,
                  studlab = sd.and.se$Experiment,
                  data = sd.and.se,
                  sm = "MD",
                  fixed = TRUE,
                  random = FALSE,
                  method.tau = "REML",
                  hakn = TRUE,
                  title= 'test')
 summary(m.gen)
 m.gen <- update(m.gen, prediction = TRUE)
 summary(m.gen)
 # 
 
 
 forest(m.gen, 
        sortvar = c(2, 4, 5, 3, 6, 1),
        prediction = TRUE,
        colgap.forest.left = "3cm",
        label.left = "favors control",
        label.right = "favors entrainment",
        leftcols = c("studlab"),
        print.tau2 = TRUE,
        col.square = c("black", "#DF536B", "#61D04F", "#2297E6", "#28E2E5", "#CD0BBC"),
        col.study = c("black", "#DF536B", "#61D04F", "#2297E6", "#28E2E5", "#CD0BBC"),
        addrow.overall = FALSE,
        lwd = 3,
        print.I2 = TRUE,
        hetstat = TRUE,
        fontsize = 11,
        spacing = 1.5,
        leftlabs = c("Experiment", "MD", "SE"),
        rightlabs = FALSE,
        #   at = c(-15, -10, -5, 0, 5, 10, 15)
        
 )
 
 #=== high vs low with frequency
 dt <- filter(d_joint, frequency != 0)
 
 dt <- ddply(dt, ~Experiment*unique_ID*frequency*performer, summarise, mean = mean(K_bsl))
 dt$Experiment <- factor(dt$Experiment, levels=c('Visual', 'Binaural (i)', 'Monaural (i)', 'Binaural (ii)', 'Binaural (iii)', 'Monaural (ii)'))
 
 dt$performer <-ifelse(dt$performer=="high",'high-capacity','low-capacity')
 #dt[which(dt$performer == 'high'), 'performer'] <- c("high-capacity")
 #dt[which(dt$performer == 'low'), 'performer'] <- "low-capacity"
 
 ggplot(dt, aes(Experiment, mean, fill = frequency))+
   geom_hline(yintercept=0, linetype = 2, color = 'grey', size = 1)+
   geom_boxplot(width = 0.3)+
   facet_wrap(.~performer)+
   xlab("")+
   # ylab("capacity index K (%) ")+
   ylab(expression(atop("capacity index K",atop("(change from baseline, %)"))))+
   theme_classic()+
   theme(
     #axis.text=element_text(size=15),
     axis.title=element_text(size=15),
     legend.title = element_text(size=13),
     #legend.position = "none",
     legend.text = element_text(size = 13),
     axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 12),
     axis.text.y = element_text(size = 15),
     strip.text.x = element_text(size = 15))+
   scale_fill_manual(name = "frequency", labels = c("4 Hz", "7 Hz"), values = c("#2297E6", "#DF536B"))
 # 900 x 400
              
 
 #====================================
 #========== ANT task ================
 #====================================
 
 d <- read.table("C:\\Users\\IndrePil\\Documents\\ANT_online\\summaryTablePavlovia.csv", header=TRUE, sep=",", dec='.')
 dAll <- read.table("C:\\Users\\IndrePil\\Documents\\ANT_online\\allRespTablePavlovia.csv", header=TRUE, sep=",", dec='.')
 
 cols <- c('frequency', 'ID', 'cue', 'target', 'N')
 d[cols] <- lapply(d[cols], as.factor)
 
 cols <- c('frequency', 'ID', 'cue', 'target', 'targOrient', 'block', 'trial')
 dAll[cols] <- lapply(dAll[cols], as.factor)

 # RTs by cue type
 cues <- ddply(d,~ cue*ID*frequency,summarise,mean=mean(RT_corr_median))
 
 a <- ezANOVA(
  # data = cues[which(cues$cue == "upper"),]
   data = cues
   , dv = mean
   , wid = ID
   #  , between = performer
   , within = .(frequency, cue)
   , type = 2
   , detailed = TRUE
 )
 
 print(a)
 
 ggplot(cues[which(cues$cue == "centre"),], aes(frequency, mean))+
   geom_boxplot()
 
 
 nc <- filter(cues, cue %in% c('blank'))

 
 sc <- filter(cues, cue %in% c('lower', 'upper'))
 sc$cue <- rep('spatial', 600)
 sc <- ddply(sc, ~cue*ID*frequency, summarise, mean = mean(mean))
 
 cc <- filter(cues, cue %in% c('centre'))
 
 # stats on RTs
 all_cues <- rbind(nc, sc, cc)
 
 
 a <- ezANOVA(
   data = all_cues
   , dv = mean
   , wid = ID
   #  , between = performer
   , within = .(frequency, cue)
   , type = 2
   , detailed = TRUE
 )
 
 print(a)
 pairwise.t.test(all_cues$mean, all_cues$frequency, data = all_cues, paired = TRUE, p.adjust.method = 'bonferroni')
 
 ddply(all_cues, ~frequency, summarise, rt = mean(mean), rt_sd = sd(mean))
 #pairwise.t.test(all_cues$mean, all_cues$cue, data = all_cues, paired = TRUE, p.adjust.method = 'bonferroni')
 
 
 
 
 
 
 
 
 # ANT scores
 OS <- cc$mean-sc$mean
 OS<- cbind(cc[,c('ID', 'frequency')], OS)
 
 AS <- nc$mean-cc$mean
 AS<- cbind(nc[,c('ID', 'frequency')], AS) 

 # scores per frequency
 AS_0 <- filter(AS, frequency %in% c(0))
 AS_4 <- filter(AS, frequency %in% c(4))
 AS_7 <- filter(AS, frequency %in% c(7))

 # baseline subtracted scores
 AS_norm_4_sub <-AS_4$AS-AS_0$AS
 
 plot(1:length(AS_norm_4_sub), AS_norm_4_sub)
 
 AS_norm_7_sub <- AS_7$AS-AS_0$AS
 plot(1:length(AS_norm_7_sub), AS_norm_7_sub)
 
 #
 AS_norm_sub <- c(AS_norm_4_sub, AS_norm_7_sub)
 
 AS_4_7 <- rbind(AS_4, AS_7)
 AS_4_7 <- cbind(AS_4_7, AS_norm_sub)
 
 # t tests 
 AS_4_7$frequency <- as.factor( AS_4_7$frequency)
 
 # check assumptions
 shapiro.test(AS_4_7[which(AS_4_7$frequency == 4),'AS_norm_sub'])
 shapiro.test(AS_4_7[which(AS_4_7$frequency == 7),'AS_norm_sub'])
 dt <- filter(dt, frequency!=0)
 leveneTest(AS_norm_sub ~ frequency, data = AS_4_7)

 
 t.test(AS_4_7[which(AS_4_7$frequency == 4),'AS_norm_sub'], 
        AS_4_7[which(AS_4_7$frequency == 7),'AS_norm_sub'], 
        paired = TRUE, alternative = 'two.sided' ) 
 
AS_4_7$frequency <- factor(AS_4_7$frequency)
 cohensD(AS_norm_sub ~ frequency,
         data   = AS_4_7,
         method = "paired")
 
 
 t.test(AS_4_7[which(AS_4_7$frequency == 4),'AS_norm_sub'], mu = 0)
 cohensD(AS_4_7[which(AS_4_7$frequency == 4),'AS_norm_sub'],
         mu = 0)
 
 t.test(AS_4_7[which(AS_4_7$frequency == 7),'AS_norm_sub'], mu = 0, alternative = 'two.sided')
 cohensD(AS_4_7[which(AS_4_7$frequency == 7),'AS_norm_sub'],
         mu = 0)
 
 ggplot(AS_4_7, aes(frequency, AS_norm_sub*1000, fill = frequency))+
   geom_hline(yintercept=0, linetype = 2, color = 'grey', size = 1)+
   geom_boxplot(width = 0.3)+
   ylab('Alerting score (ms)')+
   xlab("frequency")+
   theme_classic()+
   theme(
     #axis.text=element_text(size=15),
     axis.title=element_text(size=15),
     legend.title = element_blank(),
     legend.position = "none",
     legend.text = element_blank(),
     axis.text.x = element_text( size = 12),
     axis.text.y = element_text(size = 15),
     strip.text.x = element_text(size = 15))+
   scale_fill_manual(name = "frequency", labels = c("4 Hz", "7 Hz"), values = c("#2297E6", "#DF536B"))+
   scale_x_discrete(breaks = c(4, 7), labels = c("4 Hz", "7 Hz"))
 
 #600 x 474
 
 #================================================
 #============= pupillometry ====================
 #==============================================
 
 
 # behavioral summarized data
 d <- read.table("C:\\Users\\IndrePil\\Documents\\vWM Entrainment\\Pupillometry\\results\\allSummaryTable.csv", header=TRUE, sep=",", dec='.')
 
 # raw behavioral
 d_all <- read.table("C:\\Users\\IndrePil\\Documents\\vWM Entrainment\\Pupillometry\\results\\allRespTable.csv", header=TRUE, sep=",", dec='.')
 
 # convert variables into factors
 cols <- c('frequency', 'Load', 'ID', 'gender')
 d[cols] <- lapply(d[cols], as.factor)
 
 colsA <- c('ID', 'block', 'frequency', 'loads', 'trial')
 d_all[colsA] <- lapply(d_all[colsA], as.factor)
 
 
 # good participants (based on preprocessing in 'analysins_pupillometry.R')
 ID_in = c('P03', 'P04', 'S02', 'S03', 'S05', 'S06', 'S08', 'S09', 'S10', 'S11',
           'S12', 'S16', 'S17', 'S18', 'S19', 'S20', 'S22', 'S23', 'S24', 'S25',
           'S27', 'S28', 'S29', 'S30', 'S31', 'S32', 'S33', 'S35', 'S36', 'S38',
           'S39', 'S40', 'S41', 'S42', 'S44', 'S45', 'S46', 'S47', 'S48', 'S49',
           'S50', 'S51', 'S52', 'S53', 'S55', 'S56', 'S58', 'S59')
 
 
 d <- filter(d, ID %in% ID_in)
 d_all <- filter(d_all, ID %in% ID_in)
 # add low/high performers

 k <- ddply(d[which(d$frequency == 0),], ~ID, summarise, mean_k = mean(K))
 #k$performer <- k$ID
 
 k[which(k$mean_k < median(k$mean_k)), 'performer'] <- 'low'
 k[which(k$mean_k >= median(k$mean_k)), 'performer'] <- 'high'
 
 performer <- rep(k$performer, each = 9)
 
 d$performer <-performer
 
 # show delay period waveform
 d_w <- read.table("C:\\Users\\IndrePil\\Documents\\vWM Entrainment\\Pupillometry\\results\\PupilWaveform.csv", header=TRUE, sep=",", dec='.')
 
 
 d_w <- filter(d_w, ID %in% ID_in)
 
 colsA <- c('ID', 'frequency', 'load', 'response')
 d_w[colsA] <- lapply(d_w[colsA], as.factor)

 
 d_w_del <- filter(d_w, frame > 201 & frame < 630)
 d_w_del <- na.omit(d_w_del)
 
 # diff 4 vs 7, fixation period
 
 
 d_w_del <- filter(d_w, frame > 201 & frame < 411 & frequency != 0)
 d_w_del <- na.omit(d_w_del)
 
 
 #add low and high performers
 low <- k[which(k$performer == 'low'), 'ID']
 high <- k[which(k$performer == 'high'), 'ID']
 
 d_w_del$performer <- c()
 d_w_del[which(d_w_del$ID %in% low),'performer'] <- 'low'
 d_w_del[which(d_w_del$ID %in% high),'performer'] <- 'high'
 
 
 wave_plot <- ddply(d_w_del, c("performer","frequency", "frame"), dplyr::summarise,
                    N    = n(),
                    mean = mean(rPupilFilt_z_diff, na.rm=TRUE),
                    sd   = sd(rPupilFilt_z_diff, na.rm=TRUE),
                    se   = sd / sqrt(N),
                    CIlower = Rmisc::CI(rPupilFilt_z_diff, ci = 0.95)["lower"],
                    CIupper = Rmisc::CI(rPupilFilt_z_diff, ci = 0.95)["upper"])
 
 wave_plot$time = rep(seq(-2000, 100, length.out = length(wave_plot[which( wave_plot$frequency == 4 & wave_plot$performer == "low"),"frame"])), 2)
 
 
 # add smoothing over the lines
 knot = 20
 
 #x1 <- smooth.spline(x = wave_plot[which(wave_plot$frequency == 0),"time"], y = wave_plot[which(wave_plot$frequency == 0),"mean"], nknots = knot)
 x2 <- smooth.spline(x = wave_plot[which(wave_plot$frequency == 4 & wave_plot$performer == "high"),"time"], y = wave_plot[which(wave_plot$frequency == 4 & wave_plot$performer == "high"),"mean"], nknots = knot)
 x3 <- smooth.spline(x = wave_plot[which(wave_plot$frequency == 7 & wave_plot$performer == "high"),"time"], y = wave_plot[which(wave_plot$frequency == 7 & wave_plot$performer == "high"),"mean"], nknots = knot)
 x4 <- smooth.spline(x = wave_plot[which(wave_plot$frequency == 4 & wave_plot$performer == "low"),"time"], y = wave_plot[which(wave_plot$frequency == 4 & wave_plot$performer == "low"),"mean"], nknots = knot)
 x5 <- smooth.spline(x = wave_plot[which(wave_plot$frequency == 7 & wave_plot$performer == "low"),"time"], y = wave_plot[which(wave_plot$frequency == 7 & wave_plot$performer == "low"),"mean"], nknots = knot)
 
 
 #cu1 <- smooth.spline(x = wave_plot[which(wave_plot$frequency == 0),"time"], y = wave_plot[which(wave_plot$frequency == 0),"CIupper"], nknots = knot)
 cu2 <- smooth.spline(x = wave_plot[which(wave_plot$frequency == 4 & wave_plot$performer == "high"),"time"], y = wave_plot[which(wave_plot$frequency == 4 & wave_plot$performer == "high"),"CIupper"], nknots = knot)
 cu3 <- smooth.spline(x = wave_plot[which(wave_plot$frequency == 7 & wave_plot$performer == "high"),"time"], y = wave_plot[which(wave_plot$frequency == 7 & wave_plot$performer == "high"),"CIupper"], nknots = knot)
 cu4 <- smooth.spline(x = wave_plot[which(wave_plot$frequency == 4 & wave_plot$performer == "low"),"time"], y = wave_plot[which(wave_plot$frequency == 4 & wave_plot$performer == "low"),"CIupper"], nknots = knot)
 cu5 <- smooth.spline(x = wave_plot[which(wave_plot$frequency == 7 & wave_plot$performer == "low"),"time"], y = wave_plot[which(wave_plot$frequency == 7 & wave_plot$performer == "low"),"CIupper"], nknots = knot)
 
 
 #cl1 <- smooth.spline(x = wave_plot[which(wave_plot$frequency == 0),"time"], y = wave_plot[which(wave_plot$frequency == 0),"CIlower"], nknots = knot)
 cl2 <- smooth.spline(x = wave_plot[which(wave_plot$frequency == 4 & wave_plot$performer == "high"),"time"], y = wave_plot[which(wave_plot$frequency == 4 & wave_plot$performer == "high"),"CIlower"], nknots = knot)
 cl3 <- smooth.spline(x = wave_plot[which(wave_plot$frequency == 7 & wave_plot$performer == "high"),"time"], y = wave_plot[which(wave_plot$frequency == 7 & wave_plot$performer == "high"),"CIlower"], nknots = knot)
 cl4 <- smooth.spline(x = wave_plot[which(wave_plot$frequency == 4 & wave_plot$performer == "low"),"time"], y = wave_plot[which(wave_plot$frequency == 4 & wave_plot$performer == "low"),"CIlower"], nknots = knot)
 cl5 <- smooth.spline(x = wave_plot[which(wave_plot$frequency == 7 & wave_plot$performer == "low"),"time"], y = wave_plot[which(wave_plot$frequency == 7 & wave_plot$performer == "low"),"CIlower"], nknots = knot)
 
 
 
 wave_plot$filt <- c(x2$y, x3$y, x4$y,x5$y)
 wave_plot$CIu_filt <- c(cu2$y, cu3$y, cu4$y, cu5$y)
 wave_plot$CIl_filt <- c(cl2$y, cl3$y, cl4$y, cl5$y)
 
 wave_plot[which( wave_plot$performer == "low"),'performer'] <- 'low-capacity'
 wave_plot[which( wave_plot$performer == "high"),'performer'] <- 'high-capacity'

 
 ggplot(wave_plot, aes(time, filt, color = frequency, group = frequency))+
   geom_ribbon(aes(ymin = CIl_filt, ymax = CIu_filt, fill = frequency), alpha = 0.1, linetype = 0)+
   stat_summary(fun.y = "mean",  geom = "line", aes(group = frequency),  size = 1)+
   ylab("mean pupil diameter (z-score)")+
   xlab("time (ms)")+
   geom_vline(xintercept = 0, linetype = 2)+
   theme_classic()+
   theme(legend.text = element_text(size=18),
         legend.title = element_blank(),
       #  legend.position = "bottom",
         axis.text=element_text(size=18),
         axis.title=element_text(size=20),
       strip.text.x = element_text(size = 18))+
   scale_color_manual(name = "frequency", labels = c("4 Hz", "7 Hz"), values = c("#2297E6", "#DF536B"))+
   scale_fill_manual(name = "frequency", labels = c("4 Hz", "7 Hz"), values = c("#2297E6", "#DF536B"))+
   facet_wrap(~performer)
# 700 x 450
 #850 x450
 
 dt <- ddply(d, ~ID*frequency*performer, summarise, mean_pupil = mean(rP_mean))
 
 ggplot(dt[which(dt$frequency != 0),], aes(frequency, mean_pupil, fill = performer))+
   geom_boxplot(width = 0.3)+
   ylab('mean pupil (z-score)')+
   geom_boxplot(width = 0.3)+
   xlab("frequency")+
   theme_classic()+
   theme(axis.title=element_text(size=20),
     legend.title = element_text( size = 18),
     #legend.position = "none",
     legend.text = element_text( size = 18),
     axis.text.x = element_text( size = 18),
     axis.text.y = element_text(size = 18))+
     #strip.text.x = element_text(size = 15))+
   scale_fill_manual(name = "capacity", labels = c("high", "low"), values = c("#28E2E5", "#CD0BBC"))+
   scale_x_discrete(breaks = c(4, 7), labels = c("4 Hz", "7 Hz"))
 
 #600 x 474
 
 dt$frequency <- as.factor(dt$frequency)
 
 # check assumptions
 shapiro.test(dt[which(dt$frequency == 4),'mean_pupil'])
 shapiro.test(dt[which(dt$frequency == 7),'mean_pupil'])
 dt <- filter(dt, frequency!=0)
 leveneTest(mean_pupil ~ frequency, data = dt)
 
 t.test(dt[which(dt$frequency == 4),'mean_pupil'], 
        dt[which(dt$frequency == 7),'mean_pupil'], 
        paired = TRUE, alternative = 'two.sided' ) 
 
 # K
 dt <- ddply(d, ~ID*frequency*performer, summarise, mean = mean(K_bsl))
 dt$frequency <- as.factor(dt$frequency)
 
 # check assumptions
 shapiro.test(dt[which(dt$frequency == 4 & performer == "low"),'mean'])
 shapiro.test(dt[which(dt$frequency == 4 & performer == "high"),'mean'])
 shapiro.test(dt[which(dt$frequency == 7 & performer == "low"),'mean'])
 shapiro.test(dt[which(dt$frequency == 7 & performer == "high"),'mean'])
 dt <- filter(dt, frequency!=0)
 leveneTest(mean ~ frequency*performer, data = dt)
 dt$tr <- log(dt$mean +1) # for checking validity of results with transformed data

 a <- ezANOVA(
   data = dt[which(dt$frequency != 0),]
   , dv = mean
   , wid = ID
     , between = performer
   , within = .(frequency)
   , type = 2
   , detailed = TRUE
 )
 
 print(a)
 
 ddply(d, ~performer, summarise, mean = mean(K_bsl), sd = sd(K_bsl))
 
 # pupil
 dt <- ddply(d, ~ID*frequency*performer, summarise, mean = mean(rP_mean))
 dt$frequency <- as.factor(dt$frequency)
 
 # check assumptions
 shapiro.test(dt[which(dt$frequency == 4 & performer == "low"),'mean'])
 shapiro.test(dt[which(dt$frequency == 4 & performer == "high"),'mean'])
 shapiro.test(dt[which(dt$frequency == 7 & performer == "low"),'mean'])
 shapiro.test(dt[which(dt$frequency == 7 & performer == "high"),'mean'])
 dt <- filter(dt, frequency!=0)
 leveneTest(mean ~ frequency*performer, data = dt)
 
 a <- ezANOVA(
   data = dt[which(dt$frequency != 0),]
   , dv = mean
   , wid = ID
   , between = performer
   , within = .(frequency)
   , type = 2
   , detailed = TRUE
 )
 
 print(a)
 
 ddply(d, ~performer, summarise, mean = mean(rP_mean), sd = sd(rP_mean))
 
 #===================================================================
 #============= ANOVAs and t-test for main Results section ===========
 #====================================================================
 
 # visual
 dt <- ddply(dV, ~frequency*jitter*ID, summarise, mean = mean(K_bsl))
 dt$frequency <- as.factor(dt$frequency)
 
 # check assumptions
 shapiro.test(dt[which(dt$frequency == 4 & dt$jitter == "true"),'mean'])
 shapiro.test(dt[which(dt$frequency == 4 & dt$jitter == "false"),'mean'])
 shapiro.test(dt[which(dt$frequency == 7 & dt$jitter == "true"),'mean'])
 shapiro.test(dt[which(dt$frequency == 7 & dt$jitter == "false"),'mean'])
 
 dt<- filter(dt, frequency != 0)
 leveneTest(mean ~ frequency, data = dt)
 
 a <- ezANOVA(
   data = dt[which(dt$frequency != 0),]
   , dv = mean
   , wid = ID
 #  , between = performer
   , within = .(frequency, jitter)
   , type = 2
   , detailed = TRUE
 )
 
 print(a)
 
 ddply(dV, ~frequency, summarise, m = mean(K_bsl), sd = sd(K_bsl))
 
 # binaural (i)
 
 dt <- ddply(dBB1, ~ID*frequency, summarise, mean_K = mean(K_bsl))
 dt$frequency <- as.factor(dt$frequency)
 
 # check assumptions
 shapiro.test(dt[which(dt$frequency == 4),'mean_K'])
 shapiro.test(dt[which(dt$frequency == 7),'mean_K'])
 dt <- filter(dt, frequency!=0)
 leveneTest(mean_K ~ frequency, data = dt)

 
 t.test(dt[which(dt$frequency == 4),'mean_K'], 
        dt[which(dt$frequency == 7),'mean_K'], 
        paired = TRUE, alternative = 'two.sided' )
 ddply(dBB1, ~frequency, summarise, m = mean(K_bsl), sd = sd(K_bsl))
 
 dt <- filter(dt, frequency!=0)
 dt<- droplevels(dt)
 cohensD(mean_K ~ frequency,
         data   = dt,
         method = "paired")

 # monaural (i)
 
 dt <- ddply(dMB, ~frequency*ID*jitter, summarise, mean = mean(K_bsl))
 
 dt$frequency <- as.factor(dt$frequency)
 
 # check assumptions
 shapiro.test(dt[which(dt$frequency == 4 & dt$jitter == "true"),'mean'])
 shapiro.test(dt[which(dt$frequency == 4 & dt$jitter == "false"),'mean'])
 shapiro.test(dt[which(dt$frequency == 7 & dt$jitter == "true"),'mean'])
 shapiro.test(dt[which(dt$frequency == 7 & dt$jitter == "false"),'mean'])
 
 dt <- filter(dt, frequency!=0)
 leveneTest(mean ~ frequency*jitter, data = dt)

 a <- ezANOVA(
   data = dt[which(dt$frequency != 0),]
   , dv = mean
   , wid = ID
   #  , between = performer
   , within = .(frequency, jitter)
   , type = 2
   , detailed = TRUE
 )
 
 print(a)
 
 ddply(dMB, ~frequency, summarise, m = mean(K_bsl), sd = sd(K_bsl))
 
 # binaural (ii)
 
 dt <- ddply(dBB2, ~ID*frequency, summarise, mean_K = mean(K_bsl))
 dt$frequency <- as.factor(dt$frequency)
 
 # check assumptions
 shapiro.test(dt[which(dt$frequency == 4),'mean_K'])
 shapiro.test(dt[which(dt$frequency == 7),'mean_K'])
 dt <- filter(dt, frequency!=0)
 leveneTest(mean_K ~ frequency, data = dt)
 
 
 
 
 t.test(dt[which(dt$frequency == 4),'mean_K'], 
        dt[which(dt$frequency == 7),'mean_K'], 
        paired = TRUE, alternative = 'two.sided' )
 ddply(dBB2, ~frequency, summarise, m = mean(K_bsl), sd = sd(K_bsl))
 
 #dt <- filter(dt, frequency!=0)
 dt<- droplevels(dt)
 cohensD(mean_K ~ frequency,
         data   = dt,
         method = "paired")
 
 # binaural (iii)
 
 
 dt <- ddply(dBB3, ~ID*frequency, summarise, mean_K = mean(K_bsl))
 dt$frequency <- as.factor(dt$frequency)
 
 # check assumptions
 shapiro.test(dt[which(dt$frequency == 4),'mean_K'])
 shapiro.test(dt[which(dt$frequency == 7),'mean_K'])
 dt <- filter(dt, frequency!=0)
 leveneTest(mean_K ~ frequency, data = dt)
 
 
 
 t.test(dt[which(dt$frequency == 4),'mean_K'], 
        dt[which(dt$frequency == 7),'mean_K'], 
        paired = TRUE, alternative = 'two.sided' )
 ddply(dBB3, ~frequency, summarise, m = mean(K_bsl), sd = sd(K_bsl))
 
 #dt <- filter(dt, frequency!=0)
 dt<- droplevels(dt)
 cohensD(mean_K ~ frequency,
         data   = dt,
         method = "paired")
 
 # monaural (ii)
 
 dt <- ddply(dOn, ~ID*frequency, summarise, mean_K = mean(K_bsl))
 dt$frequency <- as.factor(dt$frequency)
 
 # check assumptions
 shapiro.test(dt[which(dt$frequency == 4),'mean_K'])
 shapiro.test(dt[which(dt$frequency == 7),'mean_K'])
 dt <- filter(dt, frequency!=0)
 leveneTest(mean_K ~ frequency, data = dt)
 
 
 
 t.test(dt[which(dt$frequency == 4),'mean_K'], 
        dt[which(dt$frequency == 7),'mean_K'], 
        paired = TRUE, alternative = 'less' )
 ddply(dOn, ~frequency, summarise, m = mean(K_bsl), sd = sd(K_bsl))
 
 dt <- filter(dt, frequency!=0)
 dt<- droplevels(dt)
 cohensD(mean_K ~ frequency,
         data   = dt,
         method = "paired")
 #=========================================================
 #========== rhythmic minus arrhythmic in Visual ==========
 #=========================================================
 
 # chech rhy vs arr
 rhy <- ddply(dV, ~ID*jitter*frequency,summarise, mean = mean(K_bsl), sd = sd(K_bsl))
 ggplot(rhy, aes(frequency, mean, fill = jitter))+
   geom_boxplot()
 

 
 # subtract rhy vs arh
 rhy <- ddply(dV, ~ID*jitter*frequency,summarise, mean = mean(K))
 
 r <- filter(rhy, (frequency != 0) & jitter == "false")
 a <- filter(rhy, frequency != 0 & jitter == "true")
 
 s<- r$mean-a$mean
 
 r$diff <-r$mean-a$mean
 
 r$frequency <- as.factor(r$frequency)
 
 shapiro.test(r[which(r$frequency == 4),'mean'])
 shapiro.test(r[which(r$frequency == 7),'mean'])
 r <- filter(r, frequency!=0)
 leveneTest(mean ~ frequency*jitter, data = r)
 
 
 t.test(r[which(r$freq == 4), 'diff'],
        r[which(r$freq == 7), 'diff'],
        paired = TRUE)
 
 cohensD(mean ~ frequency,
         data   = r,
         method = "paired")
 
 t.test(r[which(r$freq == 7), 'diff'], mu = 0)
 
 cohensD(r[which(r$frequency == 7),"diff"],
         mu = 0)
 
 t.test(r[which(r$freq == 4), 'diff'], mu = 0)
 cohensD(r[which(r$frequency == 4),"diff"],
         mu = 0)

 mean(r[which(r$frequency == 4),"mean"])/sd(r[which(r$frequency == 4),"mean"])
 
 #===================================================================
 #=========== t tests and Cohens d for combined 4 Hz+7Hz vs bls =====
 #===================================================================
 
 # Visual
 dt <- ddply(dV_sum, ~ID*condition, summarise, mean_K = mean(K_bsl))
 shapiro.test(dt[which(dt$condition == "entr"),'mean_K'])
 
 
 t.test(dt[which(dt$condition == "entr"), 'mean_K'], mu = 0)

 cohensD(dt[which(dt$condition == "entr"), 'mean_K'],
         mu = 0)
 
 # Monaural i
 dt <- ddply(dMB_sum, ~ID*condition, summarise, mean_K = mean(K_bsl))
 shapiro.test(dt[which(dt$condition == "entr"),'mean_K'])
 
 t.test(dt[which(dt$condition == "entr"), 'mean_K'], mu = 0)
 
 cohensD(dt[which(dt$condition == "entr"), 'mean_K'],
         mu = 0)
 
 # Monaural ii
 dt <- ddply(dOn_sum, ~ID*condition, summarise, mean_K = mean(K_bsl))
 shapiro.test(dt[which(dt$condition == "entr"),'mean_K'])
 
 t.test(dt[which(dt$condition == "entr"), 'mean_K'], mu = 0)
 
 cohensD(dt[which(dt$condition == "entr"), 'mean_K'],
         mu = 0)

 # Binaural i
 dt <- ddply(dBB1_sum, ~ID*condition, summarise, mean_K = mean(K_bsl))
 shapiro.test(dt[which(dt$condition == "entr"),'mean_K'])
 
 t.test(dt[which(dt$condition == "entr"), 'mean_K'], mu = 0)
 
 cohensD(dt[which(dt$condition == "entr"), 'mean_K'],
         mu = 0)
 
 # Binaural ii
 dt <- ddply(dBB2_sum, ~ID*condition, summarise, mean_K = mean(K_bsl))
 shapiro.test(dt[which(dt$condition == "entr"),'mean_K'])
 
 t.test(dt[which(dt$condition == "entr"), 'mean_K'], mu = 0)
 
 cohensD(dt[which(dt$condition == "entr"), 'mean_K'],
         mu = 0)
 
 # Binaural iii
 dt <- ddply(dBB3_sum, ~ID*condition, summarise, mean_K = mean(K_bsl))
 shapiro.test(dt[which(dt$condition == "entr"),'mean_K'])
 
 t.test(dt[which(dt$condition == "entr"), 'mean_K'], mu = 0)
 
 cohensD(dt[which(dt$condition == "entr"), 'mean_K'],
         mu = 0)
 
 