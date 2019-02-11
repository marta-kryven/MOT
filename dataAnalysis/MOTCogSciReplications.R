library(ggplot2);
library(pwr);
library(Hmisc);                       # includes rcorr
library(dplyr);

#--------------------------------------------------------------------------------------------------------
#
#  exclusion variables 
#
#--------------------------------------------------------------------------------------------------------


minimal_accuracy_cutoff = 0.2;        #  this is slightly higher than at chance, but we'd like to make sure that they did not do too badly
                                     
minimal_rt_cutoff = 550;               #  RT cutoff - this gets rid of subjects with touch screens


#---------------------------------------------------------------------------------------------------------------------------
#
#  Loading all data; please be careful not to merge Jan 9 and Jan 11 - they have a different set of columns  compared to others
#
#---------------------------------------------------------------------------------------------------------------------------


main_path = '/Users/luckyfish/Desktop/MOT/MOTReplications/data/';    # Make sure you have the correct path

data11 = read.csv( paste(main_path, 'trialsJan11.csv', sep ='') , sep = "\t", strip.white=TRUE, header=TRUE);  
demo11 = read.csv( paste(main_path, 'demoJan11.csv', sep ='') , sep = "\t", strip.white=TRUE, header=TRUE); 

data9 = read.csv( paste(main_path, 'trialsJan9.csv', sep ='') , sep = "\t", strip.white=TRUE, header=TRUE);  
demo9 = read.csv( paste(main_path, 'demoJan9.csv', sep ='') , sep = "\t", strip.white=TRUE, header=TRUE); 


#---------------------------------------------------------------------------------------------------------------------------
#
#  Jan 16 and 17 includes feedback during experiment
#
#---------------------------------------------------------------------------------------------------------------------------


data16 = read.csv( paste(main_path, 'trialsJan16.csv', sep ='') , sep = "\t", strip.white=TRUE, header=TRUE);  
demo16 = read.csv( paste(main_path, 'demoJan16.csv', sep ='') , sep = "\t", strip.white=TRUE, header=TRUE); 

data17 = read.csv( paste(main_path, 'trialsJan17.csv', sep ='') , sep = "\t", strip.white=TRUE, header=TRUE);  
demo17 = read.csv( paste(main_path, 'demoJan17.csv', sep ='') , sep = "\t", strip.white=TRUE, header=TRUE); 

#---------------------------------------------------------------------------------------------------------------------------
#
#  Jan 14 and 21, 30 feedback given only during calibration
#
#---------------------------------------------------------------------------------------------------------------------------


data14 = read.csv( paste(main_path, 'trialsJan14.csv', sep ='') , sep = "\t", strip.white=TRUE, header=TRUE);  
demo14 = read.csv( paste(main_path, 'demoJan14.csv', sep ='') , sep = "\t", strip.white=TRUE, header=TRUE); 

data21 = read.csv( paste(main_path, 'trialsJan21.csv', sep ='') , sep = "\t", strip.white=TRUE, header=TRUE);  
demo21 = read.csv( paste(main_path, 'demoJan21.csv', sep ='') , sep = "\t", strip.white=TRUE, header=TRUE); 

#---------------------------------------------------------------------------------------------------------------------------
#
#  Jan 30 uses three very different pairs of backgrounds, two of which are from the new set
#
#---------------------------------------------------------------------------------------------------------------------------


data30 = read.csv( paste(main_path, 'MOTJan30data.csv', sep ='') , sep = "\t", strip.white=TRUE, header=TRUE);  
demo30 = read.csv( paste(main_path, 'MOTJan30demo.csv', sep ='') , sep = "\t", strip.white=TRUE, header=TRUE); 


#---------------------------------------------------------------------------------------------------------------------------
#
#  We can merge all of them at this point and carry out the exclusions on all data at the same time
#
#---------------------------------------------------------------------------------------------------------------------------


data = rbind(data21, data14, data16, data17, data30);
demo = rbind(demo21, demo14, demo16, demo17, demo30);


#---------------------------------------------------------------------------------------------------------------------------
#
#  postprocessing
#
#---------------------------------------------------------------------------------------------------------------------------


data =  merge(data, demo[, c("subject", "date")], by  = "subject");
data$respcorrect = (data$numcorrect == 4);    # postprocessing


##--------------------------------------------------------------------------------------------------------
##---------------------------------------------------------------------------------------------------------------------------
##
##  below are various mandatory exclusion procedures
##
##---------------------------------------------------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------------------------



#--------------------------------------------------------------------------------------------------------
#
#  exclude the ones at celling
#
#--------------------------------------------------------------------------------------------------------


celling = subset(demo, demo$accstable == 1 & demo$accunstable == 1);

if (length(celling$subject) > 0) {
   for (i in 1: length(celling$subject)) {
	   cat("removing celling accuracy ", as.character(celling$subject[i]), demo$accstable[which(demo$subject == celling$subject[i])],  demo$accunstable[which(demo$subject == celling$subject[i])], "\n");
	   data = subset(data, data$subject != celling$subject[i]);
	   demo = subset(demo, demo$subject != celling$subject[i]);
   }
}


#--------------------------------------------------------------------------------------------------------
#
#   identify subjects who clicked at chance, based on stable accuracy  
#   --- actualy even if they get two trials out of 18 correctly they are already above chance  
#
#--------------------------------------------------------------------------------------------------------

sub = demo$subject;
for (i in 1:length(sub)) {
	k = demo$numstable[which(demo$subject == sub[i])];
	n = 18;
	p = choose(n,k)*(1/70)^k*(69/70)^(n-k);
	if (p > 0.01) {
	   cat("clicked at chance", as.character(sub[i]), demo$accstable[ which(demo$subject == sub[i])], p, "\n")
	   data = subset(data, data$subject != sub[i]);
	   demo = subset(demo, demo$subject != sub[i]);
	}
}


#--------------------------------------------------------------------------------------------------------
#
#   exclude subjects who click too fast 
#   
#--------------------------------------------------------------------------------------------------------

df2 = data[ , c("trial", "condition", "rt2",  "respcorrect", "subject")]; colnames(df2)[3] <- "rt"; df2$whichball = "2";
df3 = data[ , c("trial", "condition", "rt3",  "respcorrect", "subject")]; colnames(df3)[3] <- "rt"; df3$whichball = "3";
df4 = data[ , c("trial", "condition", "rt4",  "respcorrect", "subject")]; colnames(df4)[3] <- "rt"; df4$whichball = "4";
df = rbind(df2, df3);
df = rbind(df, df4);

# ggplot(data = df, aes(x = rt, fill = subject) ) + geom_density(alpha = 0.5) + xlim(0, 3000)                             # ok, it looks like there is at least one bad one

aggdata <-aggregate(df$rt, by=list(df$subject), FUN = function(x) c(mean = mean(x), se = sd(x)/sqrt(length(x)) ))
colnames(aggdata) <- c("subject", "x");
aggdata$rt = aggdata$x[,1];
aggdata$SE = aggdata$x[,2];

ggplot(aggdata, aes(x = rt)) + geom_histogram();                                                                        # the bad one stands out at 400 ms

for (i in 1: length(aggdata$rt)) {
	if ( aggdata$rt[i] < minimal_rt_cutoff ) {
	  cat("goofball ", as.character(aggdata$subject[i]), demo$accstable[which(demo$subject == aggdata$subject[i])],  demo$accunstable[which(demo$subject == aggdata$subject[i])], "\n");
	  data = subset(data, data$subject != aggdata$subject[i]);
	  demo = subset(demo, demo$subject != aggdata$subject[i]);
	}
}



#--------------------------------------------------------------------------------------------------------
#
#   Important: 
#   Fixing the data recorded before Jan 17 
#   The bug was that if subject double-click on the last ball, their overall accuracy is calculated badly
#   This was fixed on Jan 17
#
#--------------------------------------------------------------------------------------------------------


a = aggregate(data$respcorrect, by = list(data$condition, data$subject), FUN=mean)
colnames(a) <- c("condition", "subject", "acc")
s = subset(a, a$condition == "stable")
u = subset(a, a$condition == "unstable")
colnames(s) = c("condition", "subject", "accstable")
colnames(u) = c("condition", "subject", "accunstable")
s$accstable = round(s$accstable*100)/100;
u$accunstable = round(u$accunstable*100)/100;

for (sub in demo$subject) {

	accs = s$accstable[which(s$subject == sub)];
	accu = u$accunstable[which(u$subject == sub)];
		
	if (demo$accstable[which(demo$subject == sub)] != accs | demo$accunstable[which(demo$subject == sub)] != accu) {

        cat ("subject with badly calculated accuracy: ", sub, ":(",  demo$accstable[which(demo$subject == sub)], ",", demo$accunstable[which(demo$subject == sub)], ") instead of (", accs, "," , accu, ")\n")
        
		demo$accstable[which(demo$subject == sub)] = accs;
		demo$accunstable[which(demo$subject == sub)] = accu;
		demo$numunstable[which(demo$subject == sub)] = sum( subset(data$respcorrect, data$subject == sub & data$condition == "stable"));
		demo$numunstable[which(demo$subject == sub)] = sum( subset(data$respcorrect, data$subject == sub & data$condition == "unstable"))
	}
}

##--------------------------------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------------------------
##
##  Manual exclusions for subjects who reported technical problems, or gave incoherent debriefing resonses.
##
##--------------------------------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------------------------


demo[, c("subject", "problems")]
demo[, c("subject", "howwell")]
demo[, c("subject", "monitor")]
demo[, c("subject", "strategies")]

#--------------------------------------------------------------------------------------------------------
#
#   Jan 9 
#   
#--------------------------------------------------------------------------------------------------------


data = subset(data, data$subject != "S63710622");   # Jan 9 - technical problems
demo = subset(demo, demo$subject != "S63710622");

data = subset(data, data$subject != "S6306345");    # Jan 9 - problems
demo = subset(demo, demo$subject != "S6306345");

data = subset(data, data$subject != "S26744450");   # Jan 9 -- 7 inch monitor
demo = subset(demo, demo$subject != "S26744450");

#--------------------------------------------------------------------------------------------------------
#
#   Jan 11 
#   
#--------------------------------------------------------------------------------------------------------


data = subset(data, data$subject != "S55597793");   # Jan 11 - problems
demo = subset(demo, demo$subject != "S55597793");

data = subset(data, data$subject != "S82819066");   # Jan 11 - did calibration, then restarted and repeated experiment
demo = subset(demo, demo$subject != "S82819066");

#--------------------------------------------------------------------------------------------------------
#
#   Jan 14 
#   
#--------------------------------------------------------------------------------------------------------


data = subset(data, data$subject != "S38392608");   # Did experiment on his television?!?
demo = subset(demo, demo$subject != "S38392608");

data = subset(data, data$subject != "S30552798");   # Jan 14 - said severe floaters in one eye
demo = subset(demo, demo$subject != "S30552798");

#--------------------------------------------------------------------------------------------------------
#
#   Jan 16 
#   
#--------------------------------------------------------------------------------------------------------


data = subset(data, data$subject != "S41326220");    # somehow repeated the experiment from the same IP, Jan 16
demo = subset(demo, demo$subject != "S41326220");
data = subset(data, data$subject != "S39492032");   
demo = subset(demo, demo$subject != "S39492032");

#--------------------------------------------------------------------------------------------------------
#
#   Jan 17
#   
#--------------------------------------------------------------------------------------------------------


data = subset(data, data$subject != "S10828895");   # reported that he could highlighted a dot, so he always knew which one it was, Jan 17
demo = subset(demo, demo$subject != "S10828895");


#--------------------------------------------------------------------------------------------------------
#
#   Jan 21
#   
#--------------------------------------------------------------------------------------------------------


data = subset(data, data$subject !="S44761170"); # Jan 21 - incoherent
demo = subset(demo, demo$subject !="S44761170");

#data = subset(data, data$subject !="S35346714"); # Jan 21 - tracked only one-two balls
#demo = subset(demo, demo$subject !="S35346714");

#data = subset(data, data$subject !="S75099320"); # Jan 21 - tracked three then guessed
#demo = subset(demo, demo$subject !="S75099320");

#data = subset(data, data$subject !="S89635714");  # Jan 21 - tracked three then guessed
#demo = subset(demo, demo$subject !="S89635714");

#data = subset(data, data$subject !="S80018065");  # Jan 21 - almost ignored the 4th 
#demo = subset(demo, demo$subject !="S80018065");

#--------------------------------------------------------------------------------------------------------
#
#   Jan 30
#   
#--------------------------------------------------------------------------------------------------------


data = subset(data, data$subject !="S2008290"); 
demo = subset(demo, demo$subject !="S2008290");  # Jan 30 - siad this: 
                                                 # the background changed between a closer up of the laptop, and one more far away with the screen being smaller.


data = subset(data, data$subject !="S72925333"); # Seemed like next to last trial may have lagged a bit.
demo = subset(demo, demo$subject !="S72925333");

data = subset(data, data$subject !="S48923125"); # had a touch screen
demo = subset(demo, demo$subject !="S48923125"); 

data = subset(data, data$subject !="S18032030");
demo = subset(demo, demo$subject !="S18032030"); # incoherent


data = subset(data, data$subject !="S55878414"); # incoherent
demo = subset(demo, demo$subject !="S55878414");

data = subset(data, data$subject != "S20771601"); # ??
demo = subset(demo, demo$subject != "S20771601");

#--------------------------------------------------------------------------------------------------------
#
#   Plot accuracy for each subject with error bars, before excluding any optional exclusions
#
#--------------------------------------------------------------------------------------------------------


#d = subset(data, data$date == "11/January/2019");    # subset the data, if looking at only a specific date 

d = data;

aggdata <-aggregate(d$respcorrect, by=list(d$condition, d$subject), FUN = function(x) c(mean = mean(x), se = sd(x)/sqrt(length(x)) ))
colnames(aggdata) <- c("condition", "subject", "acc");
aggdata$accuracy = aggdata$acc[,1];
aggdata$SE = aggdata$acc[,2];

tempdemo = subset(aggdata[ , c("subject", "accuracy", "SE")], aggdata$condition == "stable");
colnames(tempdemo) <- c("subject", "accstable", "SEstable");
tempdemo = merge(tempdemo, subset(aggdata[ , c("subject", "accuracy", "SE")], aggdata$condition == "unstable"), by="subject");
colnames(tempdemo) <- c("subject", "accstable", "SEstable", "accunstable", "SEunstable");

ggplot(tempdemo, aes(x = accstable, y = accunstable)) + geom_point()  + xlim(0,1) + ylim(0,1) + geom_vline(xintercept = minimal_accuracy_cutoff, color="blue") + geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed") + geom_errorbar(aes(ymin = accunstable-SEunstable, ymax= accunstable + SEunstable)) + xlab("Accuracy, stable condition")+ ylab("Accuracy, unstable condition")


##--------------------------------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------------------------
##
##  Optional exclusions, for subjects did too badly (and we could do the same for subjects who did too well)
##
##--------------------------------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------------------------------
#
#  exclude the ones with accuracy below minimum
#
#--------------------------------------------------------------------------------------------------------

#demo = demo[ ,c("subject", "velocity", "accstable", "accunstable", "numstable", "numunstable", "age", "gender", "date") ];

lousy = subset(demo, demo$accstable <= minimal_accuracy_cutoff);

if (length(lousy$subject) > 0) {
  for (i in 1: length(lousy$subject)) {
	cat("removing poor accuracy ", as.character(lousy$subject[i]), demo$accstable[which(demo$subject == lousy$subject[i])],  demo$accunstable[which(demo$subject == lousy$subject[i])], "\n");
	data = subset(data, data$subject != lousy$subject[i]);
	demo = subset(demo, demo$subject != lousy$subject[i]);
  }
}

#-------------------------------------------------------------------------------------------
#
#  Accuracy analysis based on object-based accuracy (number of objects selected correctly per trial)
#
#-------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------
#
#   Removing participants who are too close to celling (or chance) on their object-based accuracy measures
#
#-------------------------------------------------------------------------------------------

#d <- data for testing
#dem <- demo
#d = subset(data, data$date == "30/January/2019") if checking subsets

avgdata <-aggregate(data, by=list(data$subject, data$condition), FUN = function(x) c(mean = mean(x) ))
avgdata <- avgdata[ c(1, 2, 7) ] # the columns we're interested in
colnames(avgdata) <- c( "subject", "condition", "numcorrect");

# MARTA: commented out, because, 
# we want to remove folks who may be at celling, or at chance, not based on how good they are relating to other subjects, right?
# perc_rank = mutate(avgdata, percentile_rank = ntile(avgdata$numcorrect, 100)); # assigns percentile scores to each average
# percent_cap = quantile(avgdata$numcorrect, probs = 0.95); # returns the score correlating to the 95th percentile
# percent_cap_low = quantile(avgdata$numcorrect, probs = c(0.05)); # returns the score correlating to the 5th percentile
# tooGood = subset(avgdata, avgdata$numcorrect > percent_cap); # list of subjects whose scores in either condition are above the 95th percentile
# tooBad = subset(avgdata, avgdata$numcorrect < percent_cap_low); #list of subjects whose scores in either condition are below the 5th percentile

avgdata$precentcorrect = avgdata$numcorrect/4; 
tooGood = subset(avgdata, avgdata$precentcorrect > 0.9);
tooBad =  subset(avgdata, avgdata$precentcorrect < 0.55);


cat("Total subjects:", dim(avgdata)[1], " of which",  dim(tooGood)[1], " are above 90 percent accurate on object-based acuracy, and ", dim(tooBad)[1],  " below 55 percent accurate \n");
demo = subset(demo, !( demo$subject %in% tooGood$subject) );
demo = subset(demo, !( demo$subject %in% tooBad$subject) );
data = subset(data, !( data$subject %in% tooGood$subject) );
data = subset(data, !( data$subject %in% tooBad$subject) );
avgdata = subset(avgdata, !( avgdata$subject %in% tooBad$subject) );
avgdata = subset(avgdata, !( avgdata$subject %in% tooGood$subject) );

#-------------------------------------------------------------------------------------------
#
#   apending object-based accuracy measures to demo
#
#-------------------------------------------------------------------------------------------


objaccstable = subset(avgdata, avgdata$condition=="stable");
colnames(objaccstable) <- c( "subject", "condition", "objcorrectstable", "objprecentstable");
objaccunstable = subset(avgdata, avgdata$condition=="unstable");
colnames(objaccunstable) <- c( "subject", "condition", "objcorrectunstable", "objprecentunstable");

demo =  merge(demo, objaccstable[, c("subject", "objcorrectstable", "objprecentstable")], by  = "subject");
demo =  merge(demo, objaccunstable[, c("subject", "objcorrectunstable", "objprecentunstable")], by  = "subject");

##--------------------------------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------------------------
##
##  Data Analysis
##
##--------------------------------------------------------------------------------------------------------
##--------------------------------------------------------------------------------------------------------


cat ( length(demo$subject), "subjects in the analysis");


#--------------------------------------------------------------------------------------------------------
#
#   Is there an effect for all 36 trials? - Trial-based measures
#
#--------------------------------------------------------------------------------------------------------

# subset the data if looking at a specific date
#  t.test( subset(demo$accstable, demo$date == "09/January/2019"), subset(demo$accunstable, demo$date == "09/January/2019"), paired=TRUE);
#  t.test( subset(demo$accstable, demo$date == "11/January/2019"), subset(demo$accunstable, demo$date == "11/January/2019"), paired=TRUE);


t = t.test(demo$accstable, demo$accunstable, paired=TRUE);                                 #  Withis-subject t-test for accuracy in the stable and unstable condition
t

power.t.test(delta=t$estimate, sd = sd(demo$accstable-demo$accunstable), sig.level=0.05, power = 0.8, type = "paired")  #  Power the t-test: how many subjects does it take?

num = length(demo$subject);
p = prop.test( c(sum(demo$numstable), sum(demo$numunstable)), c( num*18, num*18) );        #  Proportions for accuracy in the stable and unstable condition
     
power.prop.test(n=NULL, p1 = p$estimate[1], p2 = p$estimate[2], sig.level = 0.05, power = 0.8   )  #  Proportions for accuracy in the stable and unstable condition

#--------------------------------------------------------------------------------------------------------
#
#   Is there an effect for all 36 trials? - Object-based measures
#
#--------------------------------------------------------------------------------------------------------


t = t.test(demo$objcorrectstable, demo$objcorrectunstable, paired=TRUE); 
power.t.test(delta=t$estimate, sd = sd(demo$objcorrectstable - demo$objcorrectunstable), sig.level=0.05, power = 0.8, type = "paired")  #  Power the t-test: how many subjects does it take?

# now check by dates 
demo1617 = subset(demo, demo$date == '17/January/2019' | demo$date == '16/January/2019')    # no difference
t = t.test(demo1617$objcorrectstable, demo1617$objcorrectunstable, paired=TRUE); 
t

demo1421 = subset(demo, demo$date == '14/January/2019' | demo$date == '21/January/2019')    # 0.05
t = t.test(demo1421$objcorrectstable, demo1421$objcorrectunstable, paired=TRUE); 
t
power.t.test(delta=t$estimate, sd = sd(demo1421$objcorrectstable - demo1421$objcorrectunstable), sig.level=0.05, power = 0.8, type = "paired")  

demo142130 = subset(demo, demo$date == '14/January/2019' | demo$date == '21/January/2019' | demo$date == '30/January/2019' )    # 0.067, p-value = 0.1, df = 40, but n = 117
t = t.test(demo142130$objcorrectstable, demo142130$objcorrectunstable, paired=TRUE); 
t
power.t.test(delta=t$estimate, sd = sd(demo142130$objcorrectstable - demo142130$objcorrectunstable), sig.level=0.05, power = 0.8, type = "paired")  

#--------------------------------------------------------------------------------------------------------
#
#   is there an effect for the first 30 trials? Trial-based measures
#
#--------------------------------------------------------------------------------------------------------

n = 30;                                       
d = subset(data, data$trial <= n);

aggdata <-aggregate(d$respcorrect, by=list(d$condition, d$subject), FUN = function(x) c(mean = mean(x), se = sd(x)/sqrt(length(x)) ))
colnames(aggdata) <- c("condition", "subject", "acc");
aggdata$accuracy = aggdata$acc[,1];
aggdata$SE = aggdata$acc[,2];
aggdata$condition = factor(aggdata$condition);
res = lm(data = aggdata, accuracy ~ condition);
TukeyHSD(aov(res));

t = t.test(subset(aggdata$accuracy, aggdata$condition == "stable"), subset(aggdata$accuracy, aggdata$condition == "unstable"), paired = TRUE)
t

power.t.test(delta=t$estimate, sd = sd(subset(aggdata$accuracy, aggdata$condition == "stable") - subset(aggdata$accuracy, aggdata$condition == "unstable")), sig.level=0.05, power = 0.8, type = "paired")  



#--------------------------------------------------------------------------------------------------------
#
#   is there an effect for the first 30 trials? Object-based measures
#
#--------------------------------------------------------------------------------------------------------

n = 30;                                       
d = subset(data, data$trial <= n);

avgdata <-aggregate(d, by=list(d$subject, d$condition), FUN = function(x) c(mean = mean(x) ))
avgdata <- avgdata[ c(1, 2, 7) ] # the columns we're interested in
colnames(avgdata) <- c( "subject", "condition", "numcorrect");
objaccstable = subset(avgdata, avgdata$condition=="stable");
colnames(objaccstable) <- c( "subject", "condition", "objcorrectstable");
objaccunstable = subset(avgdata, avgdata$condition=="unstable");
colnames(objaccunstable) <- c( "subject", "condition", "objcorrectunstable");

tempDemo =  merge(objaccstable, demo[, c("subject", "date")], by  = "subject");
tempDemo =  merge(tempDemo, objaccunstable[, c("subject", "objcorrectunstable")], by  = "subject");

t = t.test(tempDemo $objcorrectstable, tempDemo $objcorrectunstable, paired=TRUE); 
power.t.test(delta=t$estimate, sd = sd(tempDemo $objcorrectstable - tempDemo $objcorrectunstable), sig.level=0.05, power = 0.8, type = "paired") 
t


# now check by dates 
demo1617 = subset(tempDemo, tempDemo $date == '17/January/2019' | tempDemo $date == '16/January/2019')    # no difference
t = t.test(demo1617$objcorrectstable, demo1617$objcorrectunstable, paired=TRUE); 
t

demo1421 = subset(tempDemo, tempDemo $date == '14/January/2019' | tempDemo$date == '21/January/2019')    # 0.075, p=.2
t = t.test(demo1421$objcorrectstable, demo1421$objcorrectunstable, paired=TRUE); 
t
power.t.test(delta=t$estimate, sd = sd(demo1421$objcorrectstable - demo1421$objcorrectunstable), sig.level=0.05, power = 0.8, type = "paired")  


#--------------------------------------------------------------------------------------------------------
#
#   This one is finally significant
#
#--------------------------------------------------------------------------------------------------------



demo142130 = subset(tempDemo, tempDemo $date == '14/January/2019' | tempDemo $date == '21/January/2019' | tempDemo $date == '30/January/2019' )    # 0.098, p-value = 0.04, df = 40, but n = 49
t = t.test(demo142130$objcorrectstable, demo142130$objcorrectunstable, paired=TRUE); 
t
power.t.test(delta=t$estimate, sd = sd(demo142130$objcorrectstable - demo142130$objcorrectunstable), sig.level=0.05, power = 0.8, type = "paired")  


#--------------------------------------------------------------------------------------------------------
#
#   accuracy for each trial - are they getting better over time?
#
#--------------------------------------------------------------------------------------------------------

aggdata <-aggregate(data$respcorrect, by=list(data$trial), FUN = function(x) c(mean = mean(x), se = sd(x)/sqrt(length(x)) ))
colnames(aggdata) <- c("trial", "acc");
aggdata$accuracy = aggdata$acc[,1];
aggdata$SE = aggdata$acc[,2];
ggplot(aggdata, aes(x= trial, y = accuracy)) + geom_point() + geom_smooth(method = 'lm') + geom_errorbar(aes(ymin = accuracy-SE, ymax= accuracy + SE))

res = lm(data= aggdata, accuracy ~ trial);      # linear model, is the regression line significant?
summary(res);


#--------------------------------------------------------------------------------------------------------
#
#   accuracy for each bin by condition -- Trial based
#
#--------------------------------------------------------------------------------------------------------

data$bin = 5
data$bin[which(data$trial <= 6 )] =  1;
data$bin[which(data$trial > 30 )] =  6;
data$bin[which(data$trial >= 7  & data$trial < 13 )] = 2;
data$bin[which(data$trial >= 13  & data$trial < 19 )] = 3;
data$bin[which(data$trial >= 19  & data$trial < 25 )] = 4;

aggdata <-aggregate(data$respcorrect, by=list(data$bin, data$condition),  FUN = function(x) c(mean = mean(x), se = sd(x)/sqrt(length(x)) ))
colnames(aggdata) <- c("bin", "condition", "acc");
aggdata$accuracy = aggdata$acc[,1];
aggdata$SE = aggdata$acc[,2];
ggplot(aggdata, aes(x= bin, y = accuracy, colour = condition)) + geom_point() + geom_errorbar(aes(ymin = accuracy-SE, ymax= accuracy +SE)) + geom_smooth(method = 'lm')




data$bin =  factor(data$bin)                                           # LM and  ANOVA - are bins different from each other ?
res = lm(data = aggdata, accuracy ~ as.numeric(bin) + condition )
summary(res);

res = lm(data = aggdata, accuracy ~ bin + condition )
TukeyHSD(aov(res))



#--------------------------------------------------------------------------------------------------------
#
#   accuracy for each bin by condition -- Object based -- and only Jan 14, 21, 30 --- SEM overlap...
#
#--------------------------------------------------------------------------------------------------------

data$bin = 5
data$bin[which(data$trial <= 6 )] =  1;
data$bin[which(data$trial > 30 )] =  6;
data$bin[which(data$trial >= 7  & data$trial < 13 )] = 2;
data$bin[which(data$trial >= 13  & data$trial < 19 )] = 3;
data$bin[which(data$trial >= 19  & data$trial < 25 )] = 4;


d = subset(data, data$date == '14/January/2019' | data$date == '21/January/2019' | data$date == '30/January/2019' )   

avgdata <-aggregate(d$numcorrect, by=list(d$bin, d$condition), FUN = function(x) c(mean = mean(x), se = sd(x)/sqrt(length(x)) ))
colnames(avgdata) <- c( "bin", "condition", "acc");
avgdata$objaccuracy = avgdata $acc[,1];
avgdata$SE = avgdata $acc[,2];
ggplot(avgdata, aes(x= bin, y = objaccuracy, colour = condition)) + geom_point() + geom_errorbar(aes(ymin = objaccuracy-SE, ymax= objaccuracy +SE)) + geom_smooth(method = 'lm')


#--------------------------------------------------------------------------------------------------------
#
#  ANOVA of accuracy for each background, first 30 trials  ---- be careful not to merge data from Jan 30, whcih uses a different set of backgrounds
#
#--------------------------------------------------------------------------------------------------------

backgrounds = c("stable-laptop-1-hd.jpg", "stable-laptop-2-hd.jpg", "stable-laptop-3-hd.jpg", "unstable-laptop-3-hd.jpg", "unstable-laptop-2-hd.jpg", "unstable-laptop-1-hd.jpg" );
bf = data.frame(background = backgrounds, bkg = c(1,2,3,4,5,6))

n = 30;
d = subset(data, data$trial <= n );
d$background = "";

for (i in 1: length(bf$bkg)) {
	d$background[which(d$bkgid == bf$bkg[i])] = as.character(bf$background[i])
}

aggdata <-aggregate(d$respcorrect, by=list(d$background, d$subject), FUN = function(x) c(mean = mean(x), se = sd(x)/sqrt(length(x)) ))
colnames(aggdata) <- c("bkg", "subject", "acc");
aggdata$accuracy = aggdata$acc[,1];
aggdata$SE = aggdata$acc[,2];
aggdata$bkg = factor(aggdata$bkg);
res = lm(data = aggdata, accuracy~ bkg);
TukeyHSD(aov(res));


#--------------------------------------------------------------------------------------------------------
#
#   reaction times for correct and incorrect trials
#
#--------------------------------------------------------------------------------------------------------

ggplot(data = data, aes(x = rt1, fill = respcorrect) ) + geom_density(alpha = 0.5) + xlab("RT on first click")  + xlim(0,3000)
t.test(subset(data$rt1, data$respcorrect == TRUE), subset(data$rt1, data$respcorrect == FALSE), var.equal = TRUE );

#--------------------------------------------------------------------------------------------------------
#
#   reaction times for stable and unstable trials
#
#--------------------------------------------------------------------------------------------------------

ggplot(data = data, aes(x = rt1, fill = condition) ) + geom_density(alpha = 0.5) + xlab("RT on first click") + xlim(0,3000)
t.test(subset(data$rt1, data$condition == "stable"), subset(data$rt1, data$condition == "unstable"), var.equal = TRUE ); # NS


#--------------------------------------------------------------------------------------------------------
#
#   reaction times for first, second, third, last balls - only the first ball is different
#
#--------------------------------------------------------------------------------------------------------

df1 = data[ , c("trial", "condition", "rt1",  "respcorrect", "subject")]; colnames(df1)[3] <- "rt"; df1$whichball = "1";
df2 = data[ , c("trial", "condition", "rt2",  "respcorrect", "subject")]; colnames(df2)[3] <- "rt"; df2$whichball = "2";
df3 = data[ , c("trial", "condition", "rt3",  "respcorrect", "subject")]; colnames(df3)[3] <- "rt"; df3$whichball = "3";
df4 = data[ , c("trial", "condition", "rt4",  "respcorrect", "subject")]; colnames(df4)[3] <- "rt"; df4$whichball = "4";
df = rbind(df1, df2);
df = rbind(df, df3);
df = rbind(df, df4);

ggplot(data = df, aes(x = rt, fill = whichball) ) + geom_density(alpha = 0.5) + xlim(0, 3000)
ggplot(data = df, aes(x = rt, fill = condition) ) + geom_density(alpha = 0.5) + xlim(0, 3000)


demoJan9 =  demo;
dataJan9 = data;

demoJan11 =  demo;
dataJan11 = data;

