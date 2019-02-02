

library(ggplot2);
library(pwr);
library(Hmisc);      # includes rcorr

#------------------------------------------------------------------------------------------------
#
#     Student project exploratory analysis
#
#------------------------------------------------------------------------------------------------


# please substitute your path
main_path  = "/Users/luckyfish/Desktop/MOT/";  

# the file you sent me
data = read.csv( paste(main_path, 'student_project/MOTProject.csv', sep ='') , sep = ",", strip.white=TRUE, header=TRUE);  

# postprocessing
data$respcorrect = (data$num_TargetsClicked == 4);
data$stability[which(data$background == 5)] = "unstable";

# calucating each subjects accuracy for each background
aggdata <-aggregate(data$respcorrect, by=list(data$background, data$SubjID), FUN=mean)
colnames(aggdata) <- c("bkg", "subject", "accuracy");

# ANOVA if any background is different -- but no pairwise comparisons are significant
aggdata$bkg = factor(aggdata$bkg);
res = lm(data = aggdata, accuracy~ bkg);
TukeyHSD(aov(res));

# calucating accuracy for each trial
aggdata <-aggregate(data$respcorrect, by=list(data$Trial), FUN=mean)
colnames(aggdata) <- c("trial", "accuracy");

# plot regression, which is not significant
ggplot(aggdata, aes(x= trial, y = accuracy)) + geom_point() + geom_smooth(method = 'lm')
res = lm(data= aggdata, accuracy ~ trial);
summary(res);

# calucating accuracy for each bin ...
data$bin = 4
data$bin[which(data$Trial <= 8 )] =  1;
data$bin[which(data$Trial > 32 )] =  5;
data$bin[which(data$Trial >= 9  & data$Trial < 17 )] = 2;
data$bin[which(data$Trial >= 17  & data$Trial < 25 )] = 3;

aggdata <-aggregate(data$respcorrect, by=list(data$bin),  FUN = function(x) c(mean = mean(x), se = sd(x)/sqrt(length(x)) ))
colnames(aggdata) <- c("bin", "acc");
aggdata$accuracy = aggdata$acc[,1];
aggdata$SE = aggdata$acc[,2];
ggplot(aggdata, aes(x= bin, y = accuracy)) + geom_point() + geom_errorbar(aes(ymin = accuracy-SE, ymax= accuracy +SE), position = "dodge") + geom_smooth(method = 'lm')




