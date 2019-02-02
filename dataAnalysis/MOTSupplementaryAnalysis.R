
#--------------------------------------------------------------------------------------------------------
#
#   Plot accuracy against calibraion velocity - useful when debugging calibration
#
#--------------------------------------------------------------------------------------------------------


#ggplot(demo, aes(x = accstable, y = velocity)) + geom_point() + geom_smooth(method='lm')	



#--------------------------------------------------------------------------------------------------------
#
#   Object-based accuracy measures
#
#--------------------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------------
#
#  fraction of stable and unstable objects that each subject clicked
#
#--------------------------------------------------------------------------------------------------------

aggdata <-aggregate(data$numcorrect, by=list(data$subject, data$condition), FUN = function(x) c(mean = mean(x), se = sd(x)/sqrt(length(x)) ))
colnames(aggdata) <- c("subject", "condition", "x");
aggdata$numobj = aggdata$x[,1];
aggdata$SE = aggdata$x[,2];

#NS = p = .6
t.test(subset(aggdata$numobj, aggdata$condition == "stable"), subset(aggdata$numobj, aggdata$condition == "unstable"), paired=TRUE)



# lets try by date
aggdata = merge(aggdata, demo[, c("subject", "date")], by  = "subject");


# plot all of them
tempdemo = subset(aggdata[ , c("subject", "numobj", "SE")], aggdata$condition == "stable");
colnames(tempdemo) <- c("subject", "numobj", "SEstable");
tempdemo = merge(tempdemo, subset(aggdata[ , c("subject", "numobj", "SE")], aggdata$condition == "unstable"), by="subject");
colnames(tempdemo) <- c("subject", "numobjstable", "SEstable", "numobjunstable", "SEunstable");

tempdemo =  merge(tempdemo, demo[, c("subject", "date")], by  = "subject");


ggplot(subest(tempdemo, tempdemo$numobjstable < 3.6 & tempdemo$numobjunstable < 3.6), aes(x = numobjstable, y = numobjunstable, colour=date)) + geom_point()  + xlim(2,4) + ylim(2,4) + geom_vline(xintercept = 1, color="blue") + geom_abline(intercept = 0, slope = 1, color="red", linetype="dashed") + geom_errorbar(aes(ymin = numobjunstable-SEunstable, ymax= numobjunstable + SEunstable)) + xlab("Objects, stable condition")+ ylab("Objects, unstable condition")


temp = subset(tempdemo, tempdemo$date == "21/January/2019" & tempdemo $numobjstable < 3.6 & tempdemo$numobjunstable < 3.6);
t = t.test(temp$numobjstable, temp$numobjunstable, paired=TRUE)
power.t.test(delta=t$estimate, sd = sd(temp$numobjstable - temp$numobjunstable), sig.level=0.05, power = 0.8, type = "paired")





#--------------------------------------------------------------------------------------------------------
#
#  metaanalysis and troubleshooting
#
#--------------------------------------------------------------------------------------------------------


# metaanalysis

demoBK = demo;
dataBK = data;

demo = subset(demoBK, (demoBK$date == "21/January/2019" | demoBK$date == "14/January/2019") & demoBK$calibh >= 760)
data = merge(dataBK, demo[ , c("calibh", "subject")], by = "subject")

demo = subset(demoBK, demoBK$date == "21/January/2019" & demoBK$calibh >= 770)
data = merge(dataBK, demo[ , c("calibh", "subject")], by = "subject")




# troubleshooting

demo  = demo[ , !(names(demo) %in% c("browser"))]
demo$diff = demo$accstable-demo$accunstable
ggplot(data=demo, aes(x=diff, y = calibh)) + geom_point() + geom_smooth(method ='lm')
res = lm(data=demo, diff~calibh + calibw)
ggplot(data=demo, aes(x=diff, y = durationMOT)) + geom_point() + geom_smooth(method ='lm')
res = lm(data=demo, diff~ durationMOT)

demom = demo

demom$display = '<700'
demom$display[which(demom$calibh > 700 & demom$calibh < 800)] = '700-800'
demom$display[which(demom$calibh > 800 & demom$calibh < 900)] = '800-900'
demom$display[which(demom$calibh > 900 )] = '>900'
aggdata=aggregate(demom$diff, by=list(demom$display), FUN = function(x) c(mean = mean(x), se = sd(x)/sqrt(length(x)) ))
colnames(aggdata) <- c("display", "eff");
aggdata$effect = aggdata$eff[,1];
aggdata$SE = aggdata$eff[,2];
dodge <- position_dodge(width = 0.9);
ggplot(aggdata, aes(x = display, y = effect))+ geom_bar(stat = "identity", position = dodge) + geom_errorbar(aes(ymin = effect-SE, ymax= effect +SE), position = "dodge")


#--------------------------------------------------------------------------------------------------------
#
#   this code generates the 'Reps' plot in our MOT share latex 
#
#--------------------------------------------------------------------------------------------------------


effects = c(0.12,  0.07,   0.0,   0.086,  -0.06,  0.025,   0.015,    0.032);
conflo =  c(0.062, 0.001, -0.07,  0.025, -0.15, -0.055, -0.044, -0.03);
confhi =  c(0.18,  0.14,   0.07,  0.15,  0.001,  0.1,    0.078,   0.095);
reps = c(0, 1, 2, 3, 4, 5, 6, 7);

ggplot(data.frame(eff= effects, conflo= conflo, confhi= confhi, reps= reps  ), aes(x = reps, y = effects))+ geom_bar(stat = "identity", position = dodge) + geom_errorbar(aes(ymin = conflo, ymax= confhi), position = "dodge")




