

library(ggplot2);
library(pwr);
library(Hmisc);      # includes rcorr

# ---------------------------------------------------------------------------------------------------
#
#  Pilot analysis, December 6 2018
#
# ---------------------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------------------
#
#  Loading turk logs
#  Always check that the path to file is correct 
#
# ---------------------------------------------------------------------------------------------------

main_path = '/Users/luckyfish/Desktop/MOT/MOTDec6/';        

df = read.csv( paste(main_path, 'MOT.csv', sep ='') , sep = "\t", strip.white=TRUE, header=TRUE); 


# ---------------------------------------------------------------------------------------------------
#
#  For pilot, the exclusion criteria is in the .csv
#
# ---------------------------------------------------------------------------------------------------


df = subset(df, df$exclude != 1);

# ---------------------------------------------------------------------------------------------------
#
#  Get rid of unwanted columns
#
# ---------------------------------------------------------------------------------------------------


df = df[, c("subject", "stable", "correct_stable", "unstable", "correct_unstable", "velocity", "age", "gender")];

# ---------------------------------------------------------------------------------------------------
#
#  Withis-subject t-test for accuracy in the stable and unstable condition
#
# ---------------------------------------------------------------------------------------------------


t = t.test(df$stable, df$unstable, paired = TRUE)
t


# ---------------------------------------------------------------------------------------------------
#
#  Power the t-test: how many subjects does it take?
#
# ---------------------------------------------------------------------------------------------------


power.t.test(delta=t$estimate, sd = sd(df$stable-df$unstable), sig.level=0.05, power = 0.8, type = "paired")   # n=6



# ---------------------------------------------------------------------------------------------------
#
#   Is there an effect for individual subjects?
#
# ---------------------------------------------------------------------------------------------------




for (  i in 1 : dim(df)[1] ) {
	
	pp = prop.test( c(df$correct_stable[i], df$correct_unstable[i]), c(15, 15) );
	
	if (pp$estimate[1] > 0.5) {
	  diff = pp$estimate[1] -  pp$estimate[2];
	  cat( "p:", pp$p.value, " prop1:", pp$estimate[1], " prop2:", pp$estimate[2], " prop.diff:", diff, "\n");
	}
}

# The difference between conditions for each subject is usually about 0.13333 (2 trials out of 15); 
# How many trials do they need to do for this difference to be significant?

power.prop.test(n=NULL, p1 = 14/15, p2 = (14 - 2)/15, sig.level = 0.05, power = 0.8   )

# Looks like they'd have to do 101 trials

n = 17*6/2; # 51
prop.test( c(n-1, round(n-1 - (n*0.1333333)) ), c(n, n) );


