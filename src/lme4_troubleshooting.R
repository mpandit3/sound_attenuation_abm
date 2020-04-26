###################################################################
##########lme4 convergence warnings: troubleshooting###############
###################################################################

library(lme4)

###Run Models from stats_analys.R script


# Rescale and center continuous parameters --------------------------------

numcols <- grep("^c\\.",names(data2))
dfs <- data2
dfs[,numcols] <- scale(dfs[,numcols])
M4_sc <- update(M4,data=dfs)
summary(M4_sc)
anova(M4_sc)

# #Checking Singularity ---------------------------------------------------

tt <- getME(M4_sc,"theta")
ll <- getME(M4_sc,"lower")
min(tt[ll==0])

#theta = 0.1696894


# Double-checking gradient calculations -----------------------------------


# Restart -----------------------------------------------------------------


# Try a different optimizer -----------------------------------------------


