####Ex 1 Analyses####
size = read.csv("Ex 1 Scored Output/Size.csv")
highlight = read.csv("Ex 1 Scored Output/Highlight.csv")

##Get sample size
length(unique(size$Username))
length(unique(highlight$Username))

colnames(size)[7] = "Direction"
colnames(highlight)[7] = "Direction"

##Remove punctuation/special characters from JOLs
size$Response.JOL =  gsub("[[:punct:]]", "", size$Response.JOL)
size$Response.JOL =  gsub("I", NA, size$Response.JOL)

#Make Size jols numeric
size$Response.JOL = as.numeric(size$Response.JOL)

##Get descriptives
summary(size)
summary(highlight)

#Remove out of range JOLs
size$Response.JOL[size$Response.JOL > 100] = NA
highlight$Response.JOL[highlight$Response.JOL > 100] = NA

##Get means
##JOLs
tapply(size$Response.JOL, size$Direction, mean, na.rm = T)
tapply(highlight$Response.JOL, highlight$Direction, mean, na.rm = T)

##Recall
tapply(size$Recall_Score, size$Direction, mean, na.rm = T)
tapply(highlight$Recall_Score, highlight$Direction, mean, na.rm = T)

##Now look at each manipulation
tapply(size$Response.JOL, list(size$Procedure.Trial.Type, size$Direction), mean, na.rm = T)
tapply(size$Recall_Score, list(size$Procedure.Trial.Type, size$Direction), mean, na.rm = T)

tapply(highlight$Response.JOL, list(highlight$Procedure.Trial.Type, highlight$Direction), mean, na.rm = T)
tapply(highlight$Recall_Score, list(highlight$Procedure.Trial.Type, highlight$Direction), mean, na.rm = T)
