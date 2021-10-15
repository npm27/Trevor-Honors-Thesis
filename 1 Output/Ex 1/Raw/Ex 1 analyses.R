####Ex 1 Analyses####
size = read.csv("Ex 1 Scored Output/Size.csv")
highlight = read.csv("Ex 1 Scored Output/Highlight.csv")
control = read.csv("JOL3.csv")

library(ez)
library(data.table)
library(reshape)
library(Hmisc)

options(scipen = 999)

##Get sample size
length(unique(size$Username))
length(unique(highlight$Username))
length(unique(control$Username))

colnames(size)[7] = "Direction"
colnames(highlight)[7] = "Direction"
colnames(control)[7] = "Direction"

##Remove punctuation/special characters from JOLs
size$Response.JOL =  gsub("[[:punct:]]", "", size$Response.JOL)
size$Response.JOL =  gsub("I", NA, size$Response.JOL)

#Make Size jols numeric
size$Response.JOL = as.numeric(size$Response.JOL)

##Get descriptives
summary(size)
summary(highlight)
summary(control)

#Remove out of range JOLs
size$Response.JOL[size$Response.JOL > 100] = NA
highlight$Response.JOL[highlight$Response.JOL > 100] = NA
control$Response.JOL[control$Response.JOL > 100] = NA

##Get means
##JOLs
tapply(size$Response.JOL, size$Direction, mean, na.rm = T)
tapply(highlight$Response.JOL, highlight$Direction, mean, na.rm = T)
tapply(control$Response.JOL, control$Direction, mean, na.rm = T)

##Recall
tapply(size$Recall_Score, size$Direction, mean, na.rm = T)
tapply(highlight$Recall_Score, highlight$Direction, mean, na.rm = T)
tapply(control$Recall_Score, control$Direction, mean, na.rm = T)

##Now look at each manipulation
##Size
tapply(size$Response.JOL, list(size$Procedure.Trial.Type, size$Direction), mean, na.rm = T)
tapply(size$Recall_Score, list(size$Procedure.Trial.Type, size$Direction), mean, na.rm = T)

##Highlight
tapply(highlight$Response.JOL, list(highlight$Procedure.Trial.Type, highlight$Direction), mean, na.rm = T)
tapply(highlight$Recall_Score, list(highlight$Procedure.Trial.Type, highlight$Direction), mean, na.rm = T)

##Control
tapply(control$Response.JOL, control$Direction, mean, na.rm = T)
tapply(control$Recall_Score, control$Direction, mean, na.rm = T)

####Write output to .csv####
##Need subject level means
##Start with Control
control2 = control[ , -c(2:6, 8, 10:12)]

control_jol = control2[ , -4]
control_recall = control2[ , -3]

##Get subject level means by direction
control_jol2 = cast(control_jol, Username ~ Direction, mean, na.rm = T)
control_recall2 = cast(control_recall, Username ~ Direction, mean, na.rm = T)

####Now do it again for size####
size2 = size[ , -c(2:6, 9, 11:13)]

size.small = subset(size2,
                    size2$Procedure.Trial.Type == "JOL_Small")
size.large = subset(size2,
                    size2$Procedure.Trial.Type == "JOL_Large")

size.small.jol = size.small[ , -c(3,5)]
size.small.recall = size.small[ , -c(3:4)]

size.large.jol = size.large[ , -c(3,5)]
size.large.recall = size.large[ , -c(3:4)]

size.small.jol2 = cast(size.small.jol, Username ~ Direction, mean, na.rm = T)
size.small.recall2 = cast(size.small.recall, Username ~ Direction, mean, na.rm = T)

size.large.jol2 = cast(size.large.jol, Username ~ Direction, mean, na.rm = T)
size.large.recall2 = cast(size.large.recall, Username ~ Direction, mean, na.rm = T)

####Now for highlights####
highlight2 = highlight[ , -c(2:6, 9, 11:13)]

highlight.yes = subset(highlight2,
                    highlight2$Procedure.Trial.Type == "JOL_H")
highlight.no = subset(highlight2,
                    highlight2$Procedure.Trial.Type == "JOL")

highlight.yes.jol = highlight.yes[ , -c(3,5)]
highlight.yes.recall = highlight.yes[ , -c(3:4)]

highlight.no.jol = highlight.no[ , -c(3,5)]
highlight.no.recall = highlight.no[ , -c(3:4)]

highlight.yes.jol2 = cast(highlight.yes.jol, Username ~ Direction, mean, na.rm = T)
highlight.yes.recall2 = cast(highlight.yes.recall, Username ~ Direction, mean, na.rm = T)

highlight.no.jol2 = cast(highlight.no.jol, Username ~ Direction, mean, na.rm = T)
highlight.no.recall2 = cast(highlight.no.recall, Username ~ Direction, mean, na.rm = T)

####Write everything to csv####
#write.csv(control_jol2, file = "EX 1 Cleaned/Control_JOL.csv", row.names = F)
#write.csv(control_recall2, file = "EX 1 Cleaned/Control_Recall.csv", row.names = F)
#write.csv(highlight.no.jol2, file = "EX 1 Cleaned/NO Highlight JOL.csv", row.names = F)
#write.csv(highlight.yes.jol2, file = "EX 1 Cleaned/Yes Highlight JOL.csv", row.names = F)
#write.csv(highlight.no.recall2, file = "EX 1 Cleaned/NO HIGHLIGHT Recall.csv", row.names = F)
#write.csv(highlight.yes.recall2, file = "EX 1 Cleaned/YES HIGHLIGHT RECALL.csv", row.names = F)
#write.csv(size.large.jol2, file = "EX 1 Cleaned/Large JOL.csv", row.names = F)
#write.csv(size.large.recall2, file = "EX 1 Cleaned/Large recall.csv", row.names = F)
#write.csv(size.small.jol2, file = "EX 1 Cleaned/Small JOL.csv", row.names = F)
#write.csv(size.small.recall2, file = "Ex 1 Cleaned/Small Recall.csv", row.names = F)

##Do gammas as a function of Encoding group and pair direction.

##Okay, I need 5 sets of gammas, with mean gammas for each pair type. So, 20 total (5 perceptual types * 4 pair directions)

control = na.omit(control)
size.large = na.omit(size.large)
size.small = na.omit(size.small)
highlight.yes = na.omit(highlight.yes)
highlight.no = na.omit(highlight.no)

####Control Gammas####
Control_F = subset(control, control$Direction == "F")
Control_B = subset(control, control$Direction == "B")
control_S = subset(control, control$Direction == "S")
control_U = subset(control, control$Direction == "U")

#Use loops to get each participant's mean gamma between JOLs and Recall
#Forward
empty = data.frame()

for (i in unique(Control_F$Username)){
  
  temp = subset(Control_F, Control_F$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_Control_F = empty

#Backward
empty = data.frame()

for (i in unique(Control_B$Username)){
  
  temp = subset(Control_B, Control_B$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_Control_B = empty

#Symmetrical
empty = data.frame()

for (i in unique(control_S$Username)){
  
  temp = subset(control_S, control_S$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_Control_S = empty

#Unrelated
empty = data.frame()

for (i in unique(control_U$Username)){
  
  temp = subset(control_U, control_U$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_Control_U = empty

##Okay, stick them all together!
Control_GAMMAS = cbind(Gammas_Control_F, Gammas_Control_B, Gammas_Control_S, Gammas_Control_U)

#Drop duplicate columns and rename
Control_GAMMAS = Control_GAMMAS[ , -c(3,5,7)]
colnames(Control_GAMMAS)[1:5] = c("Sub", "F", "B", "S", "U")

####Large font Gammas####
size.large_F = subset(size.large, size.large$Direction == "F")
size.large_B = subset(size.large, size.large$Direction == "B")
size.large_S = subset(size.large, size.large$Direction == "S")
size.large_U = subset(size.large, size.large$Direction == "U")

#Use loops to get each participant's mean gamma between JOLs and Recall
#Forward
empty = data.frame()

for (i in unique(size.large_F$Username)){
  
  temp = subset(size.large_F, size.large_F$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_size.large_F = empty

#Backward
empty = data.frame()

for (i in unique(size.large_B$Username)){
  
  temp = subset(size.large_B, size.large_B$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_size.large_B = empty

#Symmetrical
empty = data.frame()

for (i in unique(size.large_S$Username)){
  
  temp = subset(size.large_S, size.large_S$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_size.large_S = empty

#Unrelated
empty = data.frame()

for (i in unique(size.large_U$Username)){
  
  temp = subset(size.large_U, size.large_U$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_size.large_U = empty

##Okay, stick them all together!
size.large_GAMMAS = cbind(Gammas_size.large_F, Gammas_size.large_B, Gammas_size.large_S, Gammas_size.large_U)

#Drop duplicate columns and rename
size.large_GAMMAS = size.large_GAMMAS[ , -c(3,5,7)]
colnames(size.large_GAMMAS)[1:5] = c("Sub", "F", "B", "S", "U")

####Small Font Gammas####
size.small_F = subset(size.small, size.small$Direction == "F")
size.small_B = subset(size.small, size.small$Direction == "B")
size.small_S = subset(size.small, size.small$Direction == "S")
size.small_U = subset(size.small, size.small$Direction == "U")

#Use loops to get each participant's mean gamma between JOLs and Recall
#Forward
empty = data.frame()

for (i in unique(size.small_F$Username)){
  
  temp = subset(size.small_F, size.small_F$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_size.small_F = empty

#Backward
empty = data.frame()

for (i in unique(size.small_B$Username)){
  
  temp = subset(size.small_B, size.small_B$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_size.small_B = empty

#Symmetrical
empty = data.frame()

for (i in unique(size.small_S$Username)){
  
  temp = subset(size.small_S, size.small_S$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_size.small_S = empty

#Unrelated
empty = data.frame()

for (i in unique(size.small_U$Username)){
  
  temp = subset(size.small_U, size.small_U$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_size.small_U = empty

##Okay, stick them all together!
size.small_GAMMAS = cbind(Gammas_size.small_F, Gammas_size.small_B, Gammas_size.small_S, Gammas_size.small_U)

#Drop duplicate columns and rename
size.small_GAMMAS = size.small_GAMMAS[ , -c(3,5,7)]
colnames(size.small_GAMMAS)[1:5] = c("Sub", "F", "B", "S", "U")

###Highlight Gammas####
highlight.yes_F = subset(highlight.yes, highlight.yes$Direction == "F")
highlight.yes_B = subset(highlight.yes, highlight.yes$Direction == "B")
highlight.yes_S = subset(highlight.yes, highlight.yes$Direction == "S")
highlight.yes_U = subset(highlight.yes, highlight.yes$Direction == "U")

#Use loops to get each participant's mean gamma between JOLs and Recall
#Forward
empty = data.frame()

for (i in unique(highlight.yes_F$Username)){
  
  temp = subset(highlight.yes_F, highlight.yes_F$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_highlight.yes_F = empty

#Backward
empty = data.frame()

for (i in unique(highlight.yes_B$Username)){
  
  temp = subset(highlight.yes_B, highlight.yes_B$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_highlight.yes_B = empty

#Symmetrical
empty = data.frame()

for (i in unique(highlight.yes_S$Username)){
  
  temp = subset(highlight.yes_S, highlight.yes_S$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_highlight.yes_S = empty

#Unrelated
empty = data.frame()

for (i in unique(highlight.yes_U$Username)){
  
  temp = subset(highlight.yes_U, highlight.yes_U$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_highlight.yes_U = empty

##Okay, stick them all together!
highlight.yes_GAMMAS = cbind(Gammas_highlight.yes_F, Gammas_highlight.yes_B, Gammas_highlight.yes_S, Gammas_highlight.yes_U)

#Drop duplicate columns and rename
highlight.yes_GAMMAS = highlight.yes_GAMMAS[ , -c(3,5,7)]
colnames(highlight.yes_GAMMAS)[1:5] = c("Sub", "F", "B", "S", "U")

####NO Highlight gammas####
highlight.no_F = subset(highlight.no, highlight.no$Direction == "F")
highlight.no_B = subset(highlight.no, highlight.no$Direction == "B")
highlight.no_S = subset(highlight.no, highlight.no$Direction == "S")
highlight.no_U = subset(highlight.no, highlight.no$Direction == "U")

#Use loops to get each participant's mean gamma between JOLs and Recall
#Forward
empty = data.frame()

for (i in unique(highlight.no_F$Username)){
  
  temp = subset(highlight.no_F, highlight.no_F$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_highlight.no_F = empty

#Backward
empty = data.frame()

for (i in unique(highlight.no_B$Username)){
  
  temp = subset(highlight.no_B, highlight.no_B$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_highlight.no_B = empty

#Symmetrical
empty = data.frame()

for (i in unique(highlight.no_S$Username)){
  
  temp = subset(highlight.no_S, highlight.no_S$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_highlight.no_S = empty

#Unrelated
empty = data.frame()

for (i in unique(highlight.no_U$Username)){
  
  temp = subset(highlight.no_U, highlight.no_U$Username == i)
  
  g = rcorr.cens(temp$Response.JOL, temp$Recall_Score, outx = TRUE)[2]
  
  g = unname(g)
  
  temp2 = data.frame(i, g)
  
  empty = rbind(temp2, empty)
  
}

Gammas_highlight.no_U = empty

##Okay, stick them all together!
highlight.no_GAMMAS = cbind(Gammas_highlight.no_F, Gammas_highlight.no_B, Gammas_highlight.no_S, Gammas_highlight.no_U)

#Drop duplicate columns and rename
highlight.no_GAMMAS = highlight.no_GAMMAS[ , -c(3,5,7)]
colnames(highlight.no_GAMMAS)[1:5] = c("Sub", "F", "B", "S", "U")

####Get Mean gammas and CIs for TABLE####
##control
apply(Control_GAMMAS[ , -1], 2, mean, na.rm = T)
(apply(Control_GAMMAS[ , -1], 2, sd, na.rm = T) / sqrt(nrow(Control_GAMMAS))) * 1.96

##Large vs Small
#Large
apply(size.large_GAMMAS[ , -1], 2, mean, na.rm = T)
(apply(size.large_GAMMAS[ , -1], 2, sd, na.rm = T) / sqrt(nrow(size.large_GAMMAS))) * 1.96

#Small
apply(size.small_GAMMAS[ , -1], 2, mean, na.rm = T)
(apply(size.small_GAMMAS[ , -1], 2, sd, na.rm = T) / sqrt(nrow(size.small_GAMMAS))) * 1.96

##Highlight vs no highlight
#Yes highlight
apply(highlight.yes_GAMMAS[ , -1], 2, mean, na.rm = T)
(apply(highlight.yes_GAMMAS[ , -1], 2, sd, na.rm = T) / sqrt(nrow(highlight.yes_GAMMAS))) * 1.96

#No highlight
apply(highlight.no_GAMMAS[ , -1], 2, mean, na.rm = T)
(apply(highlight.no_GAMMAS[ , -1], 2, sd, na.rm = T) / sqrt(nrow(highlight.no_GAMMAS))) * 1.96

####Overall gammas####
#control
mean(c(Control_GAMMAS$F, Control_GAMMAS$B, Control_GAMMAS$S, Control_GAMMAS$U), na.rm = T)
(sd(c(Control_GAMMAS$F, Control_GAMMAS$B, Control_GAMMAS$S, Control_GAMMAS$U), na.rm = T) / sqrt(156)) * 1.96

#large
mean(c(size.large_GAMMAS$F, size.large_GAMMAS$B, size.large_GAMMAS$S, size.large_GAMMAS$U), na.rm = T)
(sd(c(size.large_GAMMAS$F, size.large_GAMMAS$B, size.large_GAMMAS$S, size.large_GAMMAS$U), na.rm = T) / sqrt(156)) * 1.96

#small
mean(c(size.small_GAMMAS$F, size.small_GAMMAS$B, size.small_GAMMAS$S, size.small_GAMMAS$U), na.rm = T)
(sd(c(size.small_GAMMAS$F, size.small_GAMMAS$B, size.small_GAMMAS$S, size.small_GAMMAS$U), na.rm = T) / sqrt(156)) * 1.96

#yes highlight
mean(c(highlight.yes_GAMMAS$F, highlight.yes_GAMMAS$B, highlight.yes_GAMMAS$S, highlight.yes_GAMMAS$U), na.rm = T)
(sd(c(highlight.yes_GAMMAS$F, highlight.yes_GAMMAS$B, highlight.yes_GAMMAS$S, highlight.yes_GAMMAS$U), na.rm = T) / sqrt(156)) * 1.96

#no highlight
mean(c(highlight.no_GAMMAS$F, highlight.no_GAMMAS$B, highlight.no_GAMMAS$S, highlight.no_GAMMAS$U), na.rm = T)
(sd(c(highlight.no_GAMMAS$F, highlight.no_GAMMAS$B, highlight.no_GAMMAS$S, highlight.no_GAMMAS$U), na.rm = T) / sqrt(156)) * 1.96

##Significant?
t.test(na.omit(c(highlight.no_GAMMAS$F, highlight.no_GAMMAS$B, highlight.no_GAMMAS$S, highlight.no_GAMMAS$U)), na.omit(c(highlight.yes_GAMMAS$F, highlight.yes_GAMMAS$B, highlight.yes_GAMMAS$S, highlight.yes_GAMMAS$U)), paired = F, var.equal = T)

##Put everything together for sig testing
##control
Control_GAMMAS2 = melt(na.omit(Control_GAMMAS),
                       id.vars = "Sub")

colnames(Control_GAMMAS2)[2:3] = c("Direction", "gamma")

ezANOVA(Control_GAMMAS2,
        within = Direction,
        wid = Sub,
        dv = gamma) #sig

##font-size
#small
size.small_GAMMAS2 = melt(na.omit(size.small_GAMMAS),
                       id.vars = "Sub")

colnames(size.small_GAMMAS2)[2:3] = c("Direction", "gamma")

ezANOVA(size.small_GAMMAS2,
        within = Direction,
        wid = Sub,
        dv = gamma) #NS

#large
size.large_GAMMAS2 = melt(na.omit(size.large_GAMMAS),
                          id.vars = "Sub")

colnames(size.large_GAMMAS2)[2:3] = c("Direction", "gamma")

ezANOVA(size.large_GAMMAS2,
        within = Direction,
        wid = Sub,
        dv = gamma) #NS

##highlight vs control
#yes
highlight.yes_GAMMAS2 = melt(na.omit(highlight.yes_GAMMAS),
                          id.vars = "Sub")

colnames(highlight.yes_GAMMAS2)[2:3] = c("Direction", "gamma")

ezANOVA(highlight.yes_GAMMAS2,
        within = Direction,
        wid = Sub,
        dv = gamma) #NS

#no
highlight.no_GAMMAS2 = melt(na.omit(highlight.no_GAMMAS),
                             id.vars = "Sub")

colnames(highlight.no_GAMMAS2)[2:3] = c("Direction", "gamma")

ezANOVA(highlight.no_GAMMAS2,
        within = Direction,
        wid = Sub,
        dv = gamma) #NS

##okay, combine into the datasets
Control_GAMMAS2$group = rep("control")

size.small_GAMMAS2$group = rep("small")
size.large_GAMMAS2$group = rep("large")

highlight.yes_GAMMAS2$group = rep("yes")
highlight.no_GAMMAS2$group = rep("no")

ls = rbind(size.small_GAMMAS2, size.large_GAMMAS2)
lc = rbind(size.large_GAMMAS2, Control_GAMMAS2)
sc = rbind(size.small_GAMMAS2, Control_GAMMAS2)
yn = rbind(highlight.yes_GAMMAS2, highlight.no_GAMMAS2)
yc = rbind(highlight.yes_GAMMAS2, Control_GAMMAS2)
nc = rbind(highlight.no_GAMMAS2, Control_GAMMAS2)

##large vs small
ezANOVA(ls,
        wid = Sub,
        between = group,
        within = Direction,
        gamma,
        type = 3) #ns
ezANOVA(lc,
        wid = Sub,
        between = group,
        within = Direction,
        gamma,
        type = 3) #ns
ezANOVA(sc,
        wid = Sub,
        between = group,
        within = Direction,
        gamma,
        type = 3) #ns
ezANOVA(yn,
        wid = Sub,
        between = group,
        within = Direction,
        gamma,
        type = 3) #ns
ezANOVA(yc,
        wid = Sub,
        between = group,
        within = Direction,
        gamma,
        type = 3) #ns
ezANOVA(nc,
        wid = Sub,
        between = group,
        within = Direction,
        gamma,
        type = 3) #ns
