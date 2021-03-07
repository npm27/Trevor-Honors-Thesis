setwd("C:/Users/nickm.000/Documents/GitHub/Trevor-Honors-Thesis/1 Output/Ex 3/Raw") #get the correct directory

#Get the files names
files = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat$Username))

####Subset by Task Type####
setwd('..')

unique(dat$Condition.Notes)

dat2 = subset(dat,
              dat$Condition.Notes == "Sans F A" | dat$Condition.Notes == "Sans F B")
dat3 = subset(dat,
              dat$Condition.Notes == "Control A" | dat$Condition.Notes == "Control B")

##Drop unused columns
dat2 = dat2[ , -c(2:4, 6:7, 9:10, 12, 20:24, 28:32,  34)]
dat3 = dat3[ , -c(2:4, 6:7, 9:10, 12, 20:24, 28:32,  34)]

#Remove buffer trials
dat2 = subset(dat2,
              dat2$Stimuli.Stimuli.Notes != "Buffer")
dat3 = subset(dat3,
              dat3$Stimuli.Stimuli.Notes != "Buffer")

#Remove instructions
dat2 = subset(dat2,
              dat2$Procedure.Trial.Type != "Instruct")
dat3 = subset(dat3,
              dat3$Procedure.Trial.Type != "Instruct")

##Remove Filler
dat2 = subset(dat2,
              dat2$Procedure.Trial.Type != "FreeRecall")
dat3 = subset(dat3,
              dat3$Procedure.Trial.Type != "FreeRecall")

####Set the data up for scoring####
unique(dat2$Procedure.Trial.Type)

#Start by subsetting out the recall and JOL data for each dataset
dat2.JOL = subset(dat2,
                  dat2$Procedure.Trial.Type == "JOL" | dat2$Procedure.Trial.Type == "JOL_SF")
dat2.Recall = subset(dat2,
                     dat2$Procedure.Trial.Type == "Test")

#get JOLs and Recall in the same order
dat2.JOL = dat2.JOL[order(dat2.JOL$Stimuli.Cue), ]
dat2.JOL = dat2.JOL[order(dat2.JOL$Condition.Number), ]
dat2.JOL = dat2.JOL[order(dat2.JOL$Stimuli.Shuffle), ]

dat2.Recall = dat2.Recall[order(dat2.Recall$Stimuli.Cue), ]
dat2.Recall = dat2.Recall[order(dat2.Recall$Condition.Number), ]
dat2.Recall = dat2.Recall[order(dat2.Recall$Stimuli.Shuffle), ]

#Okay, put it back together now
dat2.R = dat2.Recall[ , c(12:14)]

#Drop overlapping columns and clean things up
JOL2 = cbind(dat2.JOL, dat2.R)
JOL2 = JOL2[ , -c(13:14)]

colnames(JOL2)[12] = "JOL.RT"

JOL2 = JOL2[ , -c(9:10)]
SF_vs_JOL = JOL2[ , -2]

###Now do the highlight stuff
unique(dat3$Procedure.Trial.Type)

dat3.JOL = subset(dat3,
                  dat3$Procedure.Trial.Type == "JOL")
dat3.Recall = subset(dat3,
                     dat3$Procedure.Trial.Type == "Test")

#get JOLs and Recall in the same order
dat3.JOL = dat3.JOL[order(dat3.JOL$Stimuli.Cue), ]
dat3.JOL = dat3.JOL[order(dat3.JOL$Condition.Number), ]
dat3.JOL = dat3.JOL[order(dat3.JOL$Stimuli.Shuffle), ]

dat3.Recall = dat3.Recall[order(dat3.Recall$Stimuli.Cue), ]
dat3.Recall = dat3.Recall[order(dat3.Recall$Condition.Number), ]
dat3.Recall = dat3.Recall[order(dat3.Recall$Stimuli.Shuffle), ]

#Okay, put it back together now
dat3.R = dat3.Recall[ , c(12:14)]

#Drop overlapping columns and clean things up
JOL3 = cbind(dat3.JOL, dat3.R)
JOL3 = JOL3[ , -c(13:14)]

colnames(JOL3)[12] = "JOL.RT"

JOL3 = JOL3[ , -c(9:10)]

Control = JOL3[ , -2]

####Score the recall data####
library(lrd)

##Start with the SF data
##Okay, need to redo this section so it matches the new functions

#JOL2.key = tolower(Large_vs_Small$Stimuli.Answer)
#JOL2.Response = tolower(Large_vs_Small$Response.Response)
#ID = Large_vs_Small$Username

#match1 = percent_match.cr(JOL2.Response, key = JOL2.key, id = ID)
#score_recall.cr(match1, cutoff = 0.75)

#scored1 = read.csv("output.csv")
unique(SF_vs_JOL$Condition.Number)

SF_vs_JOL$Response.Response = tolower(SF_vs_JOL$Response.Response)
SF_vs_JOL$Stimuli.Answer = tolower(SF_vs_JOL$Stimuli.Answer)

SF1 = subset(SF_vs_JOL,
              SF_vs_JOL$Condition.Number == 1)

SF1 = SF1[order(SF1$Stimuli.Answer), ]
SF1 = SF1[order(SF1$Username), ]

SF1$trial_num = rep(1:nrow(SF1))

#write.csv(SF1, file = "temp.csv", row.names = F)

#SF1 = read.csv("temp.csv")

output1 = prop_correct_cued(SF1, responses = "Response.Response", key = "Stimuli.Answer",
                            key.trial = "trial_num", id.trial = "trial_num", id = "Username",
                            cutoff = 2)
output1$DF_Scored

SF2 = subset(SF_vs_JOL,
             SF_vs_JOL$Condition.Number == 2)

SF2 = SF2[order(SF2$Username), ]

SF2$trial_num = rep(1:nrow(SF2)) #FIX TRIAL NUMBER!

output2 = prop_correct_cued(SF2, responses = "Response.Response", key = "Stimuli.Answer",
                            key.trial = "trial_num", id.trial = "trial_num", id = "Username",
                            cutoff = 2)

output1a = output1$DF_Scored
output2a = output2$DF_Scored

SF_vs_JOL_Scored = rbind(output1a, output2a)

##Okay, now for the control

unique(Control$Condition.Number)

Control$Response.Response = tolower(Control$Response.Response)
Control$Stimuli.Answer = tolower(Control$Stimuli.Answer)

Control1 = subset(Control,
             Control$Condition.Number == 3)

Control1 = Control1[order(Control1$Username), ]

Control1$trial_num = rep(1:nrow(Control1))

output1 = prop_correct_cued(Control1, responses = "Response.Response", key = "Stimuli.Answer",
                            key.trial = "trial_num", id.trial = "trial_num", id = "Username",
                            cutoff = 2)

Control2 = subset(Control,
             Control$Condition.Number == 4)

Control2 = Control2[order(Control2$Username), ]

Control2$trial_num = rep(1:nrow(Control2)) #FIX TRIAL NUMBER!

output2 = prop_correct_cued(Control2, responses = "Response.Response", key = "Stimuli.Answer",
                            key.trial = "trial_num", id.trial = "trial_num", id = "Username",
                            cutoff = 2)

output1a = output1$DF_Scored
output2a = output2$DF_Scored

Control_Scored = rbind(output1a, output2a)

####Write combined raw output to data file####
#write.csv(SF_vs_JOL_Scored, file = "Ex 3 Output Scored/SF.csv", row.names = F)
#write.csv(Control_Scored, file = "Ex 3 Output Scored/Control.csv", row.names = F)
