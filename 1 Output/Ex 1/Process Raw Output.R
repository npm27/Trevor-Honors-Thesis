setwd("C:/Users/nickm.000/Documents/GitHub/Trevor-Honors-Thesis/Ex 1 Output") #get the correct directory

#Get the files names
files = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat$Username))

####Subset by Task Type####
setwd('..')

dat2 = subset(dat,
              dat$Condition.Notes == "Large vs small")
dat3 = subset(dat,
              dat$Condition.Notes == "Highlight vs normal")

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
#Start by subsetting out the recall and JOL data for each dataset
dat2.JOL = subset(dat2,
                 dat2$Procedure.Trial.Type == "JOL_Small" | dat2$Procedure.Trial.Type == "JOL_Large")
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
Large_vs_Small = JOL2[ , -2]

###Now do the highlight stuff
dat3.JOL = subset(dat3,
                  dat3$Procedure.Trial.Type == "JOL_H" | dat3$Procedure.Trial.Type == "JOL")
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

Highlight = JOL3[ , -2]

####Score the recall data####
library(lrd)

##Start with the Large vs Small data
JOL2.key = tolower(Large_vs_Small$Stimuli.Answer)
JOL2.Response = tolower(Large_vs_Small$Response.Response)
ID = Large_vs_Small$Username

match1 = percent_match.cr(JOL2.Response, key = JOL2.key, id = ID)
score_recall.cr(match1, cutoff = 0.75)

scored1 = read.csv("output.csv")

Large_vs_Small$Recall_Score = scored1$scored * 100

##Now do the highlight data
JOL3.key = tolower(Highlight$Stimuli.Answer)
JOL3.Response = tolower(Highlight$Response.Response)
ID = Highlight$Username

match2 = percent_match.cr(JOL3.Response, key = JOL3.key, id = ID)
score_recall.cr(match2, cutoff = 0.75)

scored2 = read.csv("output.csv")

Highlight$Recall_Score = scored2$scored * 100

####Write combined raw output to data file####
write.csv(Large_vs_Small, file = "Ex 1 Scored Output/Size.csv", row.names = F)
write.csv(Highlight, file = "Ex 1 Scored Output/Highlight.csv", row.names = F)
