library(odbc)
library(tidyverse)
library(DBI)
library(ggplot2)


#getting data into dataframes
#getting s21 data in a dataframe
#this first aggregation is at a student level, scores for each attempt that students made 
studentagre1 <- dbReadTable(con, "studentaggregation2")
#student level aggregation getting average scores and total attempts
studentagre3 <- dbReadTable(con, "studentaggregation1")
#getting scores for each attempt but last attempt is == 1, easy filtering to get the final score.
studentagre5 <- dbReadTable(con, "studentaggregation3")
studentagre5 <- studentagre5 %>% filter(RankingAttempts==1)
#getting data for difference in first and last attemtp
studentagre7 <- dbReadTable(con, "studentaggregation4")


#getting data for s20
#getting scores for each attempt
studentagre2 <- dbReadTable(con1, "studentaggregation2")
#this first aggregation is at a student level, showing total attempts and their average scores over all the attempts 
studentagre4 <- dbReadTable(con1, "studentaggregation1")
#getting scores for each attempt but last attempt is == 1, easy filtering to get the final score.
studentagre6 <- dbReadTable(con1, "studentaggregation3")
studentagre6 <- studentagre6 %>% filter(RankingAttempts==1)
#getting data for difference in first and last attempt
studentagre8 <- dbReadTable(con1, "studentaggregation4")


#getting data for f20
#getting scores for each attempt 
studentagre11 <- dbReadTable(con2, "studentaggregation2")
#this first aggregation is at a student level, showing total attempts and their average scores over all the attempts 
studentagre12 <- dbReadTable(con2, "studentaggregation1")
#getting scores for each attempt but last attempt is == 1, easy filtering to get the final score.  
studentagre13 <- dbReadTable(con2, "studentaggregation3")
studentagre13 <- studentagre13 %>% filter(RankingAttempts==1)
#getting data for difference in first and last attempt
studentagre14 <- dbReadTable(con2, "studentaggregation4")


#Getting basic numbers
#getting total students
#s20
length(unique(studentagre4$person_id))
#s21
length(unique(studentagre3$person_id))
#f20
length(unique(studentagre12$person_id))
#average attempts
#s20 
mean(studentagre4$Attempts)
#s21
mean(studentagre3$Attempts)
#f20
mean(studentagre3$Attempts)


#creating graphs for s21
#creating graph for each individual attempt
ggplot(studentagre1, aes(x=as.integer(RankingAttempts), y=score_obtained)) +  
  geom_point(size=2, shape=21) + geom_smooth(method=lm) + ylim(0,80) + 
  ggtitle("Attempts ~ Score Obtained S21") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(vjust = 0.5)) + 
  labs(y="Score Obtained", x = "Attempt Number")

#creating graph for overall average score
#ggplot(studentagre3, aes(x=as.integer(Attempts), y=averagescore)) +  
#geom_point(size=2, shape=21) + geom_smooth(method=lm) + ggtitle("Total Attempts ~ Average Score S21") + theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(vjust = 0.5))


#creating graphs for s20

#creating graph for each individual attempt
ggplot(studentagre2, aes(x=as.integer(RankingAttempts), y=score_obtained)) +  
  geom_point(size=2, shape=21) + geom_smooth(method=lm) + ylim(0,80) + 
  ggtitle("Attempts ~ Score Obtained S20") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(vjust = 0.5)) + 
  labs(y="Score Obtained", x = "Attempt Number")

#creating graph for overall average score
#creating graph for overall average score
#ggplot(studentagre4, aes(x=as.integer(Attempts), y=averagescore)) +  
#geom_point(size=2, shape=21) + geom_smooth(method=lm) + ggtitle("Total Attempts ~ Average Score S20") + theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(vjust = 0.5))


#creating graphs for f20


#creating graph for each individual attempt
ggplot(studentagre11, aes(x=as.integer(RankingAttempts), y=score_obtained)) +  
  geom_point(size=2, shape=21) + 
  geom_smooth(method=lm) + ylim(0,80) + ggtitle("Attempts ~ Score Obtained f20") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(vjust = 0.5)) + 
  labs(y="Score Obtained", x = "Attempt Number")


#the following lines are performing t.tests on s20 and s21 
#creating a single dataset to perform t test for overall average attempts and score
totaloverallaggre <- rbind(studentagre3, studentagre4) 
#calculating t test for their overall average scores across all their attempts
t.test(totaloverallaggre$averagescore~totaloverallaggre$task_id,mu=0,alt="two.sided",conf= 0.95,paired = F, var.eq=F) 

#calculating t test for their overall average attempts and tasks 
t.test(as.integer(totaloverallaggre$Attempts)~totaloverallaggre$task_id,mu=0,alt="two.sided",conf= 0.95,paired = F, var.eq=F) 


#filtering the last attempt and then will be running t test to evaluate the effectiveness of the final attempts
finalattempt <- rbind(studentagre6, studentagre5)
t.test(finalattempt$PercScore~finalattempt$task_id,mu=0,alt="two.sided",conf= 0.95,paired = F, var.eq=F)


#combining data and then running a t.test
scoreimprovement <- rbind(studentagre8, studentagre7)
t.test(scoreimprovement$ScoreImprovement ~scoreimprovement$task_id,mu=0,alt="two.sided",conf= 0.95,paired = F, var.eq=F) 


#now running t test between f20 and s20
#getting overall attempts and finding the difference whether it is statistically significant or not
totaloverallaggref20 <- rbind(studentagre12, studentagre4) 
#calculating t test for their overall average scores across all their attempts
t.test(as.integer(totaloverallaggref20$Attempts)~totaloverallaggref20$task_id,mu=0,alt="two.sided",conf= 0.95,paired = F, var.eq=F) 



#filtering the last attempt and then will be running t test to evaluate the effectiveness of the final attempts
finalattemptf20 <- rbind(studentagre6, studentagre13)
t.test(finalattemptf20$PercScore~finalattemptf20$task_id,mu=0,alt="two.sided",conf= 0.95,paired = F, var.eq=F)


#combining data and then running a t.test
scoreimprovementf20 <- rbind(studentagre8, studentagre14)
t.test(scoreimprovementf20$ScoreImprovement ~scoreimprovementf20$task_id,mu=0,alt="two.sided",conf= 0.95,paired = F, var.eq=F) 



#performing ttest for final attempt on the final attempt between s21 and f20
finalattemptf20s21 <- rbind(studentagre5, studentagre13)
t.test(finalattemptf20s21$PercScore~finalattemptf20s21$task_id,mu=0,alt="two.sided",conf= 0.95,paired = F, var.eq=F)




#the following calculations are done for s20
#getting subtask level breakdown for the task
subtasklevel <- dbReadTable(con1, "subtasklevel") 

#adding max score for each subtask
subtasklevel1 <- subtasklevel %>% group_by(subtask_id) %>% mutate(maxscore = max(subtask_score_obtained))
#adding percentage score obtained
subtasklevel1 <- subtasklevel1 %>% mutate(PercScore = subtask_score_obtained/maxscore)

#Question level and attempt level
Qlevelaverage <- subtasklevel1 %>% group_by(subtask_name,RankingAttempts) %>% summarise(averagemarks = round(mean(subtask_score_obtained),2), averageperc = round(mean(PercScore),2),totalstudents = )

#Question level
Qlevelaverage1 <- subtasklevel1 %>% group_by(subtask_name) %>% summarise(averagemarks = round(mean(subtask_score_obtained),2), averageperc = round(mean(PercScore),2),totalstudents = )

#getting data only for final attempt
FinalAttemptQlevel <- subtasklevel %>% group_by(person_id) %>% top_n(1)
#calculating total score and percent score for each subtask

FinalAttemptQlevel <- FinalAttemptQlevel %>% group_by(subtask_id) %>% mutate(maxscore = max(subtask_score_obtained))
#adding percentage score obtained
FinalAttemptQlevel <- FinalAttemptQlevel %>% mutate(PercScore = subtask_score_obtained/maxscore)
#adding subtask column
FinalAttemptQlevel <- FinalAttemptQlevel %>% mutate(Subtask = case_when(endsWith(subtask_name, "1")~'Subtask1',
                                                                        endsWith(subtask_name, "2")~'Subtask1',
                                                                        endsWith(subtask_name, "3")~'Subtask1',
                                                                        endsWith(subtask_name, "4")~'Subtask2',
                                                                        endsWith(subtask_name, "5")~'Subtask2',
                                                                        endsWith(subtask_name, "6")~'Subtask2',
                                                                        endsWith(subtask_name, "7")~'Subtask2',
                                                                        endsWith(subtask_name,"s")~'wiki_march_madness',
                                                                        endsWith(subtask_name,"x")~'inverted_index',
                                                                        endsWith(subtask_name,"t")~"wiki_secret",
                                                                        endsWith(subtask_name,"e")~"code-style"))

#calculating average scores for each attempt for the final attempt
FinalAttemptQlevel1 <- FinalAttemptQlevel %>% group_by(Subtask) %>% summarise(averagemarks = round(mean(subtask_score_obtained),2), averageperc = round(mean(PercScore),2),totalstudents = )




#getting data for s21
s21subtasklevel <- dbReadTable(con, "subtasklevel")

#adding max score for each subtask
s21subtasklevel1 <- s21subtasklevel %>% group_by(subtask_id) %>% mutate(maxscore = max(subtask_score_obtained))
#adding percentage score obtained
s21subtasklevel1 <- s21subtasklevel1 %>% mutate(PercScore = subtask_score_obtained/maxscore)

#Question level and attempt level
s21Qlevelaverage <- s21subtasklevel1 %>% group_by(subtask_name,RankingAttempts) %>% summarise(averagemarks = round(mean(subtask_score_obtained),2), averageperc = round(mean(PercScore),2),totalstudents = )

#Question level
s21Qlevelaverage1 <- s21subtasklevel1 %>% group_by(subtask_name) %>% summarise(averagemarks = round(mean(subtask_score_obtained),2), averageperc = round(mean(PercScore),2),totalstudents = )

#getting data only for final attempt
s21FinalAttemptQlevel <- s21subtasklevel %>% group_by(person_id) %>% top_n(1)
#calculating total score and percent score for each subtask

s21FinalAttemptQlevel <- s21FinalAttemptQlevel %>% group_by(subtask_id) %>% mutate(maxscore = max(subtask_score_obtained))
#adding percentage score obtained
s21FinalAttemptQlevel <- s21FinalAttemptQlevel %>% mutate(PercScore = subtask_score_obtained/maxscore)
#adding subtask column
s21FinalAttemptQlevel <- s21FinalAttemptQlevel %>% mutate(Subtask = case_when(endsWith(subtask_name, "1")~'Subtask1',
                                                                              endsWith(subtask_name, "2")~'Subtask1',
                                                                              endsWith(subtask_name, "3")~'Subtask1',
                                                                              endsWith(subtask_name, "4")~'Subtask2',
                                                                              endsWith(subtask_name, "5")~'Subtask2',
                                                                              endsWith(subtask_name, "6")~'Subtask2',
                                                                              endsWith(subtask_name, "7")~'Subtask2',
                                                                              endsWith(subtask_name,"s")~'wiki_march_madness',
                                                                              endsWith(subtask_name,"x")~'inverted_index',
                                                                              endsWith(subtask_name,"t")~"wiki_secret",
                                                                              endsWith(subtask_name,"e")~"code-style"))


#calculating average scores for each attempt for the final attempt
s21FinalAttemptQlevel1 <- s21FinalAttemptQlevel %>% group_by(Subtask) %>% 
  summarise(averagemarks = round(mean(subtask_score_obtained),2), averageperc = round(mean(PercScore),2),totalstudents = )

#getting data for f20

f20subtasklevel <- dbReadTable(con2, "subtasklevel")

#adding max score for each subtask
f20subtasklevel1 <- f20subtasklevel %>% group_by(subtask_id) %>% mutate(maxscore = max(subtask_score_obtained))
#adding percentage score obtained
f20subtasklevel1 <- f20subtasklevel1 %>% mutate(PercScore = subtask_score_obtained/maxscore)

#Question level and attempt level
f20Qlevelaverage <- f20subtasklevel1 %>% group_by(subtask_name,RankingAttempts) %>% summarise(averagemarks = round(mean(subtask_score_obtained),2), averageperc = round(mean(PercScore),2),totalstudents = )

#Question level
f20Qlevelaverage1 <- f20subtasklevel1 %>% group_by(subtask_name) %>% summarise(averagemarks = round(mean(subtask_score_obtained),2), averageperc = round(mean(PercScore),2),totalstudents = )

#getting data only for final attempt
f20FinalAttemptQlevel <- f20subtasklevel %>% group_by(person_id) %>% top_n(1)
#calculating total score and percent score for each subtask

f20FinalAttemptQlevel <- f20FinalAttemptQlevel %>% group_by(subtask_id) %>% mutate(maxscore = max(subtask_score_obtained))
#adding percentage score obtained
f20FinalAttemptQlevel <- f20FinalAttemptQlevel %>% mutate(PercScore = subtask_score_obtained/maxscore)
#adding subtask column
f20FinalAttemptQlevel <- f20FinalAttemptQlevel %>% mutate(Subtask = case_when(endsWith(subtask_name, "1")~'Subtask1',
                                                                              endsWith(subtask_name, "2")~'Subtask1',
                                                                              endsWith(subtask_name, "3")~'Subtask1',
                                                                              endsWith(subtask_name, "4")~'Subtask2',
                                                                              endsWith(subtask_name, "5")~'Subtask2',
                                                                              endsWith(subtask_name, "6")~'Subtask2',
                                                                              endsWith(subtask_name, "7")~'Subtask2',
                                                                              endsWith(subtask_name,"s")~'wiki_march_madness',
                                                                              endsWith(subtask_name,"x")~'inverted_index',
                                                                              endsWith(subtask_name,"t")~"wiki_secret",
                                                                              endsWith(subtask_name,"e")~"code-style"))


#calculating average scores for each attempt for the final attempt
f20FinalAttemptQlevel1 <- f20FinalAttemptQlevel %>% group_by(Subtask) %>% 
  summarise(averagemarks = round(mean(subtask_score_obtained),2), averageperc = round(mean(PercScore),2),totalstudents = )



#plotting graphs for the average scores for each subtask

#calculating average scores for last attempts for each subtask
#for s20
p10 <- ggplot(FinalAttemptQlevel, aes(y=averageperc,x=Subtask)) +  
  geom_bar(position="dodge",stat="identity") + 
  geom_text(aes(label= averageperc),vjust=1.6, color="white", size=3.5, hjust = 0.5)+
  ggtitle("Last Attempt ~ Subtask s20") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle = 90)) + 
  theme(legend.position = "none")


#for s21
p11<- ggplot(s21FinalAttemptQlevel, aes(y=averageperc,x=Subtask)) +  
  geom_bar(position="dodge",stat="identity") + 
  geom_text(aes(label= averageperc),vjust=1.6, color="white", size=3.5, hjust = 0.5)+ 
  ggtitle("Last Attempt ~ Subtask s21") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle = 90))+
  theme(legend.position = "none")


#for f20
p12<- ggplot(f20FinalAttemptQlevel, aes(y=averageperc,x=Subtask)) +  
  geom_bar(position="dodge",stat="identity") + 
  geom_text(aes(label= averageperc),vjust=1.6, color="white", size=3.5, hjust = 0.5)+ 
  ggtitle("Last Attempt ~ Subtask f20") + 
  theme(plot.title = element_text(hjust = 0.5),axis.text.x=element_text(angle = 90))



#Will be perfoming t tests on individual subtasks
#starting wikisecrets
wikisecrets20 <- FinalAttemptQlevel %>% filter(Subtask == "wiki_secret")
wikisecrets21 <- s21FinalAttemptQlevel %>%  filter(Subtask == "wiki_secret")
wikisecretf20 <- f20FinalAttemptQlevel %>% filter(Subtask == "wiki_secret") 

#combining data for s20 and s21
wikisecrets2021 <- rbind(wikisecrets21, wikisecrets20)
t.test(wikisecrets2021$PercScore~wikisecrets2021$task_id,mu=0,alt="two.sided",conf= 0.95,paired = F, var.eq=F)

wikisecrets20f20 <- rbind(wikisecretf20, wikisecrets20)
t.test(wikisecrets20f20$PercScore~wikisecrets20f20$task_id,mu=0,alt="two.sided",conf= 0.95,paired = F, var.eq=F)

wikisecrets21f20 <- rbind(wikisecretf20, wikisecrets21)
t.test(wikisecrets21f20$PercScore~wikisecrets21f20$task_id,mu=0,alt="two.sided",conf= 0.95,paired = F, var.eq=F)


#Will be perfoming t tests on individual subtasks
#starting wikimarch
wikimarch20 <- FinalAttemptQlevel %>% filter(Subtask == "wiki_march_madness")
wikimarch21 <- s21FinalAttemptQlevel %>%  filter(Subtask == "wiki_march_madness")
wikimarchf20 <- f20FinalAttemptQlevel %>% filter(Subtask == "wiki_march_madness") 

#combining data for s20 and s21
wikimarch2021 <- rbind(wikisecrets21, wikisecrets20)
t.test(wikisecrets2021$PercScore~wikisecrets2021$task_id,mu=0,alt="two.sided",conf= 0.95,paired = F, var.eq=F)

wikimarch20f20 <- rbind(wikisecretf20, wikisecrets20)
t.test(wikisecrets20f20$PercScore~wikisecrets20f20$task_id,mu=0,alt="two.sided",conf= 0.95,paired = F, var.eq=F)

wikimarch21f20 <- rbind(wikisecretf20, wikisecrets21)
t.test(wikisecrets21f20$PercScore~wikisecrets21f20$task_id,mu=0,alt="two.sided",conf= 0.95,paired = F, var.eq=F)

