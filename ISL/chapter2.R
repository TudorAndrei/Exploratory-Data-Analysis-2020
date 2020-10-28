




college = read.csv("ISL/College.csv")
fix(college)

rownames(college) = college[,1]
fix(college)


college = college[,-1]
fix(college)

summary(college)


Elite = rep("No", nrow(college))
Elite[college$Top10perc >50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college)
fix(college)


boxplot(college$Outstate, college$Elite)
