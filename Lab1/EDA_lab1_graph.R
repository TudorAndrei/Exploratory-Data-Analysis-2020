cat("\f")
options(scipen=999)


##########################################

library(polynom)



cr_0=c(25920,33984,17100,4300,535,26)
cr_1=c(10695,21914,14530,4140,535,26)
cr_2=c(20160,28384,15500,4140,535,26)
cr_3=c(17415,28314,16290,4300,535,26)
cr_4=c(19200,27584,15340,4140,535,26)
cr_5=c(11655,22714,14690,4140,535,26)

r_0=as.polynomial(cr_0)
r_1=as.polynomial(cr_1)
r_2=as.polynomial(cr_2)
r_3=as.polynomial(cr_3)
r_4=as.polynomial(cr_4)
r_5=as.polynomial(cr_5)

nr_consistent_runner_25920=c(predict(r_0,0),predict(r_1,1),predict(r_2,2),predict(r_3,3),predict(r_4,4),predict(r_5,5),
                      predict(r_0,6),predict(r_1,7),predict(r_2,8),predict(r_3,9),predict(r_4,10),predict(r_5,11),
                      predict(r_0,12),predict(r_1,13),predict(r_2,14),predict(r_3,15),predict(r_4,16),predict(r_5,17),
                      predict(r_0,18),predict(r_1,19),predict(r_2,20),predict(r_3,21),predict(r_4,22),predict(r_5,23),
                      predict(r_0,24),predict(r_1,25),predict(r_2,26),predict(r_3,27),predict(r_4,28),predict(r_5,29),
                      predict(r_0,30),predict(r_1,31),predict(r_2,32),predict(r_3,33),predict(r_4,34),predict(r_5,35),
                      predict(r_0,36),predict(r_1,37),predict(r_2,38),predict(r_3,39),predict(r_4,40),predict(r_5,41),
                      predict(r_0,42),predict(r_1,43),predict(r_2,44),predict(r_3,45),predict(r_4,46),predict(r_5,47))
nr_consistent_runner_25920

nr_consistent_runner = nr_consistent_runner_25920 / 25920

nr_consistent_runner


c_0=c(311040,338688,155520,35880,4095,182)
c_1=c(171455,265578,145210,35560,4095,182)
c_2=c(259600,322368,151520,35560,4095,182)
c_3=c(176175,287658,149850,35880,4095,182)
c_4=c(273920,316608,150880,35560,4095,182)
c_5=c(176575,271338,145850,35560,4095,182)
c_6=c(291600,338688,155520,35880,4095,182)
c_7=c(152015,265578,145210,35560,4095,182)
c_8=c(279040,322368,151520,35560,4095,182)
c_9=c(195615,287658,149850,35880,4095,182)
c_10=c(254480,316608,150880,35560,4095,182)
c_11=c(157135,271338,145850,35560,4095,182)



p_0=as.polynomial(c_0)
p_1=as.polynomial(c_1)
p_2=as.polynomial(c_2)
p_3=as.polynomial(c_3)
p_4=as.polynomial(c_4)
p_5=as.polynomial(c_5)
p_6=as.polynomial(c_6)
p_7=as.polynomial(c_7)
p_8=as.polynomial(c_8)
p_9=as.polynomial(c_9)
p_10=as.polynomial(c_10)
p_11=as.polynomial(c_11)

nr_consistent_311040=c(predict(p_0,0),predict(p_1,1),predict(p_2,2),predict(p_3,3),predict(p_4,4),predict(p_5,5),
                             predict(p_6,6),predict(p_7,7),predict(p_8,8),predict(p_9,9),predict(p_10,10),predict(p_11,11),
                             predict(p_0,12),predict(p_1,13),predict(p_2,14),predict(p_3,15),predict(p_4,16),predict(p_5,17),
                             predict(p_6,18),predict(p_7,19),predict(p_8,20),predict(p_9,21),predict(p_10,22),predict(p_11,23),
                             predict(p_0,24),predict(p_1,25),predict(p_2,26),predict(p_3,27),predict(p_4,28),predict(p_5,29),
                             predict(p_6,30),predict(p_7,31),predict(p_8,32),predict(p_9,33),predict(p_10,34),predict(p_11,35),
                             predict(p_0,36),predict(p_1,37),predict(p_2,38),predict(p_3,39),predict(p_4,40),predict(p_5,41),
                             predict(p_6,42),predict(p_7,43),predict(p_8,44),predict(p_9,45),predict(p_10,46),predict(p_11,47))
nr_consistent_311040

nr_consistent = nr_consistent_311040 / 311040

nr_consistent


d_0=c(25920,34704,18540,5000,675,36)
d_1=c(10395,21204,14850,4680,675,36)
d_2=c(17280,26064,15660,4680,675,36)
d_3=c(19035,29844,17730,5000,675,36)
d_4=c(17280,26064,15660,4680,675,36)
d_5=c(10395,21204,14850,4680,675,36)

q_0=as.polynomial(d_0)
q_1=as.polynomial(d_1)
q_2=as.polynomial(d_2)
q_3=as.polynomial(d_3)
q_4=as.polynomial(d_4)
q_5=as.polynomial(d_5)

nr_ordered_25920=c(predict(q_0,0),predict(q_1,1),predict(q_2,2),predict(q_3,3),predict(q_4,4),predict(q_5,5),
                   predict(q_0,6),predict(q_1,7),predict(q_2,8),predict(q_3,9),predict(q_4,10),predict(q_5,11),
                   predict(q_0,12),predict(q_1,13),predict(q_2,14),predict(q_3,15),predict(q_4,16),predict(q_5,17),
                   predict(q_0,18),predict(q_1,19),predict(q_2,20),predict(q_3,21),predict(q_4,22),predict(q_5,23),
                   predict(q_0,24),predict(q_1,25),predict(q_2,26),predict(q_3,27),predict(q_4,28),predict(q_5,29),
                   predict(q_0,30),predict(q_1,31),predict(q_2,32),predict(q_3,33),predict(q_4,34),predict(q_5,35),
                   predict(q_0,36),predict(q_1,37),predict(q_2,38),predict(q_3,39),predict(q_4,40),predict(q_5,41),
                   predict(q_0,42),predict(q_1,43),predict(q_2,44),predict(q_3,45),predict(q_4,46),predict(q_5,47))
nr_ordered_25920

nr_ordered = nr_ordered_25920 / 25920

nr_ordered

prob_consistent_runner = nr_consistent_runner / nr_ordered 
prob_consistent_runner

prob_consistent_runner_inf = 26/36
prob_consistent_runner_inf

prob_consistent = nr_consistent / nr_ordered 
prob_consistent

prob_consistent_inf = (182*25920)/(311040*36)
prob_consistent_inf


nr_goals = c(0:47)
nr_goals



plot(nr_goals, prob_consistent_runner, xlim = c(0,50),ylim = c(0,1), col = 4, pch =16, xlab="Number of goals in the table", ylab="Probability")
legend(3,0.3,c("consistent","runner up consistent"),col=c(2,4),pch=c(3, 16))
abline(h = prob_consistent_runner_inf, col=4, lty=3)
par(new=T)
plot(nr_goals, prob_consistent, xlim = c(0,50),ylim = c(0,1), col = 2, pch = 3, cex=0.5, xlab="", ylab="")
abline(h=prob_consistent_inf, col=2, lty=3)
par(new=F)

###############################################################################################

nr_consistent_311040=c(predict(p_4,-20),predict(p_5,-19),
                       predict(p_6,-18),predict(p_7,-17),predict(p_8,-16),predict(p_9,-15),predict(p_10,-14),predict(p_11,-13),
                       predict(p_0,-12),predict(p_1,-11),predict(p_2,-10),predict(p_3,-9),predict(p_4,-8),predict(p_5,-7),
                       predict(p_6,-6),predict(p_7,-5),predict(p_8,-4),predict(p_9,-3),predict(p_10,-2),predict(p_11,-1),
                       predict(p_0,0),predict(p_1,1),predict(p_2,2),predict(p_3,3),predict(p_4,4),predict(p_5,5),
                       predict(p_6,6),predict(p_7,7),predict(p_8,8),predict(p_9,9),predict(p_10,10),predict(p_11,11))
nr_consistent_311040

nr_consistent = nr_consistent_311040 / 311040

nr_consistent

nr_crit = c(-20:11)

plot(nr_crit, nr_consistent, xlim = c(-21,12),ylim = c(-600,600), col = 4, pch =16, xlab="Number of goals in the table", ylab="Number of consistent tables")

###############################################################################################
