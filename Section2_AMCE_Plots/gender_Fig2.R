# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Plotting AMCE results from Ono and Burden (2019)
# Producing Figure 2
# written on 06/09/2022

library(ggplot2)

## Results obtained from original Ono and Burden (2019) paper
pres_con_dat = read.csv("data/Fig1_officetype.csv")

pres = pres_con_dat$Estimate[1]
pres_sds = pres_con_dat$Std..Err[1]

con = pres_con_dat$Estimate[2]
con_sds = pres_con_dat$Std..Err[2]

#plotting parameters
s = 5
w = 50
s2 = 5
a1 = 15
a2 = 20
a3 = 5

ob_results = data.frame(x = c("President", "Congress"), estimates = c(pres, con), lower = c(pres - 1.96*pres_sds, con - 1.96*con_sds), upper = c(pres + 1.96*pres_sds, con + 1.96*con_sds))

fig_2 = ggplot(ob_results, aes(x = x, y = estimates)) + geom_point(size = s) + geom_errorbar(aes(ymax = upper, ymin = lower), size = 2,width = 0.5) + xlab("") + ylab("AMCE") +  geom_hline(yintercept=0, linetype="dashed", color = "black", size=2)+
  ylim(c(-0.05, 0.02)) +
  theme(axis.text=element_text(size=a1), axis.title=element_text(size=a2,face="bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position= "none", legend.title=element_blank(),legend.text=element_text(size=a1), plot.title = element_text(size = a2, face = "bold"), axis.title.x = element_text(vjust=-0.5))

ggsave(file="Figures/Fig2.pdf", fig_2,  width = 8, height = 5, device = "pdf")







