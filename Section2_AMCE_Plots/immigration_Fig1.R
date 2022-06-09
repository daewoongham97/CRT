# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Plotting AMCE results from Hainmueller and Hopkins (2014)
# Producing Figure 1
# written on 06/09/2022

library(ggplot2)

## Results obtained from original Hainmueller and Hopkins (2014) paper and produced by
HH_results = read.csv("data/HH_results.csv")

estimates = as.numeric(sapply(as.character(HH_results[, 2][c(204, 206, 212, 208)]), function(x) substr(x, 2, nchar(x))))
sds = as.numeric(sapply(as.character(HH_results[, 2][c(205, 207, 213, 209)]), function(x) substr(x, 3, nchar(x) - 1)))

hh_results = data.frame(x = c("Germany", "France", "Poland", "Mexico"), estimates = estimates, lower = c(estimates - 1.96*sds), upper =c(estimates + 1.96*sds))

#plotting parameters
s = 5
w = 50
s2 = 5
a1 = 15
a2 = 20
a3 = 5

fig_1 = ggplot(hh_results, aes(x = x, y = estimates)) + geom_point(size = s) + geom_errorbar(aes(ymax = upper, ymin = lower), size = 2,width = 0.5) + xlab("") + ylab("AMCE") +  geom_hline(yintercept=0, linetype="dashed", color = "black", size=2)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))  + ylim(c(-0.15, 0.15)) +
  theme(axis.text=element_text(size=a1), axis.title=element_text(size=a2,face="bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position= "none", legend.title=element_blank(),legend.text=element_text(size=a1), plot.title = element_text(size = a2, face = "bold"), axis.title.x = element_text(vjust=-0.5))

ggsave(file="Figures/Fig1.pdf", fig_1,  width = 8, height = 5, device = "pdf")





