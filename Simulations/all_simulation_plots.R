# Replication Code for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# Code for producing all simulation figures (Figure 3, 4, 5, 6, 7) 

library(ggplot2); library(ggpubr); library(cowplot)

#figure specifications
s = 5
w = 50
s2 = 5
a1 = 15
a2 = 20

v = c(0, 0.025, 0.05, 0.075, 0.1, 0.125)
int_signal = 0.075
int_signal*c(sqrt(6)/3, 2*sqrt(3)/3)
x_axis_left = 3*v^2/(3*v^2 + 9*(0.1)^2/2 + 0.05^2*15/2)*100
x_axis_right = c(0, 6, 12, 18)

## Plotting main simulation (Figure 3)

# Left Figure
AMCE_left = CRT_left = vector()
for (param_num in 1:6) {
  CRT_pvals = AMCE_pvals = vector()
  for (i in 1:1000) {
    file_name = paste0("data/Fig3_leftplot_results/pval", param_num, "_", i, ".csv")
    dat = read.csv(file_name, header = TRUE)
    AMCE_pvals[i] = dat[1, 3]
    CRT_pvals[i] = dat[2,3]
  }
  AMCE_left[param_num] = mean(AMCE_pvals <= 0.05)
  CRT_left[param_num] = mean(CRT_pvals <= 0.05)
}

pn = data.frame(x_axis = rep(x_axis_left, 2), power = c(AMCE_left, CRT_left), group = factor(rep(c("AMCE", "CRT"), c(length(CRT_left), length(CRT_left) ))))

left_fig = ggplot(pn , aes(x = x_axis, y = power, group = group)) + geom_point(aes(shape = group, color = group), size = s) +geom_line(aes(color = group), size = 2, linetype = "dashed")  + xlab("% Variance explained by interaction") + ylab("Power") + ylim(c(0,1)) + scale_color_manual(values = c("Red", "Blue")) + 
  theme(axis.text=element_text(size=a1), axis.title=element_text(size=a2,face="bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position= "top", legend.title=element_blank(),legend.key=element_blank(), legend.text=element_text(size=a1),legend.key.size = unit(4,"line"),plot.title = element_text(size = a2, face = "bold"), axis.title.x = element_text(vjust=-0.5)) 

# Right Figure
AMCE_right = CRT_right = vector()
for (param_num in 1:4) {
  CRT_pvals = AMCE_pvals = vector()
  for (i in 1:1000) {
    file_name = paste0("data/Fig3_rightplot_results/pval", param_num, "_", i, ".csv")
    dat = read.csv(file_name, header = TRUE)
    AMCE_pvals[i] = dat[1, 3]
    CRT_pvals[i] = dat[2,3]
  }
  AMCE_right[param_num] = mean(AMCE_pvals <= 0.05)
  CRT_right[param_num] = mean(CRT_pvals <= 0.05)
}

pn = data.frame(x_axis = rep(x_axis_right, 2), power = c(AMCE_right, CRT_right), group = factor(rep(c("AMCE", "CRT"), c(length(CRT_right), length(CRT_right) ))))

right_fig = ggplot(pn , aes(x = x_axis, y = power, group = group)) + geom_point(aes(shape = group, color = group), size = s) +geom_line(aes(color = group), size = 2, linetype = "dashed")  + xlab("Number of interactions") + ylab("") + ylim(c(0,1)) + scale_color_manual(values = c("Red", "Blue")) + 
  theme(axis.text=element_text(size=a1), axis.title=element_text(size=a2,face="bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position= 'none', legend.title=element_blank(),legend.text=element_text(size=a1), plot.title = element_text(size = a2, face = "bold"), axis.title.x = element_text(vjust=-0.5))

fig_3 = ggarrange(left_fig,right_fig, common.legend = TRUE, nrow = 1)

ggsave(file="Simulations/Figures/Fig3.pdf", fig_3,  width = 12, height = 5, device = "pdf")

### Apendix plotting unconstrained vs constrained HierNet (Figure 4)

# Left Figure
CRT_uncons_left = dI_CRT_left = vector()
for (param_num in 1:6) {
  CRT_uncons_pvals = dI_CRT_pvals = vector()
  for (i in 1:1000) {
    file_name = paste0("data/Appendix_simulations/Fig4_and_6_leftplot_results/pval", param_num, "_", i, ".csv")
    dat = read.csv(file_name, header = TRUE)
    CRT_uncons_pvals[i] = dat[1, 3]
    dI_CRT_pvals[i] = dat[2,3]
  }
  CRT_uncons_left[param_num] = mean(CRT_uncons_pvals <= 0.05)
  dI_CRT_left[param_num] = mean(dI_CRT_pvals <= 0.05)
}

pn = data.frame(x_axis = rep(x_axis_left, 3), power = c(AMCE_left, CRT_left, CRT_uncons_left), group = factor(rep(c("AMCE", "CRT Original", "CRT Unconstrained"), c(length(CRT_left), length(CRT_left),length(CRT_left) ))))

left_fig = ggplot(pn , aes(x = x_axis, y = power, group = group)) + geom_point(aes(shape = group, color = group), size = s) +geom_line(aes(color = group), size = 2, linetype = "dashed")  + xlab("% Variance explained by interaction") + ylab("Power") + ylim(c(0,1)) + scale_color_manual(values = c("Red", "Blue", "Purple")) + 
  theme(axis.text=element_text(size=a1), axis.title=element_text(size=a2,face="bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),  legend.position= "top", legend.title=element_blank(),legend.key=element_blank(), legend.text=element_text(size=a1),legend.key.size = unit(4,"line"), plot.title = element_text(size = a2, face = "bold"), axis.title.x = element_text(vjust=-0.5))

# Right Figure
CRT_uncons_right = dI_CRT_right = vector()
for (param_num in 1:4) {
  CRT_uncons_pvals = dI_CRT_pvals = vector()
  for (i in 1:1000) {
    file_name = paste0("data/Appendix_simulations/Fig4_and_6_rightplot_results/pval", param_num, "_", i, ".csv")
    dat = read.csv(file_name, header = TRUE)
    CRT_uncons_pvals[i] = dat[1, 3]
    dI_CRT_pvals[i] = dat[2,3]
  }
  CRT_uncons_right[param_num] = mean(CRT_uncons_pvals <= 0.05)
  dI_CRT_right[param_num] = mean(dI_CRT_pvals <= 0.05)
}

pn = data.frame(x_axis = rep(x_axis_right, 3), power = c(AMCE_right, CRT_right, CRT_uncons_right), group = factor(rep(c("AMCE", "CRT Original", "CRT Unconstrained"), c(length(CRT_right), length(CRT_right),length(CRT_right) ))))

right_fig = ggplot(pn , aes(x = x_axis, y = power, group = group)) + geom_point(aes(shape = group, color = group), size = s) +geom_line(aes(color = group), size = 2, linetype = "dashed")  + xlab("Number of interactions") + ylab("") + ylim(c(0,1)) + scale_color_manual(values = c("Red", "Blue", "Purple")) + 
  theme(axis.text=element_text(size=a1), axis.title=element_text(size=a2,face="bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position='none', legend.title=element_blank(),legend.text=element_text(size=a1), plot.title = element_text(size = a2, face = "bold"), axis.title.x = element_text(vjust=-0.5))

fig_4 = ggarrange(left_fig, right_fig, common.legend = TRUE, nrow = 1)

ggsave(file="Simulations/Figures/Fig3.pdf", fig_4,  width = 12, height = 5, device = "pdf")

## Appendix plotting heterogenous and homogenous (Figure 5)

# Left Figure
CRT_hetero_left = vector()
for (param_num in 1:6) {
  CRT_pvals = vector()
  for (i in 1:1000) {
    file_name = paste0("data/Appendix_simulations/Fig5_leftplot_results/pval", param_num, "_", i, ".csv")
    dat = read.csv(file_name, header = TRUE)
    CRT_pvals[i] = dat[1,3]
  }
  CRT_hetero_left[param_num] = mean(CRT_pvals <= 0.05)
}

pn = data.frame(x_axis = rep(x_axis_left, 2), power = c(CRT_hetero_left, CRT_left), group = factor(rep(c("CRT Heterogeneous", "CRT Homogeneous"), c(length(CRT_left), length(CRT_left) ))))

left_fig = ggplot(pn , aes(x = x_axis, y = power, group = group)) + geom_point(aes(shape = group, color = group), size = s) +geom_line(aes(color = group), size = 2, linetype = "dashed")  + xlab("% Variance explained by interaction") + ylab("Power") + ylim(c(0,1)) + scale_color_manual(values = c("Green", "Blue")) + 
  theme(axis.text=element_text(size=a1), axis.title=element_text(size=a2,face="bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),  legend.position= "top", legend.title=element_blank(),legend.key=element_blank(), legend.text=element_text(size=a1),legend.key.size = unit(4,"line"), plot.title = element_text(size = a2, face = "bold"), axis.title.x = element_text(vjust=-0.5))

# Right Figure
CRT_hetero_right = vector()
for (param_num in 1:4) {
  CRT_pvals = vector()
  for (i in 1:1000) {
    file_name = paste0("data/Appendix_simulations/Fig5_rightplot_results/pval", param_num, "_", i, ".csv")
    dat = read.csv(file_name, header = TRUE)
    CRT_pvals[i] = dat[1,3]
  }
  CRT_hetero_right[param_num] = mean(CRT_pvals <= 0.05)
}

pn = data.frame(x_axis = rep(x_axis_right, 2), power = c(CRT_hetero_right, CRT_right), group = factor(rep(c("CRT Heterogenous", "CRT Homogenous"), c(length(CRT_right), length(CRT_right) ))))

right_fig = ggplot(pn , aes(x = x_axis, y = power, group = group)) + geom_point(aes(shape = group, color = group), size = s) +geom_line(aes(color = group), size = 2, linetype = "dashed")  + xlab("Number of interactions") + ylab("") + ylim(c(0,1)) + scale_color_manual(values = c("Green", "Blue")) + 
  theme(axis.text=element_text(size=a1), axis.title=element_text(size=a2,face="bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position='none', legend.title=element_blank(),legend.text=element_text(size=a1), plot.title = element_text(size = a2, face = "bold"), axis.title.x = element_text(vjust=-0.5))

fig_5 = ggarrange(left_fig, right_fig, common.legend = TRUE, nrow = 1)

ggsave(file="Figures/Fig5.pdf", fig_5,  width = 12, height = 5, device = "pdf")

##Appendinx plotting dI CRT (Figure 6)

# Left Figure
pn = data.frame(x_axis = rep(x_axis_left, 3), power = c(AMCE_left, CRT_left, dI_CRT_left), group = factor(rep(c("AMCE", "CRT Original", "dICRT") , c(length(CRT_left), length(CRT_left),length(CRT_left) ))))

left_fig = ggplot(pn , aes(x = x_axis, y = power, group = group)) + geom_point(aes(shape = group, color = group), size = s) +geom_line(aes(color = group), size = 2, linetype = "dashed")  + xlab("% Variance explained by interaction") + ylab("Power") + ylim(c(0,1)) + scale_shape_manual(name = "", labels = c("AMCE", "CRT HierNet", expression(d['I']*CRT)), values = c(19, 17, 15)) + scale_color_manual(name = "", labels = c("AMCE", "CRT HierNet", expression(d['I']*CRT)), values = c("Red", "Blue", "Black")) + 
  theme(axis.text=element_text(size=a1), axis.title=element_text(size=a2,face="bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position= "top", legend.title=element_blank(),legend.key=element_blank(), legend.text=element_text(size=a1),legend.key.size = unit(4,"line"), plot.title = element_text(size = a2, face = "bold"), axis.title.x = element_text(vjust=-0.5))

# Right Figure
pn = data.frame(x_axis = rep(x_axis_right, 3), power = c(AMCE_right, CRT_right, dI_CRT_right), group = factor(rep(c("AMCE", "CRT Original", "dICRT"), c(length(CRT_right), length(CRT_right),length(CRT_right) ))))

right_fig = ggplot(pn , aes(x = x_axis, y = power, group = group)) + geom_point(aes(shape = group, color = group), size = s) +geom_line(aes(color = group), size = 2, linetype = "dashed")  + xlab("Number of interactions") + ylab("") + ylim(c(0,1)) + scale_color_manual(values = c("Red", "Blue", "Black")) + 
  theme(axis.text=element_text(size=a1), axis.title=element_text(size=a2,face="bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position='none', legend.title=element_blank(),legend.text=element_text(size=a1), plot.title = element_text(size = a2, face = "bold"), axis.title.x = element_text(vjust=-0.5))

fig_6 = ggarrange(left_fig, right_fig, common.legend = TRUE,nrow = 1)

ggsave(file="Figures/Fig6.pdf", fig_6,  width = 12, height = 5, device = "pdf")

## Appendix: plotting inflated p-values of logistic regression (Figure 7)

# Left Figure
dim_z = c(3, 5, 10, 11, 12, 13)

logit_power = vector()
for (param_num in 1:6) {
  pvals = vector()
  for (i in 1:1000) {
    file_name = paste0("data/Appendix_simulations/Fig7_results/pval", param_num, "_", i, ".csv")
    dat = read.csv(file_name, header = TRUE)
    pvals[i] = dat[1,2]
  }
  logit_power[param_num] = mean(pvals <= 0.05)
}

pn = data.frame(dim_z, power = logit_power)

left_fig = ggplot(pn , aes(x = dim_z, y = power)) + geom_point(size = s) +geom_line(size = 2, linetype = "dashed")  + xlab("Number of Factors (Z)") + ylab("Proportion p-values < 0.05") + ylim(c(0,0.5)) +   geom_hline(yintercept=0.05, linetype="dashed", color = "red", size=2) +
  theme(axis.text=element_text(size=a1), axis.title=element_text(size=a2,face="bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position= "top", legend.title=element_blank(),legend.key=element_blank(), legend.text=element_text(size=a1),legend.key.size = unit(4,"line"), plot.title = element_text(size = a2, face = "bold"), axis.title.x = element_text(vjust=-0.5)) 

# Right Figure
p_values = vector()
for (i in 1:1000) {
  file_name = paste0("data/Appendix_simulations/Fig7_results/pval", 5, "_", i, ".csv")
  dat = read.csv(file_name, header = TRUE)
  p_values[i] = dat[1,2]
}
logit_power[param_num] = mean(pvals <= 0.05)

pvals = data.frame(p_values = p_values)

right_fig = ggplot(pvals , aes(x = p_values)) + geom_histogram(aes(y =(..count..)/sum(..count..)*100), fill = "white", colour = "black", bins = 15) + xlab("Histogram of p-values") + ylab("Percentage") +
  theme(axis.text=element_text(size=a1), axis.title=element_text(size=a2,face="bold"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),legend.position= "top", legend.title=element_blank(),legend.key=element_blank(), legend.text=element_text(size=a1),legend.key.size = unit(4,"line"), plot.title = element_text(size = a2, face = "bold"), axis.title.x = element_text(vjust=-0.5))

fig_7 = ggarrange(left_fig, right_fig, common.legend = TRUE, nrow = 1)

ggsave(file="Figures/Fig7.pdf", fig_7,  width = 12, height = 5, device = "pdf")




