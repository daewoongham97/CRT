# Readme File for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# All scripts were written and edited by Dae Woong Ham on 06/09/2022

## Code Overview ## 

# Plotting previous empirical results (Fig 1, Fig 2)
- "Section2_AMCE_plots/immigration_Fig1.R" produces Figure 1 AMCE plots based on original AMCE estimates
- "Section2_AMCE_plots/gender_Fig2.R" produces Figure 2 AMCE plots based on original AMCE estimates

# All simulation plots (Fig 3, 4, 5, 6, 7, 8)
- All simulations are plotted through "Simulations/all_simulation_plots.R" file
- All simulation scripts are executed through "source/left_fig_simulation.sh" or "source/right_fig_simulation.sh" with 200 cores of computing cluster on FASRC Cannon cluster
- "Simulations/Section4/Figure3_leftplot.R"/"Simulations/Section4/Figure3_rightplot.R" produces results of Fig 3 # 120 and 80 minutes of computing time respectively 
- "Simulations/Appendix/Appendix_leftplot.R"/"Simulations/Section4/Appendix_rightplot.R" produces results of Fig 4, 5, 6, 7 # 672 and 448 minutes of computing time respectively
- "Simulations/Appendix/Figure9.R" produces results of Fig 9 # less than 5 minutes of computing time on FAS computing cluster

# Obtaining new p-values (Section 5 and Table 1) -- all p-values were run on Amazon Web Services with 50 cores
- All p-values in Section 5 are summarized and obtained in "Section5_ApplicationResults/pval_analysis.R"
- "Section5_ApplicationResults/immigration/CRT_pvalues.R"/"Section5_ApplicationResults/gender/CRT_pvalues.R" computes all CRT based p-values for Section 5 and table 1. # Approximately 30 minutes and 40 minutes respectively
- "Section5_ApplicationResults/immigration/AMCE_pval.do"/"Section5_ApplicationResults/gender/AMCE_pval.do" produces AMCE p-value in Table 1 #less than 1 second of total computing time
- "Section5_ApplicationResults/exploratory_lasso_results/" contains files to produce exploratory lasso p-value results when discerning if interactions played a role (for each application) #less than 1 second of total computing time

# Other folders
- "data" folder contains all relevant datasets in both Immigration and gender conjoint examples and all the saved results of p-values in simulations and test statistics for Section 5
- "Figures" folder contains all figures
- "source" folder contains all helper and main functions to run above scripts (including data cleaning, obtaining test statistics, generating simulation datasets). 

## Environment ## 
- R version 4.1.0
- 200 cores for all scripts that required parallel computing
- All parallel computations in this paper were run on the FASRC Cannon cluster supported by the FAS Division of Science Research Computing Group at Harvard University or on Amazon Web Services


