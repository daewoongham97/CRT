# Readme File for "Using Machine Learning to Test Causal Hypotheses in Conjoint Analysis" by Ham, Imai, and Janson. (2022)
# All scripts were written and edited by Dae Woong Ham on 01/27/2022

## Code Overview ## 

# Plotting previous empirical results (Fig 1, Fig 2)
- "Section2_AMCE_plots/immigration_Fig1.R" produces Figure 1 AMCE plots based on original AMCE estimates
- "Section2_AMCE_plots/gender_Fig2.R" produces Figure 2 AMCE plots based on original AMCE estimates

# All simulation plots (Fig 3, 4, 5, 6, 7)
- All simulations are plotted through "Simulations/all_simulation_plots.R" file
- All simulation scripts are executed through "source/left_fig_simulation.sh" or "source/right_fig_simulation.sh"
- "Simulations/Section4/Figure3_leftplot.R"/"Simulations/Section4/Figure3_rightplot.R" produces results of Fig 3 # 50 and 33 hours of computing time respectively 
- "Simulations/Appendix/Figure4_and_6_leftplot.R"/"Simulations/Section4/Figure4_and_6_rightplot.R" produces results of Fig 4 and 6 # 50 and 33 hours of computing time respectively
- "Simulations/Appendix/Figure5_leftplot.R"/"Simulations/Section4/Figure5_rightplot.R" produces results of Fig 5 # 50 and 33 hours of computing time respectively
- "Simulations/Appendix/Figure7.R" produces results of Fig 7 # less than 5 minutes of computing time on FAS computing cluster

# Obtaining new p-values (Section 5 and Table 1) -- around 40 minutes on computing cluster
- All p-values in Section 5 are summarized and obtained in "Section5_ApplicationResults/pval_analysis.R"
- "Section5_ApplicationResults/Immigration/main_analysis/obs_test_stat.R"/"Section5_ApplicationResults/Immigration/main_analysis/resampled_test_stats.R" produces observed and resampled test statistics to produce p-value in Table 1 row 1 column 1. # 30 minutes of total computing time
- "Section5_ApplicationResults/Immigration/main_analysis/AMCE_pval.do" produces AMCE p-value in Table 1 row 1 column 2. #less than 5 seconds of total computing time
- "Section5_ApplicationResults/Immigration/main_analysis/profile_order_effect.R"/"Section5_ApplicationResults/Immigration/main_analysis/profile_order_effect/resampled_test_stats.R" produces observed and resampled test statistics to produce p-value in Table 1 row 1 column 3. # 10 minutes of total computing time
- "Section5_ApplicationResults/Immigration/main_analysis/carryover_effect_obs_test_stat.R"/"Section5_ApplicationResults/Immigration/main_analysis/carryover_effect_resampled_test_stats.R" produces observed and resampled test statistics to produce p-value in Table 1 row 1 column 4. # 30 minutes of total computing time
- "Section5_ApplicationResults/Immigration/main_analysis/fatigue_effect_obs_test_stat.R"/"Section5_ApplicationResults/Immigration/main_analysis/fatigue_effect_resampled_test_stats.R" produces observed and resampled test statistics to produce p-value in Table 1 row 1 column 5. # 24 minutes of computing time
- To obtain p-value for second row repeat above but for "Section5_ApplicationResults/Gender/..." # Approximate computation time is listed in the individual files
- Each application also contains "../lasso_obs_test_stat.R"/"../lasso_resampled_test_stats.R" to produce supplementary main effect analysis in Section 5
- "Section5_ApplicationResults/Immigration/with_ethnocentrism/" contains files to produce p-value when including ethnocentrism in Section 5.1
- "Section5_ApplicationResults/gender/supplementary_analysis/" contains files to produce p-value when performing robustness analysis using second most significant interaction in Appendix 
- "Section5_ApplicationResults/gender/main_analysis/presidential_lasso_explore.R" contains script to find which interaction is strongest in Presidential dataset 

# Other folders
- "data" folder contains all relevant datasets in both Immigration and gender conjoint examples and all the saved results of p-values in simulations and test statistics for Section 5
- "Figures" folder contains all figures
- "source" folder contains all helper and main functions to run above scripts (including data cleaning, obtaining test statistics, generating simulation datasets). In particular "source/hiernet_source.R" contains the main function to compute all HierNet test statistics in the paper. 

## Environment ## 
- R version 4.1.0
- 200 cores for all scripts that required parallel computing
- All parallel computations in this paper were run on the FASRC Cannon cluster supported by the FAS Division of Science Research Computing Group at Harvard University


