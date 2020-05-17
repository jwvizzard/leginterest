# James W. Vizzard
# PUBP 804
# Final Project
# 2020.05.15


# Zero-inflated Negative Binomial Model

import delimited "/home/james/Documents/Drive/James/201801-20YYMM_PhD/C_PUBP80> 4_MultivariateStatistics/data/mil_amends.csv", clear 

summarize amend_count_116_2500 oversight distance_km

histogram amend_count_116_2500, discrete freq

# Zero-Inflated Negative Binomial Model
zinb amend_count_116_2500 distance_km, inflate(oversight) zip

zinb amend_count_116_2500 distance_km, inflate(oversight) robust
