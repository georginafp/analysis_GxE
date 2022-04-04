# Study of the combined effect of maternal tobacco smoking and polygenic risk scores on birth weight and body mass index in childhood

The repository contains the commands to study the combined effect of maternal tobacco smoking and polygenic risk scores (PRSs) on birth weight (BW) and body mass index (BMI) related traits in childhood in the Human Early Life Exposome (HELIX) project. The HELIX project contains data from 1,304 children aged between six and eleven years old, from six on-going European cohorts. A PRS is an estimate of an individual’s genetic liability to a trait or disease, calculated according to their genotype profile and relevant genome-wide association study (GWAS) data. In this study we calculated PRS for 4 different traits and tested their association with maternal smoking in relation to the phenotypic traits.

## Preamble

*1.* Code to prepare base GWAS data.

*2.* Code to prepare phenotypic data to estimate PRS.

*3-6.* Codes to compute the PRS for BW, BMI, waist circumference (WC), fat mass (FM), and to test their associations with the phenotypic traits using PRSice v2.

*7.* Code to scale PRS and create boxplots boxplots.

*8.* Code to create the database of the study (smoking + PRS + traits + covariates), create PRS tertiles, and make  descriptive analysis.

*9.* Code to estimate the correlation among PRS and phenotypic traits, and their plot.

*10.* Code for the association analyses (linear regression models adjusted for covariates with and without interaction terms).
