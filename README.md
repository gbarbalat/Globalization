# Globalization and OUD

This ecological analysis aims to relate globalization to DALYs for mental and substance use disorders (age standardised per country).  
P: country-wise analysis only on countries with a relatively high quality of data collection according to the Global Burden of Disease study  
E: Globalization according to the globalization index, obtained from the KOF Swiss Economic Institute  
C: Various levels of globalization are compared  
O: DALYs for a variety of mental, behavioural and addiction disorders, as well as a musculosquelettal disorders, obtained from the GBD study.  

Analysis performed on 2019 DALYs, with corresponding 1990 DALYs as a baseline.  
Many covariates were included, such as employment rate, inequalities, population etc ...  
Statistical analysis employed targeted maximum likelihood estimation using the lmtp package.  

1- Read and clean data  
using read_clean.R and read_clean_AllGI.R (the latter uses all globalization indices).

2- Perform the analysis  
using process_all_quality etc ... scripts  
sensitA changes the nature of the globalization index (economic, social, cultural)  
sensitY changes the outcome  
sensittrim changes the threshold for trimming inverse probability weights  

3- Print results  
with the print_result_quality Rmd files  

For more details, see our paper: Association of globalization with the burden of opioid use disorders 2019. A country-level analysis using targeted maximum likelihood estimation (https://globalizationandhealth.biomedcentral.com/articles/10.1186/s12992-023-00980-3)

