# BRFSS Data Analysis Project

## Overview
The Behavioral Risk Factor Surveillance System (BRFSS) is a CDC statewide telephone survey aimed to gather data on a multitude of health risks and conditions. This project focuses on Arizona and Oklahoma in regards to different health inquiries with a focus on asthma and smoking status. The analysis was performed using SAS on BRFSS data from 2022.

## Project Structure
- The data used can be found at https://www.cdc.gov/brfss/annual_data/annual_2022.html under 2022 BRFSS Data (ASCII)
- 'Evan Becker - BRFSS Code.sas' contains the SAS code for the project
- 'Evan Becker - Oklahoma and Arizona BRFSS.docx' contains the full analysis as well as the figures and tables

## Results
### Comparison of Oklahoma and Arizona
For the comparison of Arizona and Oklahoma, descriptive statistics for the demographic, health, and behavioral factors are presented in Tables 1 and 2. The amount of missing information varied among the variables, with the highest missing data rates observed for BMI group (9%), binge drinking status (12%), and income group (20%). Respondents from Arizona were more likely to be male compared to respondents from Oklahoma (p=0.021).  The age distribution differed between the two states as well (p=<.0001). In Arizona, 32% of respondents were 70 years or older while 25% of respondents from Oklahoma were 70 years or older. The distribution of race/ethnicity also was different in the two states (p<.0001). In Arizona, 8% of respondents reported being Other and Non-Hispanic while 15% of respondents in Oklahoma reported being Other and Non-Hispanic. Looking at health risk variables, residents from Arizona were more likely to binge drink (p=0.0012), additionally, BMI category differed by state (p<.0001). However, smoking status did not differ significantly (p=0.0875). The chronic health condition of interest, asthma, did differ between Arizona and Oklahoma (p=0.0005). Looking at Figure 1, respondents from Oklahoma were more likely to be diagnosed with asthma (18%) compared to respondents from Arizona (16%). Finally, the continuous BMI variable differed by state (p<.0001). The mean BMI from the Arizona respondents was 28.22 while the mean BMI from the Oklahoma respondents was 29.29. 
	
### Comparison by asthma diagnosis in Arizona
The descriptive statistics for the demographic, health, and behavioral factors by asthma diagnosis are presented in Tables 3 and 4. The amount of missing information varied among the variables, with the highest missing data rates observed for BMI group (10%), binge drinking status (13%), and income group (21%). Asthma diagnosis status differed by sex (p<.0001). 40% of respondents who reported having asthma were male, while 48% of respondents who reported not having asthma were male. The distribution of income also differed by diagnosis (p=0.0355). In the asthma group, 50% of respondents reported having an income of $50,000+ annually while 53% of respondents in the no asthma group reported having an income of $50,000+ annually. Looking at health risk variables, binge drinking status did not differ by asthma diagnosis (p=0.6201), however, the distribution of BMI group did differ by asthma status (p<.0001). Moreover, the mean BMI differed by asthma diagnosis (p<.0001). As seen in Table 4, the mean BMI for the asthma group was 29.66 while the mean BMI among those who have not been diagnosed with asthma was 27.95. Finally, the health risk factor of interest, smoking status, did differ in the two groups (p=0.0002). As seen in Figure 2, respondents who have smoked 100+ cigarettes were more likely to report a diagnosis of asthma (18%) compared to respondents who have not smoked 100+ cigarettes (15%). 
 
### Comparison of asthma diagnosis by smoking status, stratified by income group in Arizona
The variable income was stratified to determine if it is a confounding variable (Figure 3). The less than $25,000 annual income who had smoked 100+ cigarettes had the highest prevalence of asthma (20%). In each income group within the sample, the proportion of respondents with asthma who reported smoking 100 or more cigarettes was greater than the proportion of respondents with asthma who reported smoking fewer than 100 cigarettes. However, this difference was only statistically significant in the $50,000+ annual salary group. 
[<$25,000 annual income: p=0.1194, $25,000 to <$50,000 annual income: p=0.1320, $50,000+ annual income: p=0.0413]

### Conclusions:
Through the analysis of BRFSS data, different health care trends have been identified. Although the prevalence of asthma differed between Oklahoma and Arizona, the prevalence of residents who have smoked 100+ cigarettes did not differ by state. However, looking at Arizona, the prevalence of asthma diagnosis was greater in the 100+ cigarette group compared to the less than 100 cigarette group. This shows that in Arizona, those who have smoked 100+ cigarettes are more likely to have an asthma diagnosis compared to those who have not smoked 100+ cigarettes. On top of this, there is statistically significant evidence to suggest that in Arizona, the mean BMI for those who have been diagnosed with asthma was greater than the BMI of those who have not.

