# HDS2012Analysis
A re-analysis of the Housing Discrimination Study (2012) done by the US Department of Housing and Urban Development

All original data is fetched from: https://www.huduser.gov/portal/datasets/hsg_discrimination.html, but is no longer publicly available.

Start by putting raw data into Data/HDS_raw_data then make a Census api key (simple & fast, usually) at  https://api.census.gov/data/key_signup.html, placing it into the api_keys_template.R. Then run data_cleaning.R. With sales_tester.csv and cleaned_hds.csv created you can continue on and run analysis.R which will give modle and latex table output for results tables. 
