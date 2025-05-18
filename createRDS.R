
# original_data <- read.csv("Complete Datasets/HFI_Cato_2023.csv") 
# reference_data <- readRDS("optimizedshiny/datasets/VDEM.rds")
# 
# # Select the columns I want in the data :)
# filtered_data <- original_data %>% select(country = countries,year, 
#                                           human_freedom_score = hf_score, 
#                                           free_movement = pf_movement,
#                                           free_pol_parties = pf_assembly_parties,
#                                           personal_freedom_score = pf_score,
#                                           government_size = ef_government,
#                                           inflation_rate = ef_money_inflation_data,
#                                           tarrifs_revenue = ef_trade_tariffs_revenue_data,
#                                           labor_reg_min_wage_index = ef_regulation_labor_minwage,
#                                           foreign_labor = ef_regulation_labor_foreign
#                                           )
# 
# # Add Market Openness and Regulatory Burden and Internet Censorship and Government Censorship to the above statement :)
# 
# # Sorts country and year together:
# filtered_data <- filtered_data %>% arrange(country, year)
# 
# filtered_data <- filtered_data %>% filter(country == "Hong Kong SAR, China")




