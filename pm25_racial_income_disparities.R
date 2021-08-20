### Racial and income disparities in ambient exposure to fine particulate matter in1the United States 
### last update: Aug 19, 2021 

library("tidyverse")
library("ineq")
library("ggplot2")


# SECTION 0 - SETUP ---------------------------------------------------------
# ATTENTION: please rerun the whole script if any of these parameters are changed 

# all available years in dataset
available_years = 2000:2016

# parameter for Atkinson index 
eps = 0.75 

# categorize population into groups by "medhouseholdincome"
# group 1, 2, 3 has low income; 
# group 4, 5, 6, 7 has moderate income; 
# group 8, 9, 10 has high income; 
n=10; r=8; p=3

# different PM levels the data will be categorized into
thresholds=c(seq(6,16,2)) 

# analysis on urban area only
urban = T
rural = F

# function that returns the percentage of ethnic group exposed to PM higher than the input threshold
atkinson.ethnic = function(df, pm.threshold) {
  black_above_thd=sum(df$black_pop[df$pm25>=pm.threshold])/sum(df$black_pop)
  hisp_above_thd=sum(df$hisp_pop[df$pm25>=pm.threshold])/sum(df$hisp_pop)
  native_above_thd=sum(df$native_pop[df$pm25>=pm.threshold])/sum(df$native_pop)
  asian_above_thd=sum(df$asian_pop[df$pm25>=pm.threshold])/sum(df$asian_pop)
  white_above_thd=sum(df$white_pop[df$pm25>=pm.threshold])/sum(df$white_pop)
  output = c(black_above_thd, white_above_thd, native_above_thd, asian_above_thd, hisp_above_thd)
  output[is.na(output)] = 0
  return(output)
}

# function that returns the percentage of income group exposed to PM higher than the input threshold
atkinson.income = function(df, pm.threshold) {
  df.temp = subset(df, pm25 >= pm.threshold)
  numerator = aggregate(df.temp$population, by=list(Category=df.temp$group), FUN=sum)[, 2]
  denominator = aggregate(df$population, by=list(Category=df$group), FUN=sum)[, 2]
  output = numerator / denominator
  output[is.na(output)] = 0
  return(output)
}


# SECTION 1 - READ AND CLEAN DATA---------------------------------------------------------
part1 = read.csv("./data/data_part1.csv", header=TRUE)
part2 = read.csv("./data/data_part2.csv", header=TRUE)
part3 = read.csv("./data/data_part3.csv", header=TRUE)
part4 = read.csv("./data/data_part4.csv", header=TRUE)
part5 = read.csv("./data/data_part5.csv", header=TRUE)

final_pm_data = do.call("rbind", list(part1, part2, part3, part4, part5))
rm(part1, part2, part3, part4, part5)
# write.csv(final_pm_data, "./data/pm25_racial_income_disparities.csv")
# final_pm_data = read.csv("./data/pm25_racial_income_disparities.csv", header=TRUE)
no_na_all_years_pm_data = na.omit(final_pm_data)


# SECTION 2 - DEFINE OUTPUTS OF SCRIPT ---------------------------------------------------------
# Note: All mean values computed are weighted by the population size

# Average yearly PM value (weighted by population in every zipcode)
national_average_pm = numeric(length(available_years)) 

# Percentage of total population between different PM levels
population_between_pm = matrix(0, nrow = length(available_years), ncol = length(thresholds)+1) 

# Percentage between different PM levels
# income_average_pm = matrix(0, nrow = length(available_years), ncol = n) 

# Average yearly PM value by the income groups 
average_pm_poor = numeric(length(available_years))
average_pm_moderate = numeric(length(available_years))
average_pm_rich = numeric(length(available_years))

# Average yearly PM value by the ethnic groups 
black_average_pm = numeric(length(available_years))  
hisp_average_pm = numeric(length(available_years))  
native_average_pm = numeric(length(available_years))  
asian_average_pm = numeric(length(available_years))  
white_average_pm = numeric(length(available_years))

# Average yearly PM value by the ethnic groups 
race_average_pm = data.frame(available_years = available_years,
                             black = 0, hisp = 0, native = 0, asian = 0, white = 0)

# Percentage of total population exposed to the given PM levels and above
# pop_above8 = matrix(0, nrow = length(available_years), ncol = n)
# pop_above10 = matrix(0, nrow = length(available_years), ncol = n)
# pop_above12 = matrix(0, nrow = length(available_years), ncol = n)

# Percentage of income group exposed to the given PM levels
poor_population_between_pm = matrix(0, nrow = length(available_years), ncol = length(thresholds)+1) 
rich_population_between_pm = matrix(0, nrow = length(available_years), ncol = length(thresholds)+1) 
# moderate_population_between_pm = matrix(0, nrow = length(available_years), ncol = length(thresholds)+1) 

# Percentage of ethnic population between different PM levels
black_population_between_pm=matrix(0, nrow = length(available_years), ncol = length(thresholds)+1) 
hisp_population_between_pm=matrix(0, nrow = length(available_years), ncol = length(thresholds)+1)  
native_population_between_pm=matrix(0, nrow = length(available_years), ncol = length(thresholds)+1)  
asian_population_between_pm=matrix(0, nrow = length(available_years), ncol = length(thresholds)+1)  
white_population_between_pm=matrix(0, nrow = length(available_years), ncol = length(thresholds)+1) 

# Atkinson/Gini Index by income group 
Atkinson_PM_level_income = numeric(length(available_years))
Atkinson_PM_level_income_8 = numeric(length(available_years))
Atkinson_PM_level_income_10 = numeric(length(available_years))
Gini_PM_level_income_8 = numeric(length(available_years))
Gini_PM_level_income_10 = numeric(length(available_years))

# Atkinson/Gini Index by ethnic group 
Atkinson_PM_level_ethnic = numeric(length(available_years))
Atkinson_PM_level_ethnic_8 = numeric(length(available_years))
Atkinson_PM_level_ethnic_10 = numeric(length(available_years))
Atkinson_PM_level_ethnic_12 = numeric(length(available_years))
Gini_PM_level_ethnic_8 = numeric(length(available_years))
Gini_PM_level_ethnic_10 = numeric(length(available_years))
Gini_PM_level_ethnic_12 = numeric(length(available_years))

# Percentage of a ethnic group out of the ethinic population between different PM levels
# for example, the percent of white population out of the total USA white population exposed to PM level 8-10ug/m3
pops=c(seq(0,95,1))/100
black_zcta_above_pop=matrix(0, nrow = length(available_years), ncol = length(pops))
hisp_zcta_above_pop=matrix(0, nrow = length(available_years), ncol = length(pops))  
native_zcta_above_pop=matrix(0, nrow = length(available_years), ncol = length(pops))  
asian_zcta_above_pop=matrix(0, nrow = length(available_years), ncol = length(pops))  
white_zcta_above_pop=matrix(0, nrow = length(available_years), ncol = length(pops)) 

# Percentage of a ethnic&income group out of the ethinic population between different PM levels
rich_black_average_pm=numeric(length(available_years))
rich_hisp_average_pm=numeric(length(available_years))
rich_native_average_pm=numeric(length(available_years))
rich_asian_average_pm=numeric(length(available_years))
rich_white_average_pm=numeric(length(available_years))

poor_black_average_pm=numeric(length(available_years))
poor_hisp_average_pm=numeric(length(available_years))
poor_native_average_pm=numeric(length(available_years))
poor_asian_average_pm=numeric(length(available_years))
poor_white_average_pm=numeric(length(available_years))




# SECTION 3 - ANALYSIS: REPEATED FOR EVERY AVAILABLE YEAR ---------------------------------------------------------
for (y in available_years){ 
  year_counter= y - min(available_years) + 1
  
  ################################# SUBSET DATA BY YEAR AND LAND USE ###################################
  pm_data = no_na_all_years_pm_data[no_na_all_years_pm_data$year==y,]
  stopifnot(sum(duplicated(pm_data$zcta)) == 0) # confirm no duplicate in the data
  if (urban&!rural) pm_data=pm_data[which(pm_data$urban==1),] 
  if (!urban&rural) pm_data=pm_data[which(pm_data$urban==0),] 
  
  total_pop = sum(pm_data$population)
  weighted_pm = weighted.mean(pm_data$pm25, pm_data$population)
  ############################## END SUBSET DATA BY YEAR AND LAND USE ##################################

  
  ################################ BEGIN SPLIT DATA INTO INCOME GROUPS ################################
  # Variable used for group splitting
  value = pm_data$medhouseholdincome
  qtile = seq(1/n,1-1/n,1/n)
  Qlabel = c(1:n)

  # categorizing ZCTAs into income groups (1, 2, ..., 10) using percentiles
  g <- with(pm_data, factor(findInterval(value, c(-Inf, quantile(value, probs=c(qtile)), Inf)), labels = Qlabel)) 
  pm_data$group=as.integer(g) 
  
  length_pm = as.array(aggregate(pm_data$population, by=list(Category=pm_data$group), FUN=sum)[, 2]) / sum(pm_data$population)
  
  ################################# END SPLIT DATA INTO INCOME GROUPS #################################

  
  ################################## BEGIN ANALYSIS FOR INCOME GROUPS #################################

  # Average PM values for each income group (weighted by population level)
  avg_pm = data.frame(pm_data %>% group_by(group) %>% summarise(
    pm25 = weighted.mean(pm25, w=population), population = sum(population)))
  avg_pm_poor = data.frame(avg_pm %>% subset(group<=p) %>% summarise(
    pm25 = weighted.mean(pm25, w=population)))[1,1]
  avg_pm_moderate = data.frame(avg_pm %>% subset(group>p & group<r) %>% summarise(
    pm25 = weighted.mean(pm25, w=population)))[1,1]
  avg_pm_rich = data.frame(avg_pm %>% subset(group>=r) %>% summarise(
    pm25 = weighted.mean(pm25, w=population)))[1,1]


  # We find the percentage of people between certain PM levels for: 1) total population 2) poor 3) rich.
  pm_data$pm25bins = findInterval(pm_data$pm25, thresholds)
  pm_data$group3 = findInterval(pm_data$group, c(p+.5, r-.5))
  pm_grouped_by2 = data.frame(pm_data %>% group_by(pm25bins, group3) %>% 
                    summarise(population = sum(population)) %>% spread(group3, population))
  if (dim(pm_grouped_by2)[1] < 7) {
    pm_grouped_by2[7,] = c(6,0,0,0) 
  }
  
  pop_betw_pm=numeric(length(thresholds)+1) # ALL POP

  for (j in c(1:length(thresholds))){
    if(j==1){
      pop_betw_pm[j]=sum(pm_data$population[pm_data$pm25<thresholds[j]])/sum(pm_data$population)

    }else{
      pop_betw_pm[j]=sum(pm_data$population[pm_data$pm25<thresholds[j] & pm_data$pm25>thresholds[j-1]])/sum(pm_data$population)
    }
  }
  pop_betw_pm[7]=sum(pm_data$population[pm_data$pm25>16])/sum(pm_data$population)

  dp_pop_betw_pm = pm_grouped_by2[,2] / sum(pm_grouped_by2[,2], na.rm=TRUE) # Poor
  dm_pop_betw_pm = pm_grouped_by2[,3] / sum(pm_grouped_by2[,3], na.rm=TRUE) # Moderate
  dr_pop_betw_pm = pm_grouped_by2[,4] / sum(pm_grouped_by2[,4], na.rm=TRUE) # Rich
  dp_pop_betw_pm[is.na(dp_pop_betw_pm)] = 0 
  dm_pop_betw_pm[is.na(dm_pop_betw_pm)] = 0
  dr_pop_betw_pm[is.na(dr_pop_betw_pm)] = 0
  
  # test for every decile
  above8=numeric(n); above10=numeric(n); above12=numeric(n)
  for (q in c(1:n)){
    above8[q] = sum(pm_data$population[pm_data$pm25>=8 & pm_data$group==q])/sum(pm_data$population[pm_data$group==q])
    above10[q] = sum(pm_data$population[pm_data$pm25>=10 & pm_data$group==q])/sum(pm_data$population[pm_data$group==q])
    above12[q] = sum(pm_data$population[pm_data$pm25>=12 & pm_data$group==q])/sum(pm_data$population[pm_data$group==q])
  }
  
  # STORE VALUES IN DESIRED OUTPUTS:
  national_average_pm[year_counter]=sum(pm_data$pm25*pm_data$population)/sum(pm_data$population)
  # income_average_pm[year_counter,]=avg_pm$population
  population_between_pm[year_counter,]=pop_betw_pm
  poor_population_between_pm[year_counter,]=dp_pop_betw_pm
  rich_population_between_pm[year_counter,]=dr_pop_betw_pm
  # moderate_population_between_pm[year_counter,]=dm_pop_betw_pm
  average_pm_poor[year_counter]=avg_pm_poor
  average_pm_moderate[year_counter]=avg_pm_moderate
  average_pm_rich[year_counter]=avg_pm_rich
  # pop_above8[year_counter,]=above8
  # pop_above10[year_counter,]=above10
  # pop_above12[year_counter,]=above12
  
  rich_black_weighted_pm=sum(pm_data$pm25[which(pm_data$group>=r)]*pm_data$black_pop[which(pm_data$group>=r)])/sum(pm_data$black_pop[which(pm_data$group>=r)])
  rich_hisp_weighted_pm=sum(pm_data$pm25[which(pm_data$group>=r)]*pm_data$hisp_pop[which(pm_data$group>=r)])/sum(pm_data$hisp_pop[which(pm_data$group>=r)])
  rich_white_weighted_pm=sum(pm_data$pm25[which(pm_data$group>=r)]*pm_data$white_pop[which(pm_data$group>=r)])/sum(pm_data$white_pop[which(pm_data$group>=r)])
  rich_native_weighted_pm=sum(pm_data$pm25[which(pm_data$group>=r)]*pm_data$native_pop[which(pm_data$group>=r)])/sum(pm_data$native_pop[which(pm_data$group>=r)])
  rich_asian_weighted_pm=sum(pm_data$pm25[which(pm_data$group>=r)]*pm_data$asian_pop[which(pm_data$group>=r)])/sum(pm_data$asian_pop[which(pm_data$group>=r)])
  
  poor_black_weighted_pm=sum(pm_data$pm25[which(pm_data$group<=p)]*pm_data$black_pop[which(pm_data$group<=p)])/sum(pm_data$black_pop[which(pm_data$group<=p)])
  poor_hisp_weighted_pm=sum(pm_data$pm25[which(pm_data$group<=p)]*pm_data$hisp_pop[which(pm_data$group<=p)])/sum(pm_data$hisp_pop[which(pm_data$group<=p)])
  poor_white_weighted_pm=sum(pm_data$pm25[which(pm_data$group<=p)]*pm_data$white_pop[which(pm_data$group<=p)])/sum(pm_data$white_pop[which(pm_data$group<=p)])
  poor_native_weighted_pm=sum(pm_data$pm25[which(pm_data$group<=p)]*pm_data$native_pop[which(pm_data$group<=p)])/sum(pm_data$native_pop[which(pm_data$group<=p)])
  poor_asian_weighted_pm=sum(pm_data$pm25[which(pm_data$group<=p)]*pm_data$asian_pop[which(pm_data$group<=p)])/sum(pm_data$asian_pop[which(pm_data$group<=p)])
  
  
  rich_black_average_pm[year_counter]=rich_black_weighted_pm
  rich_hisp_average_pm[year_counter]=rich_hisp_weighted_pm
  rich_native_average_pm[year_counter]=rich_native_weighted_pm
  rich_asian_average_pm[year_counter]=rich_asian_weighted_pm
  rich_white_average_pm[year_counter]=rich_white_weighted_pm
  
  poor_black_average_pm[year_counter]=poor_black_weighted_pm
  poor_hisp_average_pm[year_counter]=poor_hisp_weighted_pm
  poor_native_average_pm[year_counter]=poor_native_weighted_pm
  poor_asian_average_pm[year_counter]=poor_asian_weighted_pm
  poor_white_average_pm[year_counter]=poor_white_weighted_pm
  
  ################################## END ANALYSIS FOR INCOME GROUPS ###################################
    
    
  ################################# BEGIN ANALYSIS PER ETHNIC GROUPS ##################################
  pm_data$pm25bins = findInterval(pm_data$pm25, thresholds)
  
   # Average values per ethnic group
  black_weighted_pm = weighted.mean(pm_data$pm25, w=pm_data$black_pop)
  hisp_weighted_pm = weighted.mean(pm_data$pm25, w=pm_data$hisp_pop) 
  white_weighted_pm = weighted.mean(pm_data$pm25, w=pm_data$white_pop) 
  native_weighted_pm = weighted.mean(pm_data$pm25, w=pm_data$native_pop)
  asian_weighted_pm = weighted.mean(pm_data$pm25, w=pm_data$asian_pop)
  
  # Number of ethic people between PM levels
  black_betw_pm=numeric(length(thresholds)+1)
  hisp_betw_pm=numeric(length(thresholds)+1)
  native_betw_pm=numeric(length(thresholds)+1)
  asian_betw_pm=numeric(length(thresholds)+1)
  white_betw_pm=numeric(length(thresholds)+1)

  for (j in c(1:length(thresholds))){
    if(j==1){
      black_betw_pm[j]=sum(pm_data$black_pop[pm_data$pm25<thresholds[j]])/sum(pm_data$black_pop)
      hisp_betw_pm[j]=sum(pm_data$hisp_pop[pm_data$pm25<thresholds[j]])/sum(pm_data$hisp_pop)
      native_betw_pm[j]=sum(pm_data$native_pop[pm_data$pm25<thresholds[j]])/sum(pm_data$native_pop)
      asian_betw_pm[j]=sum(pm_data$asian_pop[pm_data$pm25<thresholds[j]])/sum(pm_data$asian_pop)
      white_betw_pm[j]=sum(pm_data$white_pop[pm_data$pm25<thresholds[j]])/sum(pm_data$white_pop)

    }else{
      black_betw_pm[j]=sum(pm_data$black_pop[pm_data$pm25<thresholds[j] & pm_data$pm25>thresholds[j-1]])/sum(pm_data$black_pop)
      hisp_betw_pm[j]=sum(pm_data$hisp_pop[pm_data$pm25<thresholds[j] & pm_data$pm25>thresholds[j-1]])/sum(pm_data$hisp_pop)
      native_betw_pm[j]=sum(pm_data$native_pop[pm_data$pm25<thresholds[j] & pm_data$pm25>thresholds[j-1]])/sum(pm_data$native_pop)
      asian_betw_pm[j]=sum(pm_data$asian_pop[pm_data$pm25<thresholds[j] & pm_data$pm25>thresholds[j-1]])/sum(pm_data$asian_pop)
      white_betw_pm[j]=sum(pm_data$white_pop[pm_data$pm25<thresholds[j] & pm_data$pm25>thresholds[j-1]])/sum(pm_data$white_pop)
    }
  }
  black_betw_pm[7]=sum(pm_data$black_pop[pm_data$pm25>16])/sum(pm_data$black_pop)
  hisp_betw_pm[7]=sum(pm_data$hisp_pop[pm_data$pm25>16])/sum(pm_data$hisp_pop)
  native_betw_pm[7]=sum(pm_data$native_pop[pm_data$pm25>16])/sum(pm_data$native_pop)
  asian_betw_pm[7]=sum(pm_data$asian_pop[pm_data$pm25>16])/sum(pm_data$asian_pop)
  white_betw_pm[7]=sum(pm_data$white_pop[pm_data$pm25>16])/sum(pm_data$white_pop)
  
  
  # STORE VALUES IN DESIRED OUTPUTS:
  black_average_pm[year_counter]=black_weighted_pm
  hisp_average_pm[year_counter]=hisp_weighted_pm
  native_average_pm[year_counter]=native_weighted_pm
  asian_average_pm[year_counter]=asian_weighted_pm
  white_average_pm[year_counter]=white_weighted_pm
  
  race_average_pm$black[year_counter]=black_weighted_pm
  race_average_pm$hisp[year_counter]=hisp_weighted_pm
  race_average_pm$native[year_counter]=native_weighted_pm
  race_average_pm$asian[year_counter]=asian_weighted_pm
  race_average_pm$white[year_counter]=white_weighted_pm
  
  black_population_between_pm[year_counter,]=black_betw_pm
  hisp_population_between_pm[year_counter,]=hisp_betw_pm
  native_population_between_pm[year_counter,]=native_betw_pm
  asian_population_between_pm[year_counter,]=asian_betw_pm
  white_population_between_pm[year_counter,]=white_betw_pm

  ################################## END ANALYSIS PER ETHNIC GROUPS ###################################
  
  black_above_pop=numeric(length(pops))
  hisp_above_pop=numeric(length(pops))
  native_above_pop=numeric(length(pops))
  asian_above_pop=numeric(length(pops))
  white_above_pop=numeric(length(pops))

  for (j in c(1:length(pops))){
    black_above_pop[j] = weighted.mean(pm_data$pm25[pm_data$pct_blk>=pops[j]], 
                                       pm_data$black_pop[pm_data$pct_blk>=pops[j]])
    hisp_above_pop[j] = weighted.mean(pm_data$pm25[pm_data$pct_hisp>=pops[j]], 
                                      pm_data$hisp_pop[pm_data$pct_hisp>=pops[j]])
    native_above_pop[j] = weighted.mean(pm_data$pm25[pm_data$pct_native>=pops[j]], 
                                        pm_data$native_pop[pm_data$pct_native>=pops[j]])
    asian_above_pop[j] = weighted.mean(pm_data$pm25[pm_data$pct_asian>=pops[j]], 
                                       pm_data$asian_pop[pm_data$pct_asian>=pops[j]])
    white_above_pop[j] = weighted.mean(pm_data$pm25[pm_data$pct_white>=pops[j]], 
                                       pm_data$white_pop[pm_data$pct_white>=pops[j]])
  }

  black_zcta_above_pop[year_counter,]=black_above_pop 
  hisp_zcta_above_pop[year_counter,]=hisp_above_pop 
  native_zcta_above_pop[year_counter,]=native_above_pop 
  asian_zcta_above_pop[year_counter,]=asian_above_pop 
  white_zcta_above_pop[year_counter,]=white_above_pop 

  ################################## ATKINSON INDEX ################################### TODO 
  # Atkinson index by income groups
  AI_income=1-(sum(length_pm*(avg_pm$pm25/weighted_pm)^(1-eps)))^(1/(1-eps))
  Atkinson_PM_level_income[year_counter]=AI_income
  
  # Atkinson index by income groups when PM>=8 and when PM>=10
  Atkinson_PM_level_income_8[year_counter]=Atkinson(above8, parameter = eps)
  Atkinson_PM_level_income_10[year_counter]=Atkinson(above10,  parameter = eps)
  Gini_PM_level_income_8[year_counter]=Gini(above8, n = rep(1, length(above8)))#,unbiased = FALSE)
  Gini_PM_level_income_10[year_counter]=Gini(above10, n = rep(1, length(above10)))#, unbiased = FALSE)
  
  # Atkinson index by ethnic groups
  #AI_race=1 - (sum(pm_data$black_pop)/total_pop * (black_weighted_pm/weighted_pm)^(1-eps)+
              # sum(pm_data$hisp_pop)/total_pop * (hisp_weighted_pm/weighted_pm)^(1-eps)+
              # sum(pm_data$white_pop)/total_pop * (white_weighted_pm/weighted_pm)^(1-eps)+
              # sum(pm_data$asian_pop)/total_pop * (asian_weighted_pm/weighted_pm)^(1-eps)+
              # sum(pm_data$native_pop)/total_pop * (native_weighted_pm/weighted_pm)^(1-eps)) ^ (1/(1-eps))
  #Atkinson_PM_level_ethnic[year_counter]=AI_race

  # Atkinson index by ethnic groups when PM>=8
  x8_atkinson = atkinson.ethnic(df=pm_data, pm.threshold=8)
  Atkinson_PM_level_ethnic_8[year_counter]=Atkinson(x8_atkinson, parameter = eps)
  Gini_PM_level_ethnic_8[year_counter]=Gini(x8_atkinson, n = rep(1, length(x8_atkinson)))#, unbiased = FALSE)
  
  
  # Atkinson index by ethnic groups when PM>=10
  x10_atkinson = atkinson.ethnic(df=pm_data, pm.threshold=10)
  Atkinson_PM_level_ethnic_10[year_counter]=Atkinson(x10_atkinson,parameter = eps)
  Gini_PM_level_ethnic_10[year_counter]=Gini(x10_atkinson, n = rep(1, length(x10_atkinson)))#, unbiased = FALSE)
  
  
  # Atkinson index by ethnic groups when PM>=12
  x12_atkinson = atkinson.ethnic(df=pm_data, pm.threshold=12)
  Atkinson_PM_level_ethnic_12[year_counter]=Atkinson(x12_atkinson,parameter = eps)
  Gini_PM_level_ethnic_12[year_counter]=Gini(x12_atkinson, n = rep(1, length(x12_atkinson)))#, unbiased = FALSE)
  
  ################################## END ATKINSON INDEX ###################################
}


### CoV for race ------------------------------------------------------------------------------------
b8= apply(black_population_between_pm, 1, function(x) 1-(x[1]+x[2]))
w8= apply(white_population_between_pm, 1, function(x) 1-(x[1]+x[2]))
h8= apply(hisp_population_between_pm, 1, function(x) 1-(x[1]+x[2]))
n8= apply(native_population_between_pm, 1, function(x) 1-(x[1]+x[2]))
a8= apply(asian_population_between_pm, 1, function(x) 1-(x[1]+x[2]))
T8=apply(population_between_pm, 1, function(x) 1-(x[1]+x[2]))
eaverage8=(b8+w8+h8+n8+a8)/5
eineq8=((abs(b8-eaverage8)^2+abs(w8-eaverage8)^2+abs(h8-eaverage8)^2+abs(a8-eaverage8)^2+abs(n8-eaverage8)^2)/5)/eaverage8^2
eineq8 = sqrt(eineq8)

b10= apply(black_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]))
w10= apply(white_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]))
h10= apply(hisp_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]))
n10= apply(native_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]))
a10= apply(asian_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]))
T10=apply(population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]))
eaverage10=(b10+w10+h10+n10+a10)/5
eineq10=((abs(b10-eaverage10)^2+abs(w10-eaverage10)^2+abs(h10-eaverage10)^2+abs(a10-eaverage10)^2+abs(n10-eaverage10)^2)/5)/eaverage10^2
eineq10 = sqrt(eineq10) 

b12= apply(black_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]+x[4]))
w12= apply(white_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]+x[4]))
h12= apply(hisp_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]+x[4]))
n12= apply(native_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]+x[4]))
a12= apply(asian_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]+x[4]))
T12=apply(population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]+x[4]))
eaverage12=(b12+w12+h12+n12+a12)/5
eineq12=((abs(b12-eaverage12)^2+abs(w12-eaverage12)^2+abs(h12-eaverage12)^2+abs(a12-eaverage12)^2+abs(n12-eaverage12)^2)/5)/eaverage12^2
eineq12 = sqrt(eineq12)

### National Figure 4 
f4 = data.frame(year = c(available_years, available_years, available_years),
                exposure = c(rowSums(population_between_pm[,3:7]), 
                             rowSums(population_between_pm[,4:7]), 
                             rowSums(population_between_pm[,5:7])) * 100, 
                disparity = c(eineq8, eineq10, eineq12),
                group = c(rep("above8", 17), rep("above10", 17), rep("above12", 17)))
f4$group = as.factor(f4$group)
f4$group = factor(f4$group, levels = c("above8", "above10", "above12"))





### plot parameters
width2 = 89
height2 = 66
base_size2 = 7
base_line2 = 1

width3 = 60
height3 = 45
base_size3 = 5
base_line3 = 0.5
res = 300

pm_colors = c(rgb(76,98,143,255,maxColorValue=255),
              rgb(155,197,126,255,maxColorValue=255), 
              rgb(242,155,110,255,maxColorValue=255))

income_lines = c("11", "longdash")
race_lables = c("Black", "White", "Hispanic", "Asian", "Native American")
pm_labels = c("Threshold=8","Threshold=10", "Threshold=12")
eps_colors = c("#FFEDA0", "#FEB24C", "#FC4E2A", "#BD0026", "#800026")

eps.list = c(0.25, 0.5, 0.75, 1.0, 2.0)
eps.labs = c("eps=0.25", "eps=0.5", "eps=0.75", "eps=1", "eps=2")

if (urban==T&rural==T) {
  ### Figure 4
  png("./output/inequality_ethnic_cov_updated_has_title.jpeg", units="mm", width=183, height=120, res=res)
  # *** 
  # png("inequality_ethnic_cov_updated_for_legend.jpeg", units="mm", width=183, height=120, res=res)
  # *** 
  ggplot(data=f4) +
    geom_bar(aes(x = year, y = disparity*150, fill=group), stat ="identity", position="dodge") +
    geom_line(aes(x = year, y = exposure, colour=group), size=base_line2) +
    theme_classic(base_size=14) + 
    scale_fill_manual(name="Disparities among racial/ethnic groups", values = pm_colors, labels=pm_labels, 
                      guide = guide_legend(title.position = "top", nrow = 1)) + 
    scale_color_manual(name="Population living in pollution (%)", values = pm_colors, labels=pm_labels, 
                       guide = guide_legend(title.position = "top", nrow = 1)) +
    scale_y_continuous(name="Population living in pollution (%)", limits = c(0, 100),
                       sec.axis = sec_axis(~./150, name="Disparities among racial/ethnic groups")) + 
    guides(line = guide_legend(order = 0, title.position="top"), 
           color = guide_legend(order = 1, title.position="top")) + 
    # *** 
    # guides(fill = guide_legend(order = 2), color = guide_legend(order = 1)) +
    # *** 
    theme(plot.margin=unit(c(2,0,0,0),"mm"), 
          legend.spacing = unit(2, "lines"),
          legend.margin=margin(0,1,0,0),
          # *** 
          # legend.position = "bottom", legend.box="horizontal", legend.title=element_blank(), legend.text = element_text(size = 9),
          # *** 
          legend.position = "bottom", legend.box="horizontal", legend.title=element_text(size = 9), legend.text = element_text(size = 9),
          # *** 
          # legend.position = "bottom", legend.box="horizontal", legend.title=element_text(size = 9), legend.text = element_text(size = 9),
          # *** 
          legend.box.margin=margin(-5,-5,0,0),
          legend.key.width = unit(1., "line")) 
  
  dev.off()

  
  ### Figure A.1
  # png("./output/national_pm_update2.jpeg", units="mm", width=width3, height=height3, res=res)
  a1 = ggplot(data=NULL,aes(x=available_years,y=national_average_pm)) + 
    geom_line(size=base_line3) + 
    scale_y_continuous(limits=c(5,15.5), breaks=5:15) + 
    ylab(expression(paste('Average PM'['2.5']*' (',mu,'g/m'^"3"*')'))) + 
    xlab("Year") + 
    theme_classic(base_size=base_size2) + 
    theme(plot.margin=unit(c(2,2,2,2), "mm"),
          legend.text = element_text(size = base_size2)) + 
    geom_text(data=data.frame(x = c(2000), y = c(15), label = c("a")), 
              aes(x=x, y=y, label=label), fontface="bold")
  # dev.off()
  
  ### Figure A.4a 
  data_temp = gather(race_average_pm, race, value, black:white, factor_key=TRUE)
  data_temp$race = as.factor(data_temp$race)
  data_temp$race = factor(data_temp$race, levels = c("black", "white", "hisp", "asian", "native"))

  # png("./output/ethnic_pm_update2.jpeg", units="mm", width=width3, height=height3, res=res)
  a4a = ggplot(data=data_temp) + 
    geom_line(aes(x=available_years,y=value,color=race), size=base_line3) + 
    scale_y_continuous(limits=c(5,15.5), breaks=5:15) + 
    ylab(expression(paste('Average PM'['2.5']*' (',mu,'g/m'^"3"*')'))) + 
    xlab("Year") + 
    theme_classic(base_size=base_size2) + 
    theme(plot.margin=unit(c(2,2,2,2), "mm"),
          legend.position = c(1, 1), 
          legend.justification = c("right", "top"), 
          legend.title = element_blank(), 
          legend.text = element_text(size = base_size3), 
          legend.key.size = unit(0, "mm"),
          legend.key.width = unit(1.5, "line")) + 
    scale_color_discrete(labels = race_lables) + 
    geom_text(data=data.frame(x = c(2000-0.5), y = c(15), label = c("b")), 
               aes(x=x, y=y, label=label), fontface="bold")
  # dev.off()
  
  ### Figure A.4b
  race_zcta_above_pop_2016 = data.frame(pops=pops, 
                                        black=black_zcta_above_pop[17,], 
                                        hisp=hisp_zcta_above_pop[17,],
                                        native=native_zcta_above_pop[17,], 
                                        asian=c(asian_zcta_above_pop[17,1:60], rep(NA, 36)), 
                                        white=white_zcta_above_pop[17,])
  race_zcta_above_pop_2016 = gather(race_zcta_above_pop_2016, race, value, black:white, factor_key=TRUE)
  race_zcta_above_pop_2016$race = as.factor(race_zcta_above_pop_2016$race)
  race_zcta_above_pop_2016$race = factor(race_zcta_above_pop_2016$race, 
                                         levels = c("black", "white", "hisp", "asian", "native"))
  
  # png("./output/density_pm_update2.jpeg", units="mm", width=width3, height=height3, res=res)
  a4b = ggplot(data=race_zcta_above_pop_2016) + 
    geom_line(aes(x=pops,y=value,color=race), size=base_line3) + 
    scale_y_continuous(limits=c(2,10), breaks=2:10) +
    scale_x_continuous(limits=c(0,0.96), breaks=c(1:10)/10) +  
    ylab(expression(paste('Average PM'['2.5']*' (',mu,'g/m'^"3"*')'))) + 
    xlab("Ethnic population in ZCTA (fraction)") + 
    theme_classic(base_size=base_size2) + 
    scale_color_discrete(labels = race_lables) + 
    theme(plot.margin=unit(c(2,2,2,2), "mm"),
          legend.position = c(1, .5), 
          legend.justification = c("right", "top"), 
          legend.title = element_blank(), 
          legend.text = element_text(size = base_size3-1),
          legend.spacing = unit(0,"mm"), 
          legend.key = element_rect(color = NA, fill = NA),
          legend.key.size = unit(0, "mm"),
          legend.key.width = unit(1.5, "line")) + 
    geom_text(data=data.frame(x = c(0), y = c(10), label = c("c")), 
              aes(x=x, y=y, label=label), fontface="bold")
  # dev.off()

  ### Figure A.5
  average_pm_income = data.frame(years=available_years, 
                                 low=average_pm_poor,
                                 high=average_pm_rich)
  average_pm_income = gather(average_pm_income, income, value, low:high, factor_key=TRUE)
  average_pm_income$income = as.factor(average_pm_income$income)
  average_pm_income$income = factor(average_pm_income$income, levels = c("low", "high"))
  
  # png("./output/income_pm_update2.jpeg", units="mm", width=width3, height=height3, res=res)
  a5 = ggplot(data=average_pm_income) + 
    geom_line(aes(x=years,y=value,linetype=income), size=base_line3) + 
    scale_y_continuous(limits=c(5,15.5), breaks=5:15) + 
    ylab(expression(paste('Average PM'['2.5']*' (',mu,'g/m'^"3"*')'))) + 
    xlab("Year") + 
    theme_classic(base_size=base_size2) + 
    scale_linetype_manual(values=income_lines, labels = c("Low income", "High income")) +
    theme(plot.margin=unit(c(2,2,2,2), "mm"),
          legend.position = c(1, 1), 
          legend.justification = c("right", "top"), 
          legend.title = element_blank(), 
          legend.spacing = unit(0,"mm"), 
          legend.text = element_text(size = base_size3),
          legend.key.size = unit(0, "mm"),
          legend.key.width = unit(1.5, "line")) + 
    geom_text(data=data.frame(x = c(2000), y = c(15), label = c("d")), 
              aes(x=x, y=y, label=label), fontface="bold")
  # dev.off()
  
  ### Figure A.6a
  rich_race_average_pm = data.frame(years=available_years, 
                                    black=rich_black_average_pm, 
                                    hisp=rich_hisp_average_pm, 
                                    asian=rich_asian_average_pm,
                                    native=rich_native_average_pm, 
                                    white=rich_white_average_pm)
  rich_race_average_pm = gather(rich_race_average_pm, race, value, black:white, factor_key=TRUE)
  rich_race_average_pm$race = as.factor(rich_race_average_pm$race)
  rich_race_average_pm$race = factor(rich_race_average_pm$race, 
                                     levels = c("black", "white", "hisp", "asian", "native"))
  
  # png("./output/HI_Ethnic_update2.jpeg", units="mm", width=width3, height=height3, res=res)
  a6a = ggplot(data=rich_race_average_pm) + 
    geom_line(aes(x=years,y=value,linetype=income_lines[2],col=race), size=base_line3) + 
    scale_y_continuous(limits=c(5,15.5), breaks=5:15) + 
    ylab(expression(paste('Average PM'['2.5']*' (',mu,'g/m'^"3"*')'))) + 
    xlab("Year") + theme_classic(base_size=base_size2) + 
    scale_color_discrete(labels = paste("High income", race_lables)) + 
    scale_linetype_manual(values=c(income_lines[2]), guide = FALSE) + 
    theme(plot.margin=unit(c(2,2,2,2), "mm"),
          legend.position = c(1, 1), 
          legend.justification = c("right", "top"), 
          legend.title = element_blank(),
          legend.text = element_text(size = base_size3-1),
          legend.spacing = unit(0,"mm"), 
          legend.key.size = unit(0, "mm"),
          legend.key.width = unit(1.0, "line")) + 
    geom_text(data=data.frame(x = c(2000), y = c(15), label = c("f")), 
              aes(x=x, y=y, label=label), fontface="bold") + 
    guides(colour = guide_legend(override.aes = list(linetype = "dashed")), fill = "blue") 
  # dev.off()

  ### Figure A.6b 
  poor_race_average_pm = data.frame(years=available_years, 
                                    black=poor_black_average_pm, 
                                    hisp=poor_hisp_average_pm, 
                                    asian=poor_asian_average_pm,
                                    native=poor_native_average_pm, 
                                    white=poor_white_average_pm)
  poor_race_average_pm = gather(poor_race_average_pm, race, value, black:white, factor_key=TRUE)
  poor_race_average_pm$race = as.factor(poor_race_average_pm$race)
  poor_race_average_pm$race = factor(poor_race_average_pm$race, 
                                     levels = c("black", "white", "hisp", "asian", "native"))
  
  # png("./output/LI_Ethnic_update2.jpeg", units="mm", width=width3, height=height3, res=res)
  a6b = ggplot(data=poor_race_average_pm) + 
    geom_line(aes(x=years,y=value,linetype=income_lines[1],col=race), size=base_line3) + 
    scale_y_continuous(limits=c(5,15.5), breaks=5:15) + 
    ylab(expression(paste('Average PM'['2.5']*' (',mu,'g/m'^"3"*')'))) + 
    xlab("Year") + theme_classic(base_size = base_size2) + 
    scale_color_discrete(labels = paste("Low income", race_lables)) + 
    scale_linetype_manual(values=c(income_lines[1]), guide = FALSE) + 
    theme(plot.margin=unit(c(2,2,2,2), "mm"),
          legend.position = c(1, 1), 
          legend.justification = c("right", "top"), 
          legend.title = element_blank(), 
          legend.text = element_text(size = base_size3-1),
          legend.spacing = unit(0,"mm"), 
          legend.key = element_rect(color = NA, fill = NA),
          legend.key.size = unit(0, "mm"),
          legend.key.width = unit(1.0, "line")) + 
    geom_text(data=data.frame(x = c(2000-0.5), y = c(15), label = c("e")), 
              aes(x=x, y=y, label=label), fontface="bold") + 
    guides(colour = guide_legend(override.aes = list(linetype = income_lines[1])))
  # dev.off()
  
  library(ggpubr)
  arrange = ggarrange(a1, a4a, a4b, a5, a6b, a6a, ncol = 2, nrow = 3)
  ggsave("./output/FigA1_2.jpeg", arrange, 
         units="mm", width=width3*2, height=height3*3, dpi = 600, device='png')
  
  ### Figure A.8a Atkinson Index figures
  atkinson_pm_ethnic = data.frame(years=available_years, 
                                  above8=Atkinson_PM_level_ethnic_8, 
                                  above10=Atkinson_PM_level_ethnic_10,
                                  above12=Atkinson_PM_level_ethnic_12)
  atkinson_pm_ethnic = gather(atkinson_pm_ethnic, pm, value, above8:above12, factor_key=TRUE)
  atkinson_pm_ethnic$pm = as.factor(atkinson_pm_ethnic$pm)
  atkinson_pm_ethnic$pm = factor(atkinson_pm_ethnic$pm, 
                                 levels = c("above8", "above10", "above12"))
  
  png("./output/Atkinson_Racial_update2.jpeg", units="mm", width=width3, height=height3, res=res)
  a8a = ggplot(data=atkinson_pm_ethnic) + 
    geom_line(aes(x=years,y=value,col=pm), size=base_line3) + 
    scale_y_continuous(limits=c(0,0.2)) + 
    ylab("Atkinson Index - Racial / Ethnic") + 
    xlab("Year") + theme_classic(base_size=base_size2) + 
    scale_colour_manual(values=pm_colors, labels = pm_labels) + 
    theme(plot.margin=unit(c(2,2,2,2), "mm"),
          legend.position = c(0.6, 1), 
          legend.justification = c("right", "top"), 
          legend.title = element_blank(), 
          legend.spacing = unit(0,"mm"),
          legend.key.size = unit(2, "mm"),
          legend.key.width = unit(1.5, "line")) + 
    geom_text(data=data.frame(x = c(2000), y = c(0.2), label = c("a")), 
              aes(x=x, y=y, label=label), fontface="bold")
  a8a
  dev.off()
  
  
  ## Figure A.8b Gini Index figures
  gini_pm_ethnic = data.frame(years=available_years, 
                              above8=Gini_PM_level_ethnic_8,
                              above10=Gini_PM_level_ethnic_10, 
                              above12=Gini_PM_level_ethnic_12)
  
  gini_pm_ethnic = gather(gini_pm_ethnic, pm, value, above8:above12, factor_key=TRUE)
  gini_pm_ethnic$pm = as.factor(gini_pm_ethnic$pm)
  gini_pm_ethnic$pm = factor(gini_pm_ethnic$pm, 
                                 levels = c("above8", "above10", "above12"))
  
  png("./output/Gini_Racial_update2.jpeg", units="mm", width=width3, height=height3, res=res)
  a8b = ggplot(data=gini_pm_ethnic) + 
    geom_line(aes(x=years,y=value,col=pm), size=base_line3) + 
    scale_y_continuous(limits=c(0.05,0.35)) +
    ylab("Gini Index - Racial / Ethnic") + 
    xlab("Year") + 
    theme_classic(base_size=base_size2) + 
    scale_colour_manual(values=pm_colors, labels = pm_labels) + 
    theme(plot.margin=unit(c(2,2,2,2), "mm"),
          legend.position = c(0.65, 1), 
          legend.justification = c("right", "top"),
          legend.title = element_blank(),
          legend.spacing = unit(0,"mm"), 
          legend.key = element_rect(color = NA, fill = NA),
          legend.key.size = unit(2, "mm"),
          legend.key.width = unit(2, "line")) + 
    geom_text(data=data.frame(x = c(2000), y = c(0.35), label = c("b")), 
              aes(x=x, y=y, label=label), fontface="bold")
  a8b
  dev.off()


  ### Figure A.9a
  pm.threshold = 8
  aktinson_sensitivity8 = matrix(, nrow = length(available_years), ncol = length(eps.list))
  
  for (ieps in 1:length(eps.list)) {
    for (y in available_years){ 
      year_counter= y - min(available_years) + 1
      pm_data = no_na_all_years_pm_data[no_na_all_years_pm_data$year==y,]
      
      # categorizing ZCTAs into income groups (1, 2, ..., 10) using percentiles
      value = pm_data$medhouseholdincome
      qtile = seq(1/n,1-1/n,1/n)
      Qlabel = c(1:n)
      g = with(pm_data, factor(findInterval(value, c(-Inf, quantile(value, probs=c(qtile)), Inf)), labels = Qlabel)) 
      pm_data$group=as.integer(g) 
      
      x_atkinson = atkinson.ethnic(df=pm_data, pm.threshold=pm.threshold)
      aktinson_sensitivity8[year_counter, ieps]=Atkinson(x_atkinson, parameter = eps.list[ieps])
    } 
  }
  aktinson_sensitivity8 = data.frame(aktinson_sensitivity8)
  names(aktinson_sensitivity8) = c("vp25", "vp50", "vp75", "v1", "v2")
  aktinson_sensitivity8$years = available_years
  
  aktinson_sensitivity8 = gather(aktinson_sensitivity8, pm, value, vp25:v2, factor_key=TRUE)
  aktinson_sensitivity8$pm = as.factor(aktinson_sensitivity8$pm)
  aktinson_sensitivity8$pm = factor(aktinson_sensitivity8$pm, levels = c("vp25", "vp50", "vp75", "v1", "v2"))
  
  # png("./output/aktinson_sensitivity8_update.jpeg", units="mm", width=width3, height=height3, res=res)
  a9a = ggplot(data=aktinson_sensitivity8) + 
    geom_line(aes(x=years,y=value,col=pm), size=base_line3) + 
    scale_y_continuous(limits=c(0,0.15)) + 
    ylab("Atkinson Index - Racial / Ethnic") + 
    xlab("Year") + theme_classic(base_size=base_size2) + 
    scale_colour_manual(values=eps_colors, labels = eps.labs) + 
    theme(plot.margin=unit(c(2,2,2,2), "mm"),
          legend.position = c(0.5, 0.88), 
          legend.justification = c("right", "top"), 
          legend.title = element_blank(), 
          legend.spacing = unit(0,"mm"),
          legend.margin = unit(c(0,0,0,0), "mm"),
          legend.key.size = unit(2, "mm"),
          legend.key.width = unit(1.5, "line")) + 
    geom_text(data=data.frame(x = c(2006), y = c(0.148), label = c("a")), 
              label = c(expression(paste('PM'['2.5'], ' threshold of ', '8 ',mu,'g/m'^"3"))),
              aes(x=x, y=y, label=label), 
              size=3,
              fontface="bold")
  # dev.off()
  
  ### Figure A.9b
  pm.threshold = 10
  aktinson_sensitivity10 = matrix(, nrow = length(available_years), ncol = length(eps.list))
  
  for (ieps in 1:length(eps.list)) {
    for (y in available_years){ 
      year_counter= y - min(available_years) + 1
      pm_data = no_na_all_years_pm_data[no_na_all_years_pm_data$year==y,]
      
      # categorizing ZCTAs into income groups (1, 2, ..., 10) using percentiles
      value = pm_data$medhouseholdincome
      qtile = seq(1/n,1-1/n,1/n)
      Qlabel = c(1:n)
      g = with(pm_data, factor(findInterval(value, c(-Inf, quantile(value, probs=c(qtile)), Inf)), labels = Qlabel)) 
      pm_data$group=as.integer(g) 
      
      x_atkinson = atkinson.ethnic(df=pm_data, pm.threshold=pm.threshold)
      aktinson_sensitivity10[year_counter, ieps]=Atkinson(x_atkinson, parameter = eps.list[ieps])
    } 
  }
  
  aktinson_sensitivity10 = data.frame(aktinson_sensitivity10)
  names(aktinson_sensitivity10) = c("vp25", "vp50", "vp75", "v1", "v2")
  aktinson_sensitivity10$years = available_years
  
  aktinson_sensitivity10 = gather(aktinson_sensitivity10, pm, value, vp25:v2, factor_key=TRUE)
  aktinson_sensitivity10$pm = as.factor(aktinson_sensitivity10$pm)
  aktinson_sensitivity10$pm = factor(aktinson_sensitivity10$pm, levels = c("vp25", "vp50", "vp75", "v1", "v2"))
  
  # png("./output/aktinson_sensitivity10_update.jpeg", units="mm", width=width3, height=height3, res=res)
  a9b = ggplot(data=aktinson_sensitivity10) + 
    geom_line(aes(x=years,y=value,col=pm), size=base_line3) + 
    scale_y_continuous(limits=c(0,0.2)) + 
    ylab("Atkinson Index - Racial / Ethnic") + 
    xlab("Year") + theme_classic(base_size=base_size2) + 
    scale_colour_manual(values=eps_colors, labels = eps.labs) + 
    theme(axis.title.y = element_blank(), 
      plot.margin=unit(c(2,2,2,2), "mm"),
          legend.position = c(0.5, 0.88), 
          legend.justification = c("right", "top"), 
          legend.title = element_blank(), 
          legend.spacing = unit(0,"mm"),
          legend.margin = unit(c(0,0,0,0), "mm"),
          legend.key.size = unit(0, "mm"),
          legend.key.width = unit(1.5, "line")) + 
    geom_text(data=data.frame(x = c(2006), y = c(0.198), label = c("b")), 
              label = c(expression(paste(' PM'['2.5'], ' threshold of ', '10 ',mu,'g/m'^"3"))),
              size = 3,
              aes(x=x, y=y, label=label), fontface="bold")
  # dev.off()
  

  ### Figure A.9c
  pm.threshold = 12
  aktinson_sensitivity12 = matrix(, nrow = length(available_years), ncol = length(eps.list))
  
  for (ieps in 1:length(eps.list)) {
    for (y in available_years){ 
      year_counter= y - min(available_years) + 1
      pm_data = no_na_all_years_pm_data[no_na_all_years_pm_data$year==y,]
      
      # categorizing ZCTAs into income groups (1, 2, ..., 10) using percentiles
      value = pm_data$medhouseholdincome
      qtile = seq(1/n,1-1/n,1/n)
      Qlabel = c(1:n)
      g = with(pm_data, factor(findInterval(value, c(-Inf, quantile(value, probs=c(qtile)), Inf)), labels = Qlabel)) 
      pm_data$group=as.integer(g) 
      
      x_atkinson = atkinson.ethnic(df=pm_data, pm.threshold=pm.threshold)
      aktinson_sensitivity12[year_counter, ieps]=Atkinson(x_atkinson, parameter = eps.list[ieps])
    } 
  }
  aktinson_sensitivity12 = data.frame(aktinson_sensitivity12)
  names(aktinson_sensitivity12) = c("vp25", "vp50", "vp75", "v1", "v2")
  aktinson_sensitivity12$years = available_years
  
  aktinson_sensitivity12 = gather(aktinson_sensitivity12, pm, value, vp25:v2, factor_key=TRUE)
  aktinson_sensitivity12$pm = as.factor(aktinson_sensitivity12$pm)
  aktinson_sensitivity12$pm = factor(aktinson_sensitivity12$pm, levels = c("vp25", "vp50", "vp75", "v1", "v2"))
  
  # png("./output/aktinson_sensitivity12_update.jpeg", units="mm", width=width3, height=height3, res=res)
  a9c = ggplot(data=aktinson_sensitivity12) + 
    geom_line(aes(x=years,y=value,col=pm), size=base_line3) + 
    scale_y_continuous(limits=c(0,0.4), labels = scales::number_format(accuracy = 0.01)) + 
    ylab("Atkinson Index - Racial / Ethnic") + 
    xlab("Year") + theme_classic(base_size=base_size2) + 
    scale_colour_manual(values=eps_colors, labels = eps.labs) + 
    theme(axis.title.y = element_blank(), 
          plot.margin=unit(c(2,2,2,2), "mm"),
          legend.position = c(0.5, 0.86), 
          legend.justification = c("right", "top"), 
          legend.title = element_blank(), 
          legend.spacing = unit(0,"mm"),
          legend.margin = unit(c(0,0,0,0), "mm"),
          legend.key.size = unit(2, "mm"),
          legend.key.width = unit(1.5, "line")) + 
    geom_text(data=data.frame(x = c(2006), y = c(0.38), label = c("c")), 
              label = c(expression(paste(' PM'['2.5'], ' threshold of ', '12 ',mu,'g/m'^"3"))),
              size = 3, 
              aes(x=x, y=y, label=label), fontface="bold")
  # dev.off()

  arrange3 = ggarrange(a9a, a9b, a9c, ncol = 2, nrow = 2)
  ggsave("./output/FigA9_2.jpeg", arrange3, 
         units="mm", width=width3*2, height=height3*2, dpi = 600, device='png')
}

### Figure A.10 ---------------------------------------------------------------------------------

### National Figure A.1
if (urban==T&rural==T) {
  file_name = "./output/f10_national_a.jpeg"
  ilabel = "a"
} else if (urban==T&rural==F) {
  file_name = "./output/f10_urban_b.jpeg"
  ilabel = "b"
} else {
  file_name = "./output/f10_rural_c.jpeg"
  ilabel = "c"}

png(file_name, units="mm", width=width3, height=height3, res=res)
p1 = ggplot(data=NULL,aes(x=available_years,y=national_average_pm)) + 
  geom_line(size=base_line3) + 
  scale_y_continuous(limits=c(6,14), breaks=6:14) + 
  ylab(expression(paste('Average PM'['2.5']*' (',mu,'g/m'^"3"*')'))) + 
  xlab("Year") + 
  theme_classic(base_size=base_size2) + 
  theme(plot.margin=unit(c(2,2,2,2), "mm"),
        legend.text = element_text(size = base_size2)) + 
  geom_text(data=data.frame(x = c(2000), y = c(14), label = c(ilabel)), 
            aes(x=x, y=y, label=label), fontface="bold")
if (!(urban==T&rural==T)) {
  p1 = p1 + theme(axis.title.y = element_blank(), 
                  axis.text.y = element_blank()) }
p1
dev.off()


### National Figure A.4a 
if (urban==T&rural==T) {
  file_name = "./output/f10_national_d.jpeg"
  ilabel = "d"
} else if (urban==T&rural==F) {
  file_name = "./output/f10_urban_e.jpeg"
  ilabel = "e"
} else {
  file_name = "./output/f10_rural_f.jpeg"
  ilabel = "f"}

data_temp = gather(race_average_pm, race, value, black:white, factor_key=TRUE)
data_temp$race = as.factor(data_temp$race)
data_temp$race = factor(data_temp$race, levels = c("black", "white", "hisp", "asian", "native"))

png(file_name, units="mm", width=width3, height=height3, res=res)
p2 = ggplot(data=data_temp) + 
  geom_line(aes(x=available_years,y=value,color=race), size=base_line3) + 
  scale_y_continuous(limits=c(4,15), breaks=4:14) + 
  ylab(expression(paste('Average PM'['2.5']*' (',mu,'g/m'^"3"*')'))) + 
  xlab("Year") + 
  theme_classic(base_size=base_size2) + 
  theme(plot.margin=unit(c(2,2,2,2), "mm"),
        legend.position = c(1, 1), 
        legend.justification = c("right", "top"), 
        legend.title = element_blank(), 
        legend.text = element_text(size = base_size3), 
        legend.key.size = unit(0, "mm"),
        legend.key.width = unit(1.5, "line")) + 
  scale_color_discrete(labels = race_lables) + 
  geom_text(data=data.frame(x = c(2000), y = c(15), label = c(ilabel)), 
            aes(x=x, y=y, label=label), fontface="bold")
if (!(urban==T&rural==T)) {
  p2 = p2 + theme(axis.title.y = element_blank(), 
                  axis.text.y = element_blank())}
p2
dev.off()


### National Figure A.5
if (urban==T&rural==T) {
  ilabel = "g"
  file_name = "./output/f10_national_g.jpeg"
} else if (urban==T&rural==F) {
  ilabel = "h"
  file_name = "./output/f10_urban_h.jpeg"
} else {
  ilabel = "i"
  file_name = "./output/f10_rural_i.jpeg"
}

average_pm_income = data.frame(years=available_years, 
                               low=average_pm_poor,
                               high=average_pm_rich)
average_pm_income = gather(average_pm_income, income, value, low:high, factor_key=TRUE)
average_pm_income$income = as.factor(average_pm_income$income)
average_pm_income$income = factor(average_pm_income$income, levels = c("low", "high"))

png(file_name, units="mm", width=width3, height=height3, res=res)
p3 = ggplot(data=average_pm_income) + 
  geom_line(aes(x=years,y=value,linetype=income), size=base_line3) + 
  scale_y_continuous(limits=c(4,14), breaks=4:14) + 
  ylab(expression(paste('Average PM'['2.5']*' (',mu,'g/m'^"3"*')'))) + 
  xlab("Year") + 
  theme_classic(base_size=base_size2) + 
  scale_linetype_manual(values=income_lines, labels = c("Low income", "High income")) +
  theme(plot.margin=unit(c(2,2,2,2), "mm"),
        legend.position = c(1, 1), 
        legend.justification = c("right", "top"), 
        legend.title = element_blank(), 
        legend.spacing = unit(0,"mm"), 
        legend.text = element_text(size = base_size3),
        legend.key.size = unit(0, "mm"),
        legend.key.width = unit(1.5, "line")) + 
  geom_text(data=data.frame(x = c(2000), y = c(14), label = c(ilabel)), 
            aes(x=x, y=y, label=label), fontface="bold")
if (!(urban==T&rural==T)) {
  p3 = p3 + theme(axis.title.y = element_blank(), 
                  axis.text.y = element_blank()) }
p3
dev.off()




if (urban==T&rural==T) {
  file_name = "./output/f10_national_j.jpeg"
  ilabel = "j"
} else if (urban==T&rural==F) {
  file_name = "./output/f10_urban_k.jpeg"
  ilabel = "k"
} else {
  file_name = "./output/f10_rural_l.jpeg"
  ilabel = "l"
  f4$disparity[f4$year>2008&f4$group=="above12"] = NA
}
yyscale = 120

png(file_name, units="mm", width=width3, height=height3, res=res)
p4 = ggplot(data=f4) +
  geom_bar(aes(x = year, y = disparity*yyscale, fill=group), stat ="identity", position="dodge") +
  geom_line(aes(x = year, y = exposure, colour=group), size=base_line3) +
  theme_classic(base_size=7) + 
  xlab("Year") + 
  scale_fill_manual(name="Population living in pollution (%)", values = pm_colors, labels=pm_labels, 
                    guide = guide_legend(title.position = "top", nrow = 1)) + 
  scale_color_manual(name="Disparities among racial/ethnic groups", values = pm_colors, labels=pm_labels, 
                     guide = guide_legend(title.position = "top", nrow = 1)) +
  scale_y_continuous(name="Population living in pollution (%)", limits = c(0, 100),
                     sec.axis = sec_axis(~./yyscale, name="Disparities among racial/ethnic groups",)) + 
  theme(legend.position = "none", 
        plot.margin=unit(c(2,0,0,0),"mm")) + 
  annotate("text", x=2000,y = 100, label = ilabel, fontface=2)
if (urban==T&rural==T) {
  p4 = p4 + theme(axis.title.y.right = element_blank(), 
                  axis.ticks.y.right = element_blank(),
                  axis.text.y.right = element_blank()) + 
    scale_y_continuous(name="    ", limits = c(0, 100),
                       sec.axis = sec_axis(~./yyscale, name="",))
  p4
} else if (urban==T&rural==F) {
  p4 = p4 + theme(axis.title.y = element_blank(),
                  axis.text.y.left = element_blank(),
                  axis.ticks.y.left = element_blank(), 
                  axis.ticks.y.right = element_blank(), 
                  axis.text.y.right = element_blank())
} else {
  p4 = p4 + theme(axis.title.y = element_blank(),,
                  axis.text.y.left = element_blank(), 
                  axis.ticks.y.left = element_blank())}
p4
dev.off()



# p1_nation = p1
# p2_nation = p2
# p3_nation = p3
# p4_nation = p4
# p1_urban = p1
# p2_urban = p2
# p3_urban = p3
# p4_urban = p4
# rm(p1, p2, p3, p4)
# 
# arrange4 = ggarrange(p1_nation,p1_nation,p1_nation,
#                      p2_nation,p2_nation,p2_nation,
#                      p3_nation,p3_nation,p3_nation,
#                      p4_nation,p4_nation,p4_nation,
#                      ncol = 3, nrow = 4)
# ggsave("FigA10_new.jpeg", arrange4,
#        units="mm", width=width3*2, height=height3*4, dpi = 600, device='png')



### check stats ------------------------------------------------------------------------------------
df = no_na_all_years_pm_data

### Disparities in air pollution exposure among racial/ethnic groups
sum(df$population[df$year==2000&df$pm25>=12]) / sum(df$population[df$year==2000]) * 100 
sum(df$population[df$year==2016&df$pm25>=12]) / sum(df$population[df$year==2016]) * 100  

### Racial/ethnic disparities in relation to EPA and WHO relevant PM2:5 thresholds
sum(df$population[df$year==2000&df$pm25>=8]) / sum(df$population[df$year==2000]) * 100  
sum(df$population[df$year==2016&df$pm25>=8]) / sum(df$population[df$year==2016]) * 100  

### Discussion
(black_average_pm - white_average_pm) / white_average_pm * 100  
(black_average_pm - native_average_pm) / native_average_pm * 100  
(average_pm_poor - average_pm_rich) / average_pm_rich * 100  

