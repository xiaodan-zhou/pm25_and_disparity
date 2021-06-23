### Racial and income disparities in ambient exposure to fine particulate matter in1the United States 

library("tidyverse")
library("ineq")
library("ggplot2")

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

# rerun the whole script if any of these parameters been changed ------------------------
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
urban = TRUE
rural = TRUE


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
income_average_pm = matrix(0, nrow = length(available_years), ncol = n) 

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

# Percentage of total population exposed to the given PM levels and above
pop_above8 = matrix(0, nrow = length(available_years), ncol = n)
pop_above10 = matrix(0, nrow = length(available_years), ncol = n)
pop_above12 = matrix(0, nrow = length(available_years), ncol = n)

# Percentage of income group exposed to the given PM levels
poor_population_between_pm = matrix(0, nrow = length(available_years), ncol = length(thresholds)+1) 
rich_population_between_pm = matrix(0, nrow = length(available_years), ncol = length(thresholds)+1) 
moderate_population_between_pm = matrix(0, nrow = length(available_years), ncol = length(thresholds)+1) 

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

# 
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
  # pop_betw_pm[7]=sum(pm_data$population[pm_data$pm25<19 & pm_data$pm25>16])/sum(pm_data$population)
  # confirm 
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
  income_average_pm[year_counter,]=avg_pm$population
  population_between_pm[year_counter,]=pop_betw_pm
  poor_population_between_pm[year_counter,]=dp_pop_betw_pm
  rich_population_between_pm[year_counter,]=dr_pop_betw_pm
  moderate_population_between_pm[year_counter,]=dm_pop_betw_pm
  average_pm_poor[year_counter]=avg_pm_poor
  average_pm_moderate[year_counter]=avg_pm_moderate
  average_pm_rich[year_counter]=avg_pm_rich
  pop_above8[year_counter,]=above8
  pop_above10[year_counter,]=above10
  pop_above12[year_counter,]=above12
  
  
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
  # black_betw_pm[7]=sum(pm_data$black_pop[pm_data$pm25<19 & pm_data$pm25>16])/sum(pm_data$black_pop)
  # hisp_betw_pm[7]=sum(pm_data$hisp_pop[pm_data$pm25<19 & pm_data$pm25>16])/sum(pm_data$hisp_pop)
  # native_betw_pm[7]=sum(pm_data$native_pop[pm_data$pm25<19 & pm_data$pm25>16])/sum(pm_data$native_pop)
  # asian_betw_pm[7]=sum(pm_data$asian_pop[pm_data$pm25<19 & pm_data$pm25>16])/sum(pm_data$asian_pop)
  # white_betw_pm[7]=sum(pm_data$white_pop[pm_data$pm25<19 & pm_data$pm25>16])/sum(pm_data$white_pop)
  # confirm 
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


### plot parameters
width = 89
height = 66
res = 300
pointsize = 7
pm.colors = c(rgb(76,98,143,255,maxColorValue=255),
              rgb(155,197,126,255,maxColorValue=255), 
              rgb(242,155,110,255,maxColorValue=255))

if (urban==T&rural==T) {
  ### Figure A.1
  png("national_pm_update.jpeg", units="mm", width=width, height=height, res=res, pointsize=pointsize)
  ylab.text = expression(paste('Average PM'['2.5']*' (',mu,'g/m'^"3"*')'))
  plot(available_years,national_average_pm,type="l",xlab="Year",ylab="",lwd=2,ylim = c(7,13))
  mtext(ylab.text,side=2, line =2.5)
  grid()
  dev.off()
  
  
  ### Figure A.4a 
  png("ethnic_pm_update.jpeg", units="mm", width=width, height=height, res=res, pointsize=pointsize)
  ylab.text = expression(paste('Average PM'['2.5']*' (',mu,'g/m'^"3"*')'))
  plot(c(available_years),black_average_pm,ylim=c(6, 15),xlim=c(2000, 2017),type="l",lwd=2,col="red",
       xlab="Year",ylab="")
  mtext(ylab.text, side=2, line =2.5)
  lines(c(available_years),white_average_pm,lwd=2,col="blue")
  lines(c(available_years),hisp_average_pm,lwd=2,col="gold")
  lines(c(available_years),asian_average_pm,lwd=2,col="green")
  lines(c(available_years),native_average_pm,lwd=2,col="magenta")
  grid()
  legend(2012, 15, legend=c("Black","White","Hispanic","Asian","Native American"),
         col=c("red", "blue","gold","green","magenta"), lty=1,lwd=2, cex=.7)
  mtext("a", adj = 0.01, line = -1, cex=8/7, font=2)
  dev.off()
  
  
  ### Figure A.4b
  png("density_pm_update.jpeg", units="mm", width=width, height=height, res=res, pointsize=pointsize)
  ylab.text = expression(paste('Average PM'['2.5']*' (',mu,'g/m'^"3"*')'))
  plot(c(pops),black_zcta_above_pop[17,],type="l",ylim=c(2,10), 
       lwd=2,lty=1,col="red",xlab="Ethnic population in ZCTA (fraction)",ylab="")
  mtext(ylab.text,side=2, line =2.5)
  axis(1, seq(0,1,0.1))
  lines(c(pops),hisp_zcta_above_pop[17,],col="gold",lwd=2,lty=1)
  lines(c(pops),white_zcta_above_pop[17,],col="blue",lwd=2,lty=1)
  lines(c(pops)[1:60],asian_zcta_above_pop[17,1:60],col="green",lwd=2,lty=1)
  lines(c(pops),native_zcta_above_pop[17,],col="magenta",lwd=2,lty=1)
  grid()
  legend(0.7, 6, legend=c("Black","White","Hispanic","Asian","Native American"),
         col=c("red", "blue","gold","green","magenta"), lty=1,lwd=2, cex=0.7)
  mtext("b", adj = 0.01, line = -1, cex=8/7, font=2)
  dev.off()
  
  
  ### Figure A.5
  png("income_pm_update.jpeg", units="mm", width=width, height=height, res=res, pointsize=pointsize)
  ylab.text = expression(paste('Average PM'['2.5']*' (',mu,'g/m'^"3"*')'))
  plot(c(available_years),average_pm_poor,ylim=c(6, 14),xlim=c(2000, 2016),type="l",col="red",xlab="Year",ylab="",lwd=2) 
  mtext(ylab.text,side=2, line =2.5)
  #lines(c(available_years),average_pm_moderate,col="green",lwd=2)
  lines(c(available_years),average_pm_rich,col="blue",lwd=2)
  grid()
  legend(2012, 14, legend=c("Low income","High income"),
         col=c("red","blue"), lwd=2, cex=0.7)  
  dev.off()
  
  
  ### Figure A.6a
  png("HI_Ethnic_update.jpeg", units="mm", width=width, height=height, res=res, pointsize=pointsize)
  ylab.text = expression(paste('Average PM'['2.5']*' (',mu,'g/m'^"3"*')'))
  plot(c(available_years),rich_black_average_pm,ylim=c(7, 15),xlim=c(2000, 2017),type="l",lwd=2,col="red",xlab="Year",ylab="")
  mtext(ylab.text,side=2, line =2.5)
  lines(c(available_years),rich_white_average_pm,lwd=2,col="blue")
  lines(c(available_years),rich_hisp_average_pm,lwd=2,col="gold")
  lines(c(available_years),rich_asian_average_pm,lwd=2,col="green")
  lines(c(available_years),rich_native_average_pm,lwd=2,col="magenta")
  grid()
  legend(2009.5, 15, legend=c("High-income Black","High-income White","High-income Hispanic","High-income Asian","High-income Native American"),
         col=c("red", "blue","gold","green","magenta"), lty=1,lwd=2, cex=0.65)
  mtext("a", adj = 0.01, line = -1, cex=8/7, font=2)
  dev.off()
  
  ### Figure A.6b 
  png("LI_Ethnic_update.jpeg", units="mm", width=width, height=height, res=res, pointsize=pointsize)
  ylab.text = expression(paste('Average PM'['2.5']*' (',mu,'g/m'^"3"*')'))
  plot(c(available_years),poor_black_average_pm,ylim=c(5, 16),xlim=c(2000, 2017),type="l",lwd=2,col="red",xlab="Year",ylab="")
  mtext(ylab.text,side=2, line =2.5)
  lines(c(available_years),poor_white_average_pm,lwd=2,col="blue")
  lines(c(available_years),poor_hisp_average_pm,lwd=2,col="gold")
  lines(c(available_years),poor_asian_average_pm,lwd=2,col="green")
  lines(c(available_years),poor_native_average_pm,lwd=2,col="magenta")
  grid()
  legend(2009.5, 16, legend=c("Low-income Black","Low-income White","Low-income Hispanic","Low-income Asian","Low-income Native American"),
         col=c("red", "blue","gold","green","magenta"), lty=1,lwd=2, cex=0.65)
  mtext("b", adj = 0.01, line = -1, cex=8/7, font=2)
  dev.off()
  
  
  
  ### Figure A.8a Atkinson Index figures
  png("Atkinson_Racial_update.jpeg", units="mm", width=width, height=height, res=res, pointsize=pointsize)
  plot(c(available_years),Atkinson_PM_level_ethnic_8,ylim=c(0.005, 0.20),xlim=c(2000, 2017), lwd=2, type="l", col=pm.colors[1], 
       xlab="Year",ylab="Atkinson Index - Racial / Ethnic")
  lines(c(available_years),Atkinson_PM_level_ethnic_10, lwd=2, col=pm.colors[2]) 
  lines(c(available_years),Atkinson_PM_level_ethnic_12, lwd=2, col=pm.colors[3]) 
  grid()
  legend(2000.5, 0.20, legend=c("Threshold=8","Threshold=10", "Threshold=12"), lwd=2, cex=1, col=pm.colors)
  mtext("a", adj = 0.01, line = -1, cex=8/7, font=2)
  dev.off()
  
  
  ## Figure A.8b Gini Index figures
  png("Gini_Racial_update.jpeg", units="mm", width=width, height=height, res=res, pointsize=pointsize)
  plot(c(available_years),Gini_PM_level_ethnic_8,ylim=c(0.05, 0.35),xlim=c(2000, 2017),type="l", lwd=2, col=pm.colors[1],
       xlab="Year",ylab="Gini Index - Racial / Ethnic")
  lines(c(available_years),Gini_PM_level_ethnic_10, lwd=2, col=pm.colors[2]) 
  lines(c(available_years),Gini_PM_level_ethnic_12, lwd=2, col=pm.colors[3]) 
  grid()
  legend(2000.5, 0.35, legend=c("Threshold= 8","Threshold= 10", "Threshold=12"), lwd=2, cex=1, col=pm.colors)
  mtext("b", adj = 0.01, line = -1, cex=8/7, font=2)
  dev.off()
  
  ### Figure A.9
  eps.list = c(0.25, 0.5, 0.75, 1.0, 2.0)
  eps.color = c("#FFEDA0", "#FEB24C", "#FC4E2A", "#BD0026", "#800026")
  eps.labs = c()
  for (i in 1:length(eps.list)) eps.labs[i] = paste0("eps=", eps.list[i])
  
  ### Figure A.9a
  png("atkinson_race_sensitivity_pm8_update.jpeg", units="mm", width=width, height=height, res=res, pointsize=pointsize)
  pm.threshold = 8
  
  plot(c(available_years),rep(NULL, 17),ylim=c(0, 0.15), xlim=c(2000, 2017) ,type="l", lwd=2,
        xlab="Year",ylab="Atkinson Index - Racial / Ethnic")
  
  Atkinson_PM_level_ethnic_sensitivity = c()
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
      Atkinson_PM_level_ethnic_sensitivity[year_counter]=Atkinson(x_atkinson, parameter = eps.list[ieps])
    } 
    lines(c(available_years),Atkinson_PM_level_ethnic_sensitivity,lwd=2, col=eps.color[ieps])
  }
  grid()
  legend(2000.5, 0.15, legend = eps.labs, col=eps.color, lwd=2, cex=1)
  mtext("a", adj = 0.01, line = -1, cex=8/7, font=2)
  dev.off()
  
  
  ### Figure A.9b
  png("atkinson_race_sensitivity_pm10_update.jpeg", units="mm", width=width, height=height, res=res, pointsize=pointsize)
  pm.threshold = 10
  
  plot(c(available_years),rep(NULL, 17),ylim=c(0, 0.20), xlim=c(2000, 2017) ,type="l", lwd=2,
       xlab="Year",ylab="Atkinson Index - Racial / Ethnic")
  
  Atkinson_PM_level_ethnic_sensitivity = c()
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
      Atkinson_PM_level_ethnic_sensitivity[year_counter]=Atkinson(x_atkinson, parameter = eps.list[ieps])
    } 
    lines(c(available_years),Atkinson_PM_level_ethnic_sensitivity,lwd=2, col=eps.color[ieps])
  }
  grid()
  legend(2000.5, 0.20, legend = eps.labs, col=eps.color, lwd=2, cex=1)
  mtext("b", adj = 0.01, line = -1, cex=8/7, font=2)
  dev.off()
  
  
  ### Figure A.9c
  png("atkinson_race_sensitivity_pm12_update.jpeg", units="mm", width=width, height=height, res=res, pointsize=pointsize)
  pm.threshold = 12
  
  plot(c(available_years),rep(NULL, 17),ylim=c(0, 0.40), xlim=c(2000, 2017) ,type="l", lwd=2,
       xlab="Year",ylab="Atkinson Index - Racial / Ethnic")
  
  Atkinson_PM_level_ethnic_sensitivity = c()
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
      Atkinson_PM_level_ethnic_sensitivity[year_counter]=Atkinson(x_atkinson, parameter = eps.list[ieps])
    } 
    lines(c(available_years),Atkinson_PM_level_ethnic_sensitivity,lwd=2, col=eps.color[ieps])
  }
  grid()
  legend(2000.5, 0.40, legend = eps.labs, col=eps.color, lwd=2, cex=1)
  mtext("c", adj = 0.01, line = -1, cex=8/7, font=2)
  dev.off()
  
}



### Figure A.10 ---------------------------------------------------------------------------------
width_f10 = 60
height_f10 = 45
pointsize_f10 = 10


### National Figure A.1
if (urban==T&rural==T) {
  file_name = "f10_national.jpeg"
  ilabel = "a"
} else if (urban==T&rural==F) {
  file_name = "f10_urban.jpeg"
  ilabel = "b"
} else {
  file_name = "f10_rural.jpeg"
  ilabel = "c"}

png(file_name, units="mm", width=width_f10, height=height_f10*2.6, res=res, pointsize=pointsize_f10)
par(lwd=1, mfrow = c(3, 1), mai = c(.2, .2, 0.1, 0.1))
plot(available_years,national_average_pm,type="l",xlab="Year",ylab="",lwd=1, xlim=c(1999, 2016), ylim = c(6,14))
grid()
mtext(ilabel, adj = 0.01, line = -2, cex=8/7, font=2)



### National Figure A.4a 
if (urban==T&rural==T) {
  ilabel = "d"
} else if (urban==T&rural==F) {
  ilabel = "e"
} else {
  ilabel = "f"}

plot(c(available_years),black_average_pm, xlim=c(1999, 2016),type="l",lwd=1,col="red", ylim=c(4, 15), 
     xlab="Year",ylab="")
lines(c(available_years),white_average_pm,lwd=1,col="blue")
lines(c(available_years),hisp_average_pm,lwd=1,col="gold")
lines(c(available_years),asian_average_pm,lwd=1,col="green")
lines(c(available_years),native_average_pm,lwd=1,col="magenta")
grid()
legend(2010, 15, legend=c("Black","White","Hispanic","Asian","Native American"),
       col=c("red", "blue","gold","green","magenta"), lty=1,lwd=1, cex=.7)
mtext(ilabel, adj = 0.01, line = -2, cex=8/7, font=2)



### National Figure A.5
if (urban==T&rural==T) {
  ilabel = "g"
} else if (urban==T&rural==F) {
  ilabel = "h"
} else {
  ilabel = "i"}

plot(c(available_years),average_pm_poor,ylim=c(5, 14),xlim=c(1999, 2016),type="l",col="red",xlab="Year",ylab="",lwd=1) 
lines(c(available_years),average_pm_rich,col="blue",lwd=1)
grid()
legend(2010, 14, legend=c("Low income","High income"),
       col=c("red","blue"), lwd=2, cex=0.7)  
mtext(ilabel, adj = 0.01, line = -2, cex=8/7, font=2)
# dev.off()


### CoV for race ------------------------------------------------------------------------------------
b8= apply(black_population_between_pm, 1, function(x) 1-(x[1]+x[2]))
w8= apply(white_population_between_pm, 1, function(x) 1-(x[1]+x[2]))
h8= apply(hisp_population_between_pm, 1, function(x) 1-(x[1]+x[2]))
n8= apply(native_population_between_pm, 1, function(x) 1-(x[1]+x[2]))
a8= apply(asian_population_between_pm, 1, function(x) 1-(x[1]+x[2]))
T8=apply(population_between_pm, 1, function(x) 1-(x[1]+x[2]))
eaverage8=(b8+w8+h8+n8+a8)/5
eineq8=((abs(b8-eaverage8)^2+abs(w8-eaverage8)^2+abs(h8-eaverage8)^2+abs(a8-eaverage8)^2+abs(n8-eaverage8)^2)/5)/eaverage8^2

b10= apply(black_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]))
w10= apply(white_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]))
h10= apply(hisp_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]))
n10= apply(native_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]))
a10= apply(asian_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]))
T10=apply(population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]))
eaverage10=(b10+w10+h10+n10+a10)/5
eineq10=((abs(b10-eaverage10)^2+abs(w10-eaverage10)^2+abs(h10-eaverage10)^2+abs(a10-eaverage10)^2+abs(n10-eaverage10)^2)/5)/eaverage10^2

b12= apply(black_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]+x[4]))
w12= apply(white_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]+x[4]))
h12= apply(hisp_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]+x[4]))
n12= apply(native_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]+x[4]))
a12= apply(asian_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]+x[4]))
T12=apply(population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]+x[4]))
eaverage12=(b12+w12+h12+n12+a12)/5
eineq12=((abs(b12-eaverage12)^2+abs(w12-eaverage12)^2+abs(h12-eaverage12)^2+abs(a12-eaverage12)^2+abs(n12-eaverage12)^2)/5)/eaverage12^2


### National Figure 4 
f4 = data.frame(year = c(available_years, available_years, available_years),
           exposure = c(rowSums(population_between_pm[,3:7]), 
                        rowSums(population_between_pm[,4:7]), 
                        rowSums(population_between_pm[,5:7])) * 100, 
           disparity = c(eineq8, eineq10, eineq12),
           group = c(rep("above8", 17), rep("above10", 17), rep("above12", 17)))
f4$group = as.factor(f4$group)
f4$group = factor(f4$group, levels = c("above8", "above10", "above12"))

if (urban==T&rural==T) {
  file_name = "f10_national_j.jpeg"
  ilabel = "j"
} else if (urban==T&rural==F) {
  file_name = "f10_urban_k.jpeg"
  ilabel = "k"
} else {
  file_name = "f10_rural_l.jpeg"
  ilabel = "l"}

png(file_name, units="mm", width=width_f10, height=height_f10+10, res=res, pointsize=6)
ggplot(data=f4) +
  geom_line(aes(x = year, y = exposure, colour=group), size=.5) + 
  geom_bar(aes(x = year, y = disparity*200, fill=group), stat ="identity", position="dodge") +
  geom_line(aes(x = year, y = exposure, colour=group), size=.5) + theme_bw() + 
  scale_fill_manual(name="Disparities among racial/ethnic groups", values = pm.colors, labels=c("Threshold=8", "Threshold=10", "Threshold=12"), 
                    guide = guide_legend(title.position = "top", nrow = 1)) + 
  scale_color_manual(name="Population Living in Pollution (%)", values = pm.colors, labels=c("Threshold=8", "Threshold=10", "Threshold=12"), 
                     guide = guide_legend(title.position = "top", nrow = 1)) +
  scale_y_continuous(name="Population Living in Pollution (%)", limits = c(0, 100),
                     sec.axis = sec_axis(~./200, name="Disparities among racial/ethnic groups",)) + 
  theme(axis.title = element_blank(), plot.margin=unit(c(2,0,0,0),"mm"), 
        legend.position = "bottom", legend.box="vertical",
        legend.spacing = unit(0, "lines"), 
        legend.text=element_text(size=6),
        legend.title=element_text(size=6), 
        axis.text.x=element_text(size=6), 
        axis.text.y=element_text(size=6), 
        legend.margin=margin(0,10,0,0),
        legend.box.margin=margin(-2, 0, 0, 0)) + 
  annotate("text", x=1999,y = 95, label = ilabel, fontface =2)
dev.off()



### Figure 4
if (urban==T&rural==T) {
  png("inequality_ethnic_cov_updated.jpeg", units="mm", width=120, height=height, res=res, pointsize=7)
  ggplot(data=f4) +
    geom_line(aes(x = year, y = exposure, colour=group), size=.5) + 
    geom_bar(aes(x = year, y = disparity*200, fill=group), stat ="identity", position="dodge") +
    geom_line(aes(x = year, y = exposure, colour=group), size=.5) + theme_bw() + 
    scale_fill_manual(name="Disparities among racial/ethnic groups", values = pm.colors, labels=c("Threshold=8", "Threshold=10", "Threshold=12"), 
                      guide = guide_legend(title.position = "top", nrow = 1)) + 
    scale_color_manual(name="Population Living in Pollution (%)", values = pm.colors, labels=c("Threshold=8", "Threshold=10", "Threshold=12"), 
                       guide = guide_legend(title.position = "top", nrow = 1)) +
    scale_y_continuous(name="Population Living in Pollution (%)", limits = c(0, 100),
                       sec.axis = sec_axis(~./200, name="Disparities among racial/ethnic groups",)) + 
    theme(plot.margin=unit(c(2,0,0,0),"mm"), # axis.title = element_blank(), 
          legend.position = "bottom", legend.box="horizontal",
          legend.spacing = unit(0, "lines"), 
          legend.text=element_text(size=5),
          legend.title=element_text(size=5), 
          axis.text.x=element_text(size=7), 
          axis.text.y=element_text(size=7), 
          axis.title=element_text(size=7),
          legend.margin=margin(0,2,0,0),
          legend.box.margin=margin(-5,-5,0,0))
  dev.off()
}





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




################# Inequality in Income
# r8= apply(rich_population_between_pm, 1, function(x) 1-(x[1]+x[2]))
# m8= apply(moderate_population_between_pm, 1, function(x) 1-(x[1]+x[2]))
# p8= apply(poor_population_between_pm, 1, function(x) 1-(x[1]+x[2]))
# T8=apply(population_between_pm, 1, function(x) 1-(x[1]+x[2]))
# iaverage8=(r8+m8+p8)/3
# iineq8=(abs(r8-iaverage8)^2+abs(m8-iaverage8)^2+abs(p8-iaverage8)^2)/iaverage8^2
# 
# r10= apply(rich_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]))
# m10= apply(moderate_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]))
# p10= apply(poor_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]))
# T10=apply(population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]))
# iaverage10=(r10+m10+p10)/3
# iineq10=(abs(r10-iaverage10)^2+abs(m10-iaverage10)^2+abs(p10-iaverage10)^2)/iaverage10^2
# 
# 
# r12= apply(rich_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]+x[4]))
# m12= apply(moderate_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]+x[4]))
# p12= apply(poor_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]+x[4]))
# T12=apply(population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]+x[4]))
# iaverage12=(r12+m12+p12)/3
# iineq12=(abs(r12-iaverage12)^2+abs(m12-iaverage12)^2+abs(p12-iaverage12)^2)/iaverage12^2
# 
# plot(T8,iineq8,type="l",xlim=c(0,0.95),ylim=c(0,1.2))
# lines(T10,iineq10,col="red")
# lines(T10,eineq10,col="red",lty=2)
# lines(T8,eineq8,col="black",lty=2)

################# Inequality in Income alternative
# r8= apply(rich_population_between_pm, 1, function(x) 1-(x[1]+x[2]))
# m8= apply(moderate_population_between_pm, 1, function(x) 1-(x[1]+x[2]))
# p8= apply(poor_population_between_pm, 1, function(x) 1-(x[1]+x[2]))
# T8=apply(population_between_pm, 1, function(x) 1-(x[1]+x[2]))
# iaverage8=(r8+p8)/2
# iineq8=(abs(r8-iaverage8)^2+abs(p8-iaverage8)^2)/iaverage8^2
# 
# r10= apply(rich_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]))
# m10= apply(moderate_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]))
# p10= apply(poor_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]))
# T10=apply(population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]))
# iaverage10=(r10+p10)/2
# iineq10=(abs(r10-iaverage10)^2+abs(p10-iaverage10)^2)/iaverage10^2
# 
# 
# r12= apply(rich_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]+x[4]))
# m12= apply(moderate_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]+x[4]))
# p12= apply(poor_population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]+x[4]))
# T12=apply(population_between_pm, 1, function(x) 1-(x[1]+x[2]+x[3]+x[4]))
# iaverage12=(r12+p12)/2
# iineq12=(abs(r12-iaverage12)^2+abs(p12-iaverage12)^2)/iaverage12^2
# 
# plot(T8,iineq8,type="l",xlim=c(0,0.95),ylim=c(0,1.5))
# lines(T10,iineq10,col="red")
# lines(T12,iineq12,col="blue")


##### Inequality new way (removed from revised version)

#T8=apply(population_between_pm, 1, function(x) 1-(x[1]+x[2]))
# iaverage8=rowMeans(pop_above8)
# iineq8=(rowSums(abs(pop_above8-iaverage8)^2)/10)/iaverage8^2
# 
# iaverage10=rowMeans(pop_above10)
# iineq10=(rowSums(abs(pop_above10-iaverage10)^2)/10)/iaverage10^2
# 
# iaverage12=rowMeans(pop_above12)
# iineq12=rowSums(abs(pop_above12-iaverage12)^2)/iaverage12^2

# make dataframe
#df_urban=data.frame(eineq8,T8,eineq10,T10,iineq8,iineq10)
#df_rural=data.frame(eineq8,T8,eineq10,T10,iineq8,iineq10)
#df_new=data.frame(eineq8,T8,eineq10,T10,eineq12,T12)

#library("writexl")
#write_xlsx(df_new,"/Users/abj127/OneDrive - Harvard University/Harvard_Research/Air_Pollution/Paper_draft/Revisions/df_with_12_rural.xlsx")
#

# # to verify number of classifications with zctas
# # > completeFun <- function(data, desiredCols) {
# #   +     completeVec <- complete.cases(data[, desiredCols])
# #   +     return(data[completeVec, ])
# #   + }
# 
# for (i in c(2000:2016))
# {
# final_year=final_pm_data_new[which(final_pm_data_new$year==i),]
# d=completeFun(final_year, "zcta")[,c(1,2,6,21,15)]
# print(unique(d[,2]))
# print(length(unique(d$zcta)))
# print(colSums(!is.na(d))[4])
# }








################# More Atkinson Index Income Sensitivity Check, eps ####################
# eps.list = c(0.25, 0.5, 0.75, 1.0, 2.0)
# eps.color = c("#FFEDA0", "#FEB24C", "#FC4E2A", "#BD0026", "#800026")
# 
# pm.threshold = 10
# year_counter = 1
# eps.labs = c()
# for (i in 1:length(eps.list)) eps.labs[i] = paste0("eps=", eps.list[i])
# 
# if (pm.threshold == 8) {
#   ylim=c(0, 0.018)
# } else { ylim=c(0, 0.05)}
# plot(c(available_years),rep(NULL, 17),ylim=ylim,xlim=c(2000, 2017),type="l",
#      lwd=2,xlab="Year",ylab="Atkinson Index - Income", col=eps.color[1])
# 
# Atkinson_PM_level_income = c() ## TODO check whether this ruin others 
# for (ieps in 1:length(eps.list)) {
#   for (y in available_years){ 
#     year_counter= y - min(available_years) + 1
#     pm_data = no_na_all_years_pm_data[no_na_all_years_pm_data$year==y,]
#     
#     # categorizing ZCTAs into income groups (1, 2, ..., 10) using percentiles
#     value = pm_data$medhouseholdincome
#     qtile = seq(1/n,1-1/n,1/n)
#     Qlabel = c(1:n)
#     g = with(pm_data, factor(findInterval(value, c(-Inf, quantile(value, probs=c(qtile)), Inf)), labels = Qlabel)) 
#     pm_data$group=as.integer(g) 
#     
#     x_atkinson = atkinson.income(df=pm_data, pm.threshold=pm.threshold)
#     Atkinson_PM_level_income[year_counter]=Atkinson(x_atkinson, parameter = eps.list[ieps])
#   } 
#   lines(c(available_years),Atkinson_PM_level_income,lwd=2,col=eps.color[ieps]) # = ieps)
# }
# grid()
# legend("topleft", legend = eps.labs, lwd=2, cex=1, col=eps.color)
################# End Atkinson Index Income Sensitivity Check, eps ####################




################# More Atkinson Index Ethinic Sensitivity Check, Urban&Rural ####################
# urban = FALSE
# rural = TRUE
# pm.thresholds = c(8,9,10,11,12,13)
# pm.labs = c()
# pm.lines = pm.thresholds - min(pm.thresholds) + 1
# for (i in 1:length(pm.thresholds)) pm.labs[i] = paste0("pm", pm.thresholds[i])
# 
# plot(c(available_years),rep(NULL, 17),xlim=c(2000, 2017),ylim=c(0, 0.2),type="l",
#      lwd=2,col="black",xlab="Year",ylab="Atkinson Index - Racial / Ethnic")
# if (urban&!rural)
#   plot(c(available_years),rep(NULL, 17),xlim=c(2000, 2017),ylim=c(0, 0.2),type="l",
#        lwd=2,col="black",xlab="Year",ylab="Atkinson Index - Racial / Ethnic", main="urban")
# if (!urban&rural)
#   plot(c(available_years),rep(NULL, 17),xlim=c(2000, 2017),ylim=c(0, 0.2),type="l",
#        lwd=2,col="black",xlab="Year",ylab="Atkinson Index - Racial / Ethnic", main="urban")
# for (ipm in 1:length(pm.thresholds)) {
#   for (y in available_years){
# 
#     year_counter= y - min(available_years) + 1
#     pm_data = no_na_all_years_pm_data[no_na_all_years_pm_data$year==y,]
#     stopifnot(sum(duplicated(pm_data$zcta)) == 0)
#     if (urban&!rural) pm_data=pm_data[which(pm_data$urban==1),]
#     if (!urban&rural) pm_data=pm_data[which(pm_data$urban==0),]
# 
#     x_atkinson = atkinson.ethnic(df=pm_data, pm.threshold=pm.thresholds[ipm])
#     Atkinson_PM_level_ethnic[year_counter]=Atkinson(x_atkinson, parameter = eps)
#   }
#   lines(c(available_years),Atkinson_PM_level_ethnic,lwd=2,lty = pm.lines[ipm])
# }
# grid()
# legend("topleft", legend = pm.labs,
#        lty=pm.thresholds-min(pm.thresholds)+1,lwd=2, cex=1)

################# End Atkinson Index Ethinic Sensitivity Check, Urban&Rural ####################


# #Figure 1b
# plot(c(thresholds,19),population_between_pm[1,],type="l",col="black",ylim=c(0,0.5),xlab="PM Threshold",ylab="Fraction of population",main="Fraction of population between thresholds",cex.axis=1.5,cex.main=1.5,cex.lab=1.5)
# lines(c(thresholds,19),population_between_pm[5,],col="red")
# lines(c(thresholds,19),population_between_pm[9,],col="blue")
# lines(c(thresholds,19),population_between_pm[13,],col="brown")
# lines(c(thresholds,19),population_between_pm[17,],col="gray")
# grid(length(thresholds),length(thresholds))
# legend(16, 0.5, legend=c("2000", "2004","2008","2012","2016"),
#        col=c("black", "red","blue","brown","gray"), lty=1, cex=0.8)
# #Figure 2a
# plot(c(available_years),black_average_pm,ylim=c(6, 15),xlim=c(2000, 2017),type="l",col="black",xlab="year",ylab="average PM2.5",main="PM level with race")
# lines(c(available_years),white_average_pm,col="green")
# lines(c(available_years),hisp_average_pm,col="red")
# lines(c(available_years),asian_average_pm,col="brown")
# lines(c(available_years),native_average_pm,col="blue")
# grid(4,length(avg_pm$group_number))
# legend(2012, 15, legend=c("black","white","hispanic","asian","native"),
#        col=c("black", "green", "red","brown","blue"), lty=1, cex=0.8)
#Figure 3
# plot(c(thresholds,19),black_population_between_pm[1,],type="l",lty=1,col="black",ylim=c(0,0.55),xlab="threshold",ylab="% between adjacent threshold",main="Black vs. White")
# lines(c(thresholds,19),black_population_between_pm[17,],col="black",lty=2)
# lines(c(thresholds,19),white_population_between_pm[1,],col="red",lty=1)
# lines(c(thresholds,19),white_population_between_pm[17,],col="red",lty=2)
# grid(length(thresholds),length(thresholds))
# legend(16, 0.55, legend=c("black-2000", "black-2016","white-2000","white-2016"),
#        col=c("black", "black","red","red"), lty=c(1,2,1,2), cex=0.6)
# Figure 3a (here r=8, p=3. In the report figure, r=10, p=1)
# plot(c(thresholds,19),poor_population_between_pm[1,],type="l",col="black",lty=1, ylim=c(0,0.62),xlab="threshold",ylab="% between adjacent threshold",main="Poor vs. Rich")
# lines(c(thresholds,19),poor_population_between_pm[17,],col="black",lty=2)
# lines(c(thresholds,19),rich_population_between_pm[1,],col="red",lty=1)
# lines(c(thresholds,19),rich_population_between_pm[17,],col="red",lty=2)
# lines(c(thresholds,19),moderate_population_between_pm[1,],col="blue",lty=1)
# lines(c(thresholds,19),moderate_population_between_pm[17,],col="blue",lty=2)
# grid(length(thresholds),length(thresholds))
# legend(14, 0.62, legend=c("poor-2000", "poor-2016","rich-2000","rich-2016"),
#        col=c("black", "black","red","red"), lty=c(1,2,1,2), cex=0.8)
