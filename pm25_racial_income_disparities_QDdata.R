### Disparities in air pollution exposure across population and income groups
### last update: Aug 24, 2021
### ATTENTION: DATA NOT PUBLICLY AVAILABLE FOR THIS SCRIPT

library("tidyverse")
library("ineq")
library(ggpubr)
library(gridExtra)


# SECTION 1 - READ AND CLEAN DATA---------------------------------------------------------
final_pm_data<-read.csv("./data/Canadian_PM_revision.csv", header=TRUE)

# all_years_data: select and rename columns from final_pm_data
colnames(final_pm_data)[colnames(final_pm_data)=="qd_pm25"] <- "pm25" 
all_years_data<-subset(final_pm_data,select=-c(tmmx,rmax,pr,medianhousevalue,pct_owner_occ))
colnames(all_years_data)[colnames(all_years_data)=="hispanic"] <- "pct_hisp"

# calculate ethnic group population per zipcode
all_years_data$black_pop = all_years_data$pct_blk * all_years_data$population 
all_years_data$hisp_pop = all_years_data$pct_hisp * all_years_data$population 
all_years_data$native_pop = all_years_data$pct_native * all_years_data$population 
all_years_data$asian_pop = all_years_data$pct_asian * all_years_data$population
all_years_data$white_pop = all_years_data$pct_white * all_years_data$population

# reorder columns
col_order=c("year","ZIP","zcta","popdensity","population","poverty","education",
            "pct_blk","pct_hisp","pct_native","pct_asian","pct_white","black_pop","hisp_pop",
            "native_pop","asian_pop","white_pop","medhouseholdincome","pm25","urban")
all_years_pm_data = all_years_data[, col_order] 

# no_na_all_years_pm_data: delete rows with missing data in all_years_pm_data
no_na_all_years_pm_data = na.omit(all_years_pm_data)


# all available years in dataset
available_years = 2000:2016

n=10
p=3
r=8
# different PM levels the data will be categorized into
thresholds=c(seq(6,16,2)) 

# analysis on urban area only
urban = TRUE
rural = TRUE


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
race_average_pm = data.frame(available_years = available_years,
                             black = 0, hisp = 0, native = 0, asian = 0, white = 0)

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

# Percentage of ethnic population between different PM levels
black_population_between_pm=matrix(0, nrow = length(available_years), ncol = length(thresholds)+1) 
hisp_population_between_pm=matrix(0, nrow = length(available_years), ncol = length(thresholds)+1)  
native_population_between_pm=matrix(0, nrow = length(available_years), ncol = length(thresholds)+1)  
asian_population_between_pm=matrix(0, nrow = length(available_years), ncol = length(thresholds)+1)  
white_population_between_pm=matrix(0, nrow = length(available_years), ncol = length(thresholds)+1) 


# Percentage of an ethnic group out of the ethnic population between different PM levels
# for example, the percent of white population out of the total USA white population exposed to PM level 8-10ug/m3
pops=c(seq(0,95,1))/100
black_zcta_above_pop=matrix(0, nrow = length(available_years), ncol = length(pops))
hisp_zcta_above_pop=matrix(0, nrow = length(available_years), ncol = length(pops))  
native_zcta_above_pop=matrix(0, nrow = length(available_years), ncol = length(pops))  
asian_zcta_above_pop=matrix(0, nrow = length(available_years), ncol = length(pops))  
white_zcta_above_pop=matrix(0, nrow = length(available_years), ncol = length(pops)) 

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
  
  pop_betw_pm=numeric(length(thresholds)+1) # ALL POP
  
  for (j in c(1:length(thresholds))){
    if(j==1){
      pop_betw_pm[j]=sum(pm_data$population[pm_data$pm25<thresholds[j]])/sum(pm_data$population)
      
    }else{
      pop_betw_pm[j]=sum(pm_data$population[pm_data$pm25<thresholds[j] & pm_data$pm25>thresholds[j-1]])/sum(pm_data$population)
    }
  }
  # pop_betw_pm[7]=sum(pm_data$population[pm_data$pm25<19 & pm_data$pm25>16])/sum(pm_data$population)
  pop_betw_pm[7]=sum(pm_data$population[pm_data$pm25>16])/sum(pm_data$population)
  
  
  # STORE VALUES IN DESIRED OUTPUTS:
  national_average_pm[year_counter]=sum(pm_data$pm25*pm_data$population)/sum(pm_data$population)
  income_average_pm[year_counter,]=avg_pm$population
  population_between_pm[year_counter,]=pop_betw_pm
  average_pm_poor[year_counter]=avg_pm_poor
  average_pm_moderate[year_counter]=avg_pm_moderate
  average_pm_rich[year_counter]=avg_pm_rich
  
  
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
  # 
  black_betw_pm[7]=sum(pm_data$black_pop[  pm_data$pm25>16])/sum(pm_data$black_pop)
  hisp_betw_pm[7]=sum(pm_data$hisp_pop[ pm_data$pm25>16])/sum(pm_data$hisp_pop)
  native_betw_pm[7]=sum(pm_data$native_pop[  pm_data$pm25>16])/sum(pm_data$native_pop)
  asian_betw_pm[7]=sum(pm_data$asian_pop[  pm_data$pm25>16])/sum(pm_data$asian_pop)
  white_betw_pm[7]=sum(pm_data$white_pop[ pm_data$pm25>16])/sum(pm_data$white_pop)
  

    # STORE VALUES IN DESIRED OUTPUTS:
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
  
}


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

### Figure A.11a

data_temp = gather(race_average_pm, race, value, black:white, factor_key=TRUE)
data_temp$race = as.factor(data_temp$race)
data_temp$race = factor(data_temp$race, levels = c("black", "white", "hisp", "asian", "native"))

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
        legend.text = element_text(size = base_size3-1), 
        legend.key.size = unit(0, "mm"),
        legend.key.width = unit(1.5, "line")) + 
  scale_color_discrete(labels = race_lables) + 
  geom_text(data=data.frame(x = c(2000-0.5), y = c(15), label = c("a")), 
            aes(x=x, y=y, label=label), fontface="bold")
a4a


average_pm_income = data.frame(years=available_years, 
                               low=average_pm_poor,
                               high=average_pm_rich)
average_pm_income = gather(average_pm_income, income, value, low:high, factor_key=TRUE)
average_pm_income$income = as.factor(average_pm_income$income)
average_pm_income$income = factor(average_pm_income$income, levels = c("low", "high"))

# png("income_pm_update2.jpeg", units="mm", width=width3, height=height3, res=res)
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
  geom_text(data=data.frame(x = c(2000), y = c(15), label = c("b")), 
            aes(x=x, y=y, label=label), fontface="bold")
a5 # dev.off()




### CoV for race ------------------------------------------------------------------------------------
b8 = 1 - rowSums(black_population_between_pm[,c(1,2)])
w8 = 1 - rowSums(white_population_between_pm[,c(1,2)])
h8 = 1 - rowSums(hisp_population_between_pm[,c(1,2)])
n8 = 1 - rowSums(native_population_between_pm[,c(1,2)])
a8 = 1 - rowSums(asian_population_between_pm[,c(1,2)])
bwhna8 = matrix(c(b8, w8, h8, n8, a8), ncol = 5)
eineq8 = apply(bwhna8, 1, sd) / apply(bwhna8, 1, mean)

b10 = 1 - rowSums(black_population_between_pm[,c(1,2,3)])
w10 = 1 - rowSums(white_population_between_pm[,c(1,2,3)])
h10 = 1 - rowSums(hisp_population_between_pm[,c(1,2,3)])
n10 = 1 - rowSums(native_population_between_pm[,c(1,2,3)])
a10 = 1 - rowSums(asian_population_between_pm[,c(1,2,3)])
bwhna10 = matrix(c(b10, w10, h10, n10, a10), ncol = 5)
eineq10 = apply(bwhna10, 1, sd) / apply(bwhna10, 1, mean)

b12 = 1 - rowSums(black_population_between_pm[,c(1,2,3,4)])
w12 = 1 - rowSums(white_population_between_pm[,c(1,2,3,4)])
h12 = 1 - rowSums(hisp_population_between_pm[,c(1,2,3,4)])
n12 = 1 - rowSums(native_population_between_pm[,c(1,2,3,4)])
a12 = 1 - rowSums(asian_population_between_pm[,c(1,2,3,4)])
bwhna12 = matrix(c(b12, w12, h12, n12, a12), ncol = 5)
eineq12 = apply(bwhna12, 1, sd) / apply(bwhna12, 1, mean)

### National Figure 4 
f4 = data.frame(year = c(available_years, available_years, available_years),
                exposure = c(rowSums(population_between_pm[,3:7]), 
                             rowSums(population_between_pm[,4:7]), 
                             rowSums(population_between_pm[,5:7])) * 100, 
                disparity = c(eineq8, eineq10, eineq12),
                group = c(rep("above8", 17), rep("above10", 17), rep("above12", 17)))
f4$group = as.factor(f4$group)
f4$group = factor(f4$group, levels = c("above8", "above10", "above12"))


yyscale = 100/max(f4$disparity)
a4 = ggplot(data=f4) +
  geom_bar(aes(x = year, y = disparity*yyscale, fill=group), stat ="identity", position="dodge") +
  geom_line(aes(x = year, y = exposure, colour=group), size=base_line3) +
  theme_classic(base_size=base_size2) + 
  scale_color_manual(name="Population living in pollution (%)", values = pm_colors, labels=pm_labels, 
                     guide = guide_legend(title.position = "top", nrow = 1)) +
  scale_y_continuous(name="Population living in pollution (%)", limits = c(0, 100),#  breaks=seq(10, 100, 10), 
                     sec.axis = sec_axis(~./yyscale, name="Disparities among racial/ethnic groups")) + 
  scale_fill_manual(name="Disparities among racial/ethnic groups", values = pm_colors, labels=pm_labels, 
                    guide = guide_legend(title.position = "top", nrow = 1)) + 
  guides(line = guide_legend(order = 0, title.position="top"), 
         color = guide_legend(order = 1, title.position="top")) +
  theme(plot.margin=unit(c(2,0,0,0),"mm"), 
        legend.spacing = unit(0.8, "line"),
        legend.margin=margin(0,1,0,0),
        # legend.position = "bottom", legend.box="horizontal", legend.title=element_blank(), legend.text = element_text(size = 9),
        legend.position = "bottom", legend.box="horizontal", # legend.title=element_text(size = 9), legend.text = element_text(size = 9),
        # legend.position = "bottom", legend.box="horizontal", legend.title=element_text(size = 9), legend.text = element_text(size = 9),
        legend.box.margin=margin(-5,-5,0,0),
        legend.key.width = unit(1., "line"))  + 
  geom_text(data=data.frame(x = c(2000-0.5), y = c(100), label = c("c")), 
            aes(x=x, y=y, label=label), fontface="bold")

a4

gl = list()
gl[[1]] = a4a
gl[[2]] = a5
gl[[3]] = a4
arrange3 = grid.arrange(grobs = gl,
  layout_matrix = rbind(c(1, 2),
                        c(1, 2),
                        c(3, 3),
                        c(3, 3), 
                        c(3, 3)))
arrange3
ggsave("./output/benchmark_QD.jpeg", arrange3, 
       units="mm", width=width3*2, height=height3*2, dpi = 600, device='png')

