library(dplyr)
library(tidyverse)
library(tigris)

library(shiny)
library(sf)
library(socviz)

#Cleaning the data
ACS_2019_state_cleaned <- ACS_2019_state_new %>% select(Qualifying.Name, State..FIPS.Code., Total.Population, Housing.Units, Civilian.Population.in.Labor.Force.16.Years.and.Over., Civilian.Population.in.Labor.Force.16.Years.and.Over..Employed, 
    Civilian.Population.in.Labor.Force.16.Years.and.Over..Unemployed, Median.Household.Income..In.2019.Inflation.Adjusted.Dollars., Per.Capita.Income..In.2019.Inflation.Adjusted.Dollars., 
    Median.Value, Median.Gross.Rent, Median.Gross.Rent.as.a.Percentage.of.Household..Income.in.the.Past.12.Months..Dollars., 
    Average.Commute.to.Work..In.Min.) %>% mutate(year = 2019) %>% rename(name = Qualifying.Name, FIPS = State..FIPS.Code., pop = Total.Population, housing_units = Housing.Units, 
    labor_force = Civilian.Population.in.Labor.Force.16.Years.and.Over., employed = Civilian.Population.in.Labor.Force.16.Years.and.Over..Employed,
    unemployed = Civilian.Population.in.Labor.Force.16.Years.and.Over..Unemployed, median_house_income = Median.Household.Income..In.2019.Inflation.Adjusted.Dollars.,
    per_capita_income = Per.Capita.Income..In.2019.Inflation.Adjusted.Dollars., median_house_value = Median.Value, median_gross_rent = Median.Gross.Rent,
    median_gross_rent_pct = Median.Gross.Rent.as.a.Percentage.of.Household..Income.in.the.Past.12.Months..Dollars., avg_commute = Average.Commute.to.Work..In.Min.)

ACS_2020_state_cleaned <- ACS_2020_state_new %>% select(Qualifying.Name, State..FIPS.Code., 
      Total.Population, Housing.Units, Civilian.Population.in.Labor.Force.16.Years.and.Over., 
      Civilian.Population.in.Labor.Force.16.Years.and.Over..Employed, Civilian.Population.in.Labor.Force.16.Years.and.Over..Unemployed, 
      Median.Household.Income..In.2020.Inflation.Adjusted.Dollars., Per.Capita.Income..In.2020.Inflation.Adjusted.Dollars., 
      Median.Value, Median.Gross.Rent, Median.Gross.Rent.as.a.Percentage.of.Household..Income.in.the.Past.12.Months..Dollars., 
      Average.Commute.to.Work..In.Min.) %>% mutate(year = 2020) %>% rename(name = Qualifying.Name, FIPS = State..FIPS.Code., pop = Total.Population, housing_units = Housing.Units, 
      labor_force = Civilian.Population.in.Labor.Force.16.Years.and.Over., employed = Civilian.Population.in.Labor.Force.16.Years.and.Over..Employed,
      unemployed = Civilian.Population.in.Labor.Force.16.Years.and.Over..Unemployed, median_house_income = Median.Household.Income..In.2020.Inflation.Adjusted.Dollars.,
      per_capita_income = Per.Capita.Income..In.2020.Inflation.Adjusted.Dollars., median_house_value = Median.Value, median_gross_rent = Median.Gross.Rent,
      median_gross_rent_pct = Median.Gross.Rent.as.a.Percentage.of.Household..Income.in.the.Past.12.Months..Dollars., avg_commute = Average.Commute.to.Work..In.Min.
        )


ACS_2021_state_cleaned <- ACS_2021_state_new %>% select(Qualified.Area.Name, State..FIPS.Code., 
      Total.Population, Housing.Units, Civilian.Population.in.Labor.Force.16.Years.and.Over., 
      Civilian.Population.in.Labor.Force.16.Years.and.Over..Employed, Civilian.Population.in.Labor.Force.16.Years.and.Over..Unemployed, 
      Median.Household.Income..In.2021.Inflation.Adjusted.Dollars., Per.Capita.Income..In.2021.Inflation.Adjusted.Dollars., 
      Median.Value, Median.Gross.Rent, Median.Gross.Rent.as.a.Percentage.of.Household..Income.in.the.Past.12.Months..Dollars., 
      Average.Commute.to.Work..In.Min.) %>% mutate(year = 2021) %>% rename(name = Qualified.Area.Name, FIPS = State..FIPS.Code., pop = Total.Population, housing_units = Housing.Units, 
      labor_force = Civilian.Population.in.Labor.Force.16.Years.and.Over., employed = Civilian.Population.in.Labor.Force.16.Years.and.Over..Employed,
      unemployed = Civilian.Population.in.Labor.Force.16.Years.and.Over..Unemployed, median_house_income = Median.Household.Income..In.2021.Inflation.Adjusted.Dollars.,
      per_capita_income = Per.Capita.Income..In.2021.Inflation.Adjusted.Dollars., median_house_value = Median.Value, median_gross_rent = Median.Gross.Rent,
      median_gross_rent_pct = Median.Gross.Rent.as.a.Percentage.of.Household..Income.in.the.Past.12.Months..Dollars., avg_commute = Average.Commute.to.Work..In.Min.
      )


ACS_2022_state_cleaned <- ACS_2022_state_new %>% select(Qualified.Area.Name, State..FIPS.Code., 
      Total.Population, Housing.Units, Civilian.Population.in.Labor.Force.16.Years.and.Over., 
      Civilian.Population.in.Labor.Force.16.Years.and.Over..Employed, Civilian.Population.in.Labor.Force.16.Years.and.Over..Unemployed, 
      Median.Household.Income..In.2022.Inflation.Adjusted.Dollars., Per.Capita.Income..In.2022.Inflation.Adjusted.Dollars., 
      Median.Value, Median.Gross.Rent, Median.Gross.Rent.as.a.Percentage.of.Household..Income.in.the.Past.12.Months..Dollars., 
      Average.Commute.to.Work..In.Min.) %>% mutate(year = 2022) %>% rename(name = Qualified.Area.Name, FIPS = State..FIPS.Code., pop = Total.Population, housing_units = Housing.Units, 
      labor_force = Civilian.Population.in.Labor.Force.16.Years.and.Over., employed = Civilian.Population.in.Labor.Force.16.Years.and.Over..Employed,
      unemployed = Civilian.Population.in.Labor.Force.16.Years.and.Over..Unemployed, median_house_income = Median.Household.Income..In.2022.Inflation.Adjusted.Dollars.,
      per_capita_income = Per.Capita.Income..In.2022.Inflation.Adjusted.Dollars., median_house_value = Median.Value, median_gross_rent = Median.Gross.Rent,
      median_gross_rent_pct = Median.Gross.Rent.as.a.Percentage.of.Household..Income.in.the.Past.12.Months..Dollars., avg_commute = Average.Commute.to.Work..In.Min.
      )



ACS_2023_state_cleaned <- ACS_2023_state_new %>% select(Qualified.Area.Name, State..FIPS.Code., 
      Total.Population, Housing.Units, Civilian.Population.in.Labor.Force.16.Years.and.Over., Civilian.Population.in.Labor.Force.16.Years.and.Over..Employed, 
      Civilian.Population.in.Labor.Force.16.Years.and.Over..Unemployed, Median.Household.Income..In.2023.Inflation.Adjusted.Dollars., 
      Per.Capita.Income..In.2023.Inflation.Adjusted.Dollars., Median.Value, Median.Gross.Rent, Median.Gross.Rent.as.a.Percentage.of.Household..Income.in.the.Past.12.Months..Dollars., 
      Average.Commute.to.Work..In.Min.) %>% mutate(year = 2023) %>% rename(name = Qualified.Area.Name, FIPS = State..FIPS.Code., pop = Total.Population, housing_units = Housing.Units, 
      labor_force = Civilian.Population.in.Labor.Force.16.Years.and.Over., employed = Civilian.Population.in.Labor.Force.16.Years.and.Over..Employed,
      unemployed = Civilian.Population.in.Labor.Force.16.Years.and.Over..Unemployed, median_house_income = Median.Household.Income..In.2023.Inflation.Adjusted.Dollars.,
      per_capita_income = Per.Capita.Income..In.2023.Inflation.Adjusted.Dollars., median_house_value = Median.Value, median_gross_rent = Median.Gross.Rent,
      median_gross_rent_pct = Median.Gross.Rent.as.a.Percentage.of.Household..Income.in.the.Past.12.Months..Dollars., avg_commute = Average.Commute.to.Work..In.Min.
      )


ACS_2024_state_cleaned <- ACS_2024_state_new %>% select(Qualified.Area.Name, Total.Population, Housing.Units, 
      Civilian.Population.in.Labor.Force.16.Years.and.Over, Civilian.Population.in.Labor.Force.16.Years.and.Over..Employed, 
      Civilian.Population.in.Labor.Force.16.Years.and.Over..Unemployed, Median.Household.Income..In.2024.Inflation.Adjusted.Dollars., 
      Per.Capita.Income..In.2024.Inflation.Adjusted.Dollars., Median.House.Value, Median.Gross.Rent, Median.Gross.Rent.as.a.Percentage.of.Household.Income, 
      Average.Commute.to.Work..In.Min.) %>% mutate(year = 2024) %>% rename(name = Qualified.Area.Name, pop = Total.Population, housing_units = Housing.Units, 
      labor_force = Civilian.Population.in.Labor.Force.16.Years.and.Over, employed = Civilian.Population.in.Labor.Force.16.Years.and.Over..Employed,
      unemployed = Civilian.Population.in.Labor.Force.16.Years.and.Over..Unemployed, median_house_income = Median.Household.Income..In.2024.Inflation.Adjusted.Dollars.,
      per_capita_income = Per.Capita.Income..In.2024.Inflation.Adjusted.Dollars., median_house_value = Median.House.Value, median_gross_rent = Median.Gross.Rent,
      median_gross_rent_pct = Median.Gross.Rent.as.a.Percentage.of.Household.Income, avg_commute = Average.Commute.to.Work..In.Min.
      )

ACS_allset <- bind_rows(ACS_2019_state_cleaned, ACS_2020_state_cleaned, ACS_2021_state_cleaned, ACS_2022_state_cleaned, ACS_2023_state_cleaned, ACS_2024_state_cleaned)

ACS_allset1 <- ACS_allset %>% mutate(unemployment_rate = unemployed / (unemployed+employed))
#ACS_allset2 <- ACS_allset1 %>% mutate(labor_part = (unemployed + employed) / labor_force)
ACS_allset2 <- ACS_allset1 %>% mutate(house_income_ratio = median_house_value / median_house_income)

p <- ggplot(data = ACS_allset2, mapping = aes(x = median_house_income, y = median_house_value)) #Create ggplot where the x-axis is the log of GDP per Capita and the y axis is life expectancy. 
p + geom_point(alpha = .35) + geom_smooth() + labs(x = "Median Household Income", 
    y = "Median House Value", title = "Relationship of Household Median Income and Median House Value", 
    subtitle = "View of Household median income and median house value per state-year (2019-2024)", 
    caption = "Data: Social Explorer")

acs_clean <- ACS_2020_state_cleaned %>% # Define our ACS data frame
  select( # keep and rename the columns you'll use (the FIPS info plus the variables you'll select)
    state = FIPS,# state FIPS code
    pop = pop
    #county = Geo_COUNTY, # county FIPS code
    #gini = `SE_T163_001`, # Gini Coefficient Rate
    #publicassistance = `PCT_SE_T091_002`, #Households with Public Assistance Income
    #mortgage = `PCT_SE_T124_002` #Housing units with Mortgage 
  ) %>%
  mutate(
    state = as.numeric(state), # coerce to numeric
    #county = as.numeric(county), # coerce to numeric
    fips = state # construct full 5-digit FIPS
  ) %>% #the above is just one way to combine the state and county parts of the FIPs code - there are other ways to solve the problem addressed here.
  select(fips, pop) # keep only FIPS and outcome

options(tigris_use_cache = TRUE)

state_sf <- states(cb = TRUE, year = 2019) %>%   # cb = TRUE gives simplified boundaries
  filter(!STUSPS %in% c("Puerto Rico", "VI", "GU", "AS", "MP")) %>%
  mutate(fips = as.numeric(STATEFP)) %>%
  left_join(acs_clean, by = "fips") %>%
  select(fips, NAME, pop, geometry)

# optional: move Alaska and Hawaii under the lower 48
state_sf <- shift_geometry(state_sf)

ggplot(state_sf) + geom_sf(aes(fill = pop))

#Convert the datasets into a format where each observation has the state and year assigned to it (done)
#Need to rename all the variables into something cleaner and the same across all year datasets (done)
#Unemployment rate, labor force participation rate, median house value to median income ratio (make this)
#try to get gini coefficient into the data (either by redownloading the files or doing a merge)
#Figure out those widgets to make the website 

#reminder to self: you have population and housing units as variables. You can create new vars that measure the change in growth of these two
