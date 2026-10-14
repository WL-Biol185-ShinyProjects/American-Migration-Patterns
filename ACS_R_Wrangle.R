library(dplyr)
library(tidyverse)

library(shiny)

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

#Convert the datasets into a format where each observation has the state and year assigned to it
#Need to rename all the variables into something cleaner and the same across all year datasets
#Figure out those widgets to make the website

#reminder to self: you have population and housing units as variables. You can create new vars that measure the change in growth of these two
