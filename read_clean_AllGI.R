close.screen(all=TRUE)
rm(list=ls())
header=1
library(dplyr)

title_here="OUD"#OUD #IPV #SH #NLE #LBP
here_data="C:/Users/Guillaume/Desktop/Recoverit 2022-04-17 at 22.09.53/_DATA/"
here_project="C:/Users/Guillaume/Desktop/Recoverit 2022-04-17 at 22.09.53/PROJECTS/"
outcome_has_been_loaded=0
use_pop=0


##########
#first load data
#permalink
#https://ghdx.healthdata.org/gbd-results-tool?params=gbd-api-2019-permalink/7b9ec5587771bfac724b62482408434a##########


##########
#Y- DALYs read
##########
here_data_DALYs=paste0(here_data,"all_data_files_GBD/DALYs/Rate_",
                       title_here, "_204locations_3yrs_Agestd/")
list_files=list.files(path=here_data_DALYs, pattern = 
                        glob2rx("IHME*\\csv"))#
#"^IHME
#\\.csv$")

#measure e.g. DALYs or incidence
#location e.g. Maldives
#sex
#age
#cause
#metric Rate or Nb
#year

#cluster your analysis
if (outcome_has_been_loaded==0) {
    i=1
    tmp=read.csv(paste0(here_data_DALYs,list_files[i]))
    outcome_matrix_ <- tmp %>%
      dplyr::filter(measure=="DALYs (Disability-Adjusted Life Years)") %>% 
      #filter(cause=="Depressive disorders") %>% #main MBD disorders
      # filter(metric=="Rate") %>% #"Number" or "Rate"
      # filter(sex=="Male" | sex=="Female") %>%
      select(-c(upper,lower))
  
  }


####################
# Merge with covariates and Exposure
####################
##Use both list to find A and W's
which(outcome_matrix_$location %in% "CÃ´te d'Ivoire")
outcome_matrix <- outcome_matrix_ %>%
  mutate(location=case_when(
    location=="CÃ´te d'Ivoire"~"Cote d'Ivoire",
    TRUE ~ as.character(location))) %>%
  rename(Y=val)
outcome_location=as_tibble(unique(outcome_matrix$location))
which(outcome_location$value %in% "CÃ´te d'Ivoire")



##########
#A- globalization index read
##########
data_global=paste0(here_data,"Globalization/KOFGI_2021_public.csv")
data_global=read.csv(data_global,header = TRUE, sep=",")
#adjust country names, some of which you have to remove
data_global <- data_global %>%
  mutate(country=case_when(
      country=="Aruba" ~ NA_character_,
      country=="Bahamas, The" ~ "Bahamas",
      country=="Bolivia" ~ "Bolivia (Plurinational State of)",
      country=="Cayman Islands" ~ NA_character_,
      country=="Congo, Dem Rep" ~ "Democratic Republic of the Congo",
      country=="Congo, Rep" ~ "Congo",
      country=="Czech Republic" ~ "Czechia",
      country=="East Asia and Pacific" ~ NA_character_,
      country=="Egypt, Arab Rep" ~ "Egypt",
      country=="Europe and Central Asia" ~ NA_character_,
      country=="Faroe Islands" ~ NA_character_,
      country=="French Polynesia" ~ NA_character_,
      country=="Gambia, The" ~ "Gambia",
      country=="High income" ~ NA_character_,
      country=="Hong Kong SAR, China" ~ NA_character_,
      country=="Iran, Islamic Rep" ~ "Iran (Islamic Republic of)",
      country=="Korea, Dem People’s Rep" ~ "Democratic People's Republic of Korea",
      country=="Korea, Rep" ~ "Republic of Korea",
      country=="Kyrgyz Republic" ~ "Kyrgyzstan",
      country=="Lao PDR" ~ "Lao People's Democratic Republic",
      country=="Latin America and Caribbean" ~ NA_character_,
      country=="Liechtenstein" ~ NA_character_,
      country=="Low income" ~ NA_character_,
      country=="Lower middle income" ~ NA_character_,
      country=="Macao SAR, China" ~ NA_character_,
      country=="Micronesia, Fed Sts"~ "Micronesia (Federated States of)",
      country=="Middle East and North Africa" ~ NA_character_,
      country=="Moldova" ~ "Republic of Moldova",
      country=="New Caledonia" ~ NA_character_,
      country=="North America" ~ NA_character_,
      country=="Slovak Republic" ~ "Slovakia",
      country=="South Asia" ~ NA_character_,
      country=="St Kitts and Nevis" ~ "Saint Kitts and Nevis",
      country=="St Lucia" ~ "Saint Lucia",
      country=="St Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines",
      country=="Sub-Saharan Africa" ~ NA_character_,
      country=="Tanzania" ~ "United Republic of Tanzania",
      country=="United States" ~ "United States of America",
      country=="Upper middle income" ~ NA_character_,
      country=="Venezuela, RB" ~ "Venezuela (Bolivarian Republic of)",
      country=="Vietnam" ~ "Viet Nam",
      country=="Virgin Islands (US)" ~ "United States Virgin Islands",
      country=="West Bank and Gaza" ~ NA_character_,
      country=="World" ~ NA_character_,
      country=="Yemen, Rep" ~ "Yemen",
      TRUE ~ as.character(country)
  )) %>%
  rename(location=country)

#check NA values
{
data_global_location=as_tibble(unique(data_global$location))
joined <- data_global_location %>%
  right_join(outcome_location, copy = TRUE, keep = TRUE)
sum(is.na(joined)) 
tmp<-joined[which(is.na(joined$value.y)),1]
}




##########################################################################################
#W- covariates read
##########################################################################################


#########
#- Unemployment rate
#########
data_unemploy=paste0(here_data,"Unemploy/WB_unemploy.csv")
data_unemploy=read.csv(data_unemploy, header=TRUE, sep=";")
data_unemploy <- data_unemploy %>%
  mutate(Country.Name=case_when(
    Country.Name=="CÃ´te d'Ivoire"~"Cote d'Ivoire",
    TRUE ~ as.character(Country.Name)
  ))
#pivot longer
data_unemploy <- data_unemploy %>%
  tidyr::pivot_longer(
    cols=X1990:X2020,
    names_to="year",
    names_prefix="X",
    values_to="unemploy"
  ) %>%
  dplyr::rename(location=Country.Name) %>%
  dplyr::mutate(across("year", ~ as.numeric(.))) %>%
  dplyr::mutate(across("unemploy", ~ as.numeric(gsub(pattern=",", replacement = ".",.))))

#check NA values
{
  data_location=as_tibble(unique(data_unemploy$location))
  joined <- data_location %>%
    right_join(outcome_location, copy = TRUE, keep = TRUE)
  sum(is.na(joined)) 
  tmp<-joined[which(is.na(joined$value.x)),2]
}


#########
#- development index (with richness, education and fertility)
#########
data_SDI=paste0(here_data,"all_data_files_GBD/SDI/all_SDI.csv")
data_SDI=read.csv(data_SDI, header=TRUE, sep=";")
data_SDI <- data_SDI %>%
  mutate(Location=case_when(
    Location=="CÃ´te d'Ivoire"~"Cote d'Ivoire",
    TRUE ~ as.character(Location)
  ))
#pivot longer
data_SDI <- data_SDI %>%
  tidyr::pivot_longer(
    cols=X1990:X2019,
    names_to="year",
    names_prefix="X",
    values_to="SDI"
  ) %>%
  dplyr::rename(location=Location) %>%
  dplyr::mutate(across("year", ~ as.numeric(.)))

#check NA values
{
  data_location=as_tibble(unique(data_SDI$location))
  joined <- data_location %>%
    right_join(outcome_location, copy = TRUE, keep = TRUE)
  sum(is.na(joined)) 
  tmp<-joined[which(is.na(joined$value.x)),2]
}


#########
#- Urbanity index
#########
data_urban=paste0(here_data,"all_data_files_GBD/URBANICITY.csv")
data_urban=read.csv(data_urban, header=TRUE, sep=";")
data_urban <- data_urban %>%
  mutate(location=case_when(
    location=="CÃ´te d'Ivoire"~"Cote d'Ivoire",
    TRUE ~ as.character(location)
  ))

#check NA values
{
  data_location=as_tibble(unique(data_urban$location))
  joined <- data_location %>%
    right_join(outcome_location, copy = TRUE, keep = TRUE)
  sum(is.na(joined)) 
  tmp<-joined[which(is.na(joined$value.x)),2]
  }


#########
#- UN-Happiness index
#########
data_unhappy=paste0(here_data,"all_data_files_GBD/gallup.csv")
data_unhappy=read.csv(data_unhappy, header=TRUE, sep=";")
data_unhappy <- data_unhappy %>%
  mutate(location=case_when(
    location=="CÃ´te d'Ivoire"~"Cote d'Ivoire",
    TRUE ~ as.character(location)
  )) %>%
  rename(unhappy=val)

#check NA values
{
  data_location=as_tibble(unique(data_unhappy$location))
  joined <- data_location %>%
    right_join(outcome_location, copy = TRUE, keep = TRUE)
  sum(is.na(joined)) 
  tmp<-joined[which(is.na(joined$value.x)),2]
  }


#########
#- Childhood abuse (GBD data sources)
#########
data_CSA=paste0(here_data,"all_data_files_GBD/GBD_AGESTD_CSA.csv")
data_CSA=read.csv(data_CSA, header=TRUE, sep=";")
data_CSA <- data_CSA %>%
  mutate(location=case_when(
    location=="CÃ´te d'Ivoire"~"Cote d'Ivoire",
    location=="Georgia"~"Georgia US",
    location=="Georgia (EUR)"~"Georgia",
    TRUE ~ as.character(location)
  ))

#check NA values
{
  data_location=as_tibble(unique(data_CSA$location))
  joined <- data_location %>%
    right_join(outcome_location, copy = TRUE, keep = TRUE)
  sum(is.na(joined)) 
  tmp<-joined[which(is.na(joined$value.x)),2]
  }


#########
#- Income inequality WID database (missing cells probably)
#########
data_p90p100=paste0(here_data,"BMJO VP/Inequal.csv")
data_p90p100=read.csv(data_p90p100, header=TRUE, sep=";")
data_p90p100 <- data_p90p100 %>%
  mutate(location=case_when(
    location=="CÃ´te d'Ivoire"~"Cote d'Ivoire",
    TRUE ~ as.character(location)
  )) %>%
  mutate(across(year, ~as.numeric(.)))

#check NA values
{
  data_location=as_tibble(unique(data_p90p100$location))
  joined <- data_location %>%
    right_join(outcome_location, copy = TRUE, keep = TRUE)
  sum(is.na(joined)) 
  tmp<-joined[which(is.na(joined$value.x)),2]
  }


#########
#- health systems QUality and access
#########
data_HAQI=paste0(here_data,"all_data_files_GBD/HAQI.csv")
data_HAQI=read.csv(data_HAQI, header=TRUE, sep=";")
data_HAQI <- data_HAQI %>%
  mutate(location=case_when(
    location=="CÃ´te d'Ivoire"~"Cote d'Ivoire",
    TRUE ~ as.character(location)
  ))

#check NA values
{
  data_location=as_tibble(unique(data_HAQI$location))
  joined <- data_location %>%
    right_join(outcome_location, copy = TRUE, keep = TRUE)
  sum(is.na(joined)) 
  tmp<-joined[which(is.na(joined$value.x)),2]
  }


#########
#- Population
#########
#big if condition
if (use_pop) {
  
data_pop=paste0(here_data,"all_data_files_GBD/population/all_pop.csv")
data_pop=read.csv(data_pop, header=TRUE, sep=",")
data_pop <- data_pop %>%
  filter(year==2018) %>%
  filter(X!=2891628) %>% #dont take US Georgia
  mutate(location=case_when(
    location=="CÃ´te d'Ivoire"~"Cote d'Ivoire",
    TRUE ~ as.character(location)
  )) %>%
  rename(pop_tot=pop)

#check NA values
{
  data_location=as_tibble(unique(data_pop$location))
  joined <- data_location %>%
    right_join(outcome_location, copy = TRUE, keep = TRUE)
  sum(is.na(joined)) 
  tmp<-joined[which(is.na(joined$value.x)),2]
  }



##########
#Population by age and sex
##########
here_data_pop_struct=paste0(here_data,"all_data_files_GBD/population/")
list_files=list.files(path=here_data_pop_struct, pattern = 
                        glob2rx("IHME_*\\CSV"))#
all_location=outcome_location

#cluster your analysis
if (pop_has_been_loaded==1) {
  
  library(R.utils)
  
  results=list()
  
  num_cores <- detectCores()
  cl <- makeCluster(num_cores-1)
  clusterEvalQ(cl,c(library(dplyr),library(R.utils)))#c(library(SuperLearner),library(dplyr))
  clusterExport(cl, list("here_data_pop_struct","list_files","all_location"))
  
  try_this <- function(i) {
    
    i=29#year 2018
    tmp1=read.csv(paste0(here_data_pop_struct,list_files[i]), sep=",") 
    tmp2<- tmp1 %>%
      mutate(location=case_when(
        location_name=="CÃ´te d'Ivoire"~"Cote d'Ivoire",#location_name
        location_name=="Côte d'Ivoire"~"Cote d'Ivoire",#location_name
        TRUE ~ as.character(location_name))) %>%
      filter(location_id!=533) %>%
      mutate(sex_name=capitalize(sex_name)) %>%
      rename(c(pop=val,sex=sex_name,age=age_group_name, year=year_id)) 
    
    tmp3 <- tmp2 %>%
      #right_join(all_location,by=c("location"="value")) %>%
      filter(sex=="Male" | sex=="Female") %>%
      filter(age=="15 to 19" | 
               age=="20 to 24" | age=="25 to 29" | 
               age=="30 to 34" | age=="35 to 39" | 
               age=="40 to 44" 
             #| age=="0 to 44" | age=="45 to 49" | age=="50 to 54" | age=="55 to 59"
      ) %>%
      select(location,sex, age, year, pop)
    
    # debug - Georgia AGAIN!!!!
    # outcome_matrix_ %>%
    #   group_by(location) %>%
    #   summarise(N=n()) %>%
    #   arrange(N)
    # end of debug
    
    return(tmp3)
  }
  
  
  system.time(results <-clusterApply(cl,1:length(list_files),try_this))
  
  stopCluster(cl)
  
  data_pop=as_tibble(bind_rows(results))

}

#merge tmp3 with data_pop
data_pop_struct <- data_pop %>%
  left_join(tmp3, by = c("location","year")) %>%
  mutate(pct_pop=pop/pop_tot)

save(file="G:/PROJECTS/2022/sAP Globalization/data_pop_struct.RData",data_pop_struct)


#end of big if condition
}


###################
#- Quality of data sources from the GBD
###################
data_quality=read.csv(paste0(here_data,"all_data_files_GBD/GBD Quality.csv"), 
                 header=TRUE, sep=";")
data_quality <- data_quality %>%
  mutate(location=case_when(
    location=="CÃ´te d'Ivoire"~"Cote d'Ivoire",
    TRUE ~ as.character(location)
  ))

#check NA values
{
  data_location=as_tibble(unique(data_quality$location))
  joined <- data_location %>%
    right_join(outcome_location, copy = TRUE, keep = TRUE)
  sum(is.na(joined)) 
  tmp<-joined[which(is.na(joined$value.x)),2]
  }




#############################################################################
#build up raw design matrix
#############################################################################
final_matrix <- outcome_matrix %>%
  left_join(dplyr::select(data_global,location:KOFPoGIdj), by=c("location","year")) %>%
  left_join(data_unemploy, by=c("location","year")) %>%
  left_join(data_SDI, by=c("location","year")) %>%
  left_join(dplyr::select(data_urban,c(location,year,urban)), by=c("location","year")) %>%
  left_join(dplyr::select(data_unhappy,c(location,year,unhappy)), by=c("location","year")) %>%
  left_join(dplyr::select(data_CSA,c(location,year,CSA)), by=c("location","year")) %>%
  left_join(dplyr::select(data_p90p100,c(location,year,p90p100)), by=c("location","year")) %>%
  left_join(dplyr::select(data_HAQI,c(location,year,haqi)), by=c("location","year")) %>%
  #left_join(dplyr::select(data_pop,c(location,year,pop)), by=c("location","year")) %>%
  left_join(dplyr::select(data_quality,c(location,Quality)), by=c("location")) 
  

#transform char into factors
#No need as no char.
save(file=paste0(here_project,"2022/sAP Globalization/",title_here,"_Astd_AllGI_DALYs.RData"),
     final_matrix)
