# Load_libraries

library(tidyverse)
library(knitr)
library(pastecs)
library(magrittr)

rm(list=ls()) # To clear the environment

# Read_OAC_data

OAC_2011 <-
  readr::read_csv("Data/2011_OAC_Raw_kVariables.csv")

# Creating new table for assigned LAD - Wolverhampton 

Wolverhampton_LAD <-
  readr::read_csv("Data/OA11_LSOA11_MSOA11_LAD11_EW_LUv2.csv") %>% 
  dplyr::filter(LAD11CD == "E08000031") %>%
  dplyr::select(-LAD11NMW) %>%
  readr::write_csv("Data/Wolverhampton_LAD.csv")

# Read_LAD data

Wolverhampton_LAD <-
  readr::read_csv("Data/Wolverhampton_LAD.csv")

# tibble joining 

OAC_2011 %>%
  
dplyr::inner_join(

  Wolverhampton_LAD,
  by = c("OA" = "OA11CD")
) %>%
  dplyr::select(OA, Total_Population, Total_Households, Total_Dwellings,
                Total_Household_Spaces, Total_Population_16_and_over,
                Total_Population_16_to_74, Total_Employment_16_to_74, 
                Total_Pop_in_Housesholds_16_and_over, 
                k004, k009, k010, k027, k031, k041,k046
  ) %>%
  readr::write_csv("Data/Wolverhampton_OAC2011.csv")

# Read_Wolverhampton_data

Wolverhampton_2011OAC <-
  readr::read_csv("Data/Wolverhampton_OAC2011.csv")

# Option A
## Question A.1

### Data Visualisation
### Distribution of variables

#We start the analysis with the simple histogram, to explore the distribution of
#the variable

#K004

summary(Wolverhampton_2011OAC$k004)

#Histogram

Wolverhampton_2011OAC %>%
  ggplot2::ggplot (
    aes(
      x = k004
    )
  ) +
  ggplot2::geom_histogram(binwidth = 5, fill="#e41a1c", colour="black") +
  ggplot2::ggtitle("k004 : Persons aged 45 to 64") +
  ggplot2::xlab("Population of Age") +
  ggplot2::ylab("Count") +
  ggplot2::theme_bw()

#Scatterplot 

Wolverhampton_2011OAC %>%
  ggplot2::ggplot(
    aes(
      x = Total_Population,
      y = k004
    )
  )+
  ggplot2::geom_point(color= "black", shape = 23, size = 1, fill = "#e41a1c") +
  ggplot2::ggtitle("Wolverhampton persons aged 45 to 64") +
  ggplot2::xlab("Total number of population") +
  ggplot2::ylab("Persons aged 45 to 64") +
  ggplot2::scale_y_log10() +
  ggplot2::theme_bw()

#Maximum

k004_max <- 
Wolverhampton_2011OAC %>%
dplyr::filter(k004>20) %>%
  dplyr::select(OA, k004) %>%
  dplyr::slice_max(k004, n=20)

ggplot2::ggplot(k004_max,
       aes(
         x = k004,
         y = OA,
         )
       )+
ggplot2::geom_bar(position = "stack", stat = "identity", fill="#e41a1c", colour="black") +
ggplot2::ggtitle("Top 20 regions of Wolverhampton person aged 45 to 64")+
ggplot2::xlab("Population age")+
ggplot2::ylab("Output Area (OA)")+
ggplot2::theme_bw() 

#Minimum

k004_min <- 
  Wolverhampton_2011OAC %>%
  dplyr::filter(k004>20) %>%
  dplyr::select(OA, k004) %>%
  dplyr::slice_min(k004, n=20)

ggplot2::ggplot(k004_min,
                aes(
                  x = k004,
                  y = OA,
                )
)+
  ggplot2::geom_bar(position = "stack", stat = "identity", fill="#e41a1c", colour="black") +
  ggplot2::ggtitle("Bottom 20 regions of Wolverhampton person aged 45 to 64")+
  ggplot2::xlab("Population age")+
  ggplot2::ylab("Output Area (OA)")+
  ggplot2::theme_bw() 

#k009

summary(Wolverhampton_2011OAC$k009)

#Histogram

Wolverhampton_2011OAC %>%
  ggplot2::ggplot (
    aes(
      x = k009
    )
  ) +
  ggplot2::geom_histogram(binwidth = 5, fill="#f781bf", colour="black") +
  ggplot2::ggtitle("k009 : Persons aged over 16 who are single") +
  ggplot2::xlab("Marital_and_Civil_Partnership_Status") +
  ggplot2::ylab("Count") +
  ggplot2::theme_bw()

#Scatterplot

Wolverhampton_2011OAC %>%
  ggplot2::ggplot(
    aes(
      x = Total_Population_16_and_over,
      y = k009
    )
  )+
  ggplot2::geom_point(color= "black", shape = 23, size = 1, fill = "#f781bf") +
  ggplot2::ggtitle("Wolverhampton populations Marital and Civil Partnership Status") +
  ggplot2::xlab("Total population aged 16 and over") +
  ggplot2::ylab("Persons aged over 16 who are single") +
  ggplot2::scale_y_log10() +
  ggplot2::theme_bw()

#Maximum

k009_max <- 
  Wolverhampton_2011OAC %>%
  dplyr::select(OA, k009) %>%
  dplyr::slice_max(k009, n=20)

ggplot2::ggplot(k009_max,
                aes(
                  x = k009,
                  y = OA,
                )
)+
  ggplot2::geom_bar(position = "stack", stat = "identity", fill="#f781bf", colour="black") +
  ggplot2::ggtitle("Top 20 regions of Wolverhamptom Marital_and_Civil_Partnership_Status")+
  ggplot2::xlab("Persons aged over 16 who are single")+
  ggplot2::ylab("Output Area (OA)")+
  ggplot2::theme_bw() 

#Minimum

k009_min <- 
  Wolverhampton_2011OAC %>%
  dplyr::select(OA, k009) %>%
  dplyr::slice_min(k009, n=20)

ggplot2::ggplot(k009_min,
                aes(
                  x = k009,
                  y = OA,
                )
)+
  ggplot2::geom_bar(position = "stack", stat = "identity", fill="#f781bf", colour="black") +
  ggplot2::ggtitle("Bottom 20 regions of Wolverhamptom Marital_and_Civil_Partnership_Status")+
  ggplot2::xlab("Persons aged over 16 who are single")+
  ggplot2::ylab("Output Area (OA)")+
  ggplot2::theme_bw() 

#k010

summary(Wolverhampton_2011OAC$k010)

#Histogram

Wolverhampton_2011OAC %>%
  ggplot2::ggplot (
    aes(
      x = k010
    )
  ) +
  ggplot2::geom_histogram(binwidth = 5, fill="#ff7f00", colour="black") +
  ggplot2::ggtitle("k010 : Persons aged over 16 who are married or in a registered same-sex civil partnership") +
  ggplot2::xlab("Marital_and_Civil_Partnership_Status") +
  ggplot2::ylab("Count") +
  ggplot2::theme_bw()

#Scatterplot

Wolverhampton_2011OAC %>%
  ggplot2::ggplot(
    aes(
      x = Total_Population_16_and_over,
      y = k010
    )
  )+
  ggplot2::geom_point(color= "black", shape = 23, size = 1, fill = "#ff7f00") +
  ggplot2::ggtitle("Wolverhampton Marital and Civil Partnership Status") +
  ggplot2::xlab("Total populatoin aged 16 and over") +
  ggplot2::ylab("Persons aged over 16 who are married or in a registered same-sex civil partnership") +
  ggplot2::scale_y_log10() +
  ggplot2::theme_bw()

#Maximum

k010_max <- 
  Wolverhampton_2011OAC %>%
  dplyr::select(OA, k010) %>%
  dplyr::filter(k010>20) %>%
  dplyr::slice_max(k010, n=20)

ggplot2::ggplot(k010_max,
                aes(
                  x = k010,
                  y = OA,
                )
)+
  ggplot2::geom_bar(position = "stack", stat = "identity", fill="#ff7f00", colour="black") +
  ggplot2::ggtitle("Top 20 regions of Wolverhamptom Marital and Civil Partnership Status")+
  ggplot2::xlab("Persons aged over 16 who are married or in a registered same-sex civil partnership")+
  ggplot2::ylab("Output Area (OA)")+
  ggplot2::theme_bw() 

#Minimum

k010_min <- 
  Wolverhampton_2011OAC %>%
  dplyr::filter(k010>20) %>%
  dplyr::select(OA, k010) %>%
  dplyr::slice_min(k010, n=20)

ggplot2::ggplot(k010_min,
                aes(
                  x = k010,
                  y = OA,
                )
)+
  ggplot2::geom_bar(position = "stack", stat = "identity", fill="#ff7f00", colour="black") +
  ggplot2::ggtitle("Bottom 20 regions of Wolverhamptom Marital_and_Civil_Partnership_Status")+
  ggplot2::xlab("Persons aged over 16 who are married or in a registered same-sex civil partnership")+
  ggplot2::ylab("Output Area (OA)")+
  ggplot2::theme_bw() 

#k027

summary(Wolverhampton_2011OAC$k027)

#Histogram

Wolverhampton_2011OAC %>%
  ggplot2::ggplot (
    aes(
      x = k027
    )
  ) +
  ggplot2::geom_histogram(binwidth = 5, fill="#a65628", colour="black") +
  ggplot2::ggtitle("k027 : Households who live in a detached house or bungalow") +
  ggplot2::xlab("Housing Type") +
  ggplot2::ylab("Count") +
  ggplot2::theme_bw()

#Scatterplot

Wolverhampton_2011OAC %>%
  ggplot2::ggplot(
    aes(
      x = Total_Household_Spaces,
      y = k027
    )
  )+
  ggplot2::geom_point(color= "black", shape = 23, size = 1, fill = "#a65628") +
  ggplot2::ggtitle("Households who live in a detached house or bungalow in Wolverhampton") +
  ggplot2::xlab("Total dwelling types") +
  ggplot2::ylab("Housing types") +
  ggplot2::scale_y_log10() +
  ggplot2::theme_bw() 

#Maximum

k027_max <- 
  Wolverhampton_2011OAC %>%
  dplyr::select(OA, k027) %>%
  dplyr::slice_max(k027, n=20)

ggplot2::ggplot(k027_max,
                aes(
                  x = k027,
                  y = OA,
                )
)+
  ggplot2::geom_bar(position = "stack", stat = "identity", fill="#a65628", colour="black") +
  ggplot2::ggtitle("Top 20 regions of Wolverhamptom Housing types")+
  ggplot2::xlab("Households who live in a detached house or bungalow")+
  ggplot2::ylab("Output Area (OA)")+
  ggplot2::theme_bw() 

#Minimum

k027_min <- 
  Wolverhampton_2011OAC %>%
  dplyr::select(OA, k027) %>%
  dplyr::filter(k027>20) %>%
  dplyr::slice_min(k027, n=20)

ggplot2::ggplot(k027_min,
                aes(
                  x = k027,
                  y = OA,
                )
)+
  ggplot2::geom_bar(position = "stack", stat = "identity", fill="#a65628", colour="black") +
  ggplot2::ggtitle("Bottom 20 regions of Wolverhamptom Housing types")+
  ggplot2::xlab("Households who live in a detached house or bungalow")+
  ggplot2::ylab("Output Area (OA)")+
  ggplot2::theme_bw() 

#k031

summary(Wolverhampton_2011OAC$k031)

#Histogram

Wolverhampton_2011OAC %>%
  ggplot2::ggplot (
    aes(
      x = k031
    )
  ) +
  ggplot2::geom_histogram(binwidth = 5, fill="#984ea3", colour="black") +
  ggplot2::ggtitle("k027 : Households who own or have shared ownership of property") +
  ggplot2::xlab("Housing Ownership") +
  ggplot2::ylab("Count") +
  ggplot2::theme_bw()

#Scatterplot

Wolverhampton_2011OAC %>%
  ggplot2::ggplot(
    aes(
      x = Total_Households,
      y = k031
    )
  )+
  ggplot2::geom_point(color= "black", shape = 23, size = 1, fill = "#984ea3") +
  ggplot2::ggtitle("Households who own or have shared ownership of property in Wolverhampton") +
  ggplot2::xlab("Total dwelling types") +
  ggplot2::ylab("Housing_Ownership") +
  ggplot2::scale_y_log10() +
  ggplot2::theme_bw()

#Maximum

k031_max <- 
  Wolverhampton_2011OAC %>%
  dplyr::select(OA, k031) %>%
  dplyr::filter(k031>20) %>%
  dplyr::slice_max(k031, n=20)

ggplot2::ggplot(k031_max,
                aes(
                  x = k031,
                  y = OA,
                )
)+
  ggplot2::geom_bar(position = "stack", stat = "identity", fill="#984ea3", colour="black") +
  ggplot2::ggtitle("Top 20 regions of Wolverhamptom Housing ownership")+
  ggplot2::xlab("Households who own or have shared ownership of property")+
  ggplot2::ylab("Output Area (OA)")+
  ggplot2::theme_bw() 

#Minimum

k031_min <- 
  Wolverhampton_2011OAC %>%
  dplyr::select(OA, k031) %>%
  dplyr::filter(k031>20) %>%
  dplyr::slice_min(k031, n=20)

ggplot2::ggplot(k031_min,
                aes(
                  x = k031,
                  y = OA,
                )
)+
  ggplot2::geom_bar(position = "stack", stat = "identity", fill="#984ea3", colour="black") +
  ggplot2::ggtitle("Bottom 20 regions of Wolverhamptom Housing types")+
  ggplot2::xlab("Households who live in a detached house or bungalow")+
  ggplot2::ylab("Output Area (OA)")+
  ggplot2::theme_bw() 

#k041  

summary(Wolverhampton_2011OAC$k041)

Histogram

Wolverhampton_2011OAC %>%
  ggplot2::ggplot (
    aes(
      x = k041
    )
  ) +
  ggplot2::geom_histogram(binwidth = 5, fill="#377eb8", colour="black") +
  ggplot2::ggtitle("k041 : Households with two or more cars or vans") +
  ggplot2::xlab("Households Vehicle Availability") +
  ggplot2::ylab("Count") +
  ggplot2::theme_bw()

#Scatterplot

Wolverhampton_2011OAC %>%
  ggplot2::ggplot(
    aes(
      x = Total_Households,
      y = k041
    )
  )+
  ggplot2::geom_point(color= "black", shape = 23, size = 1, fill = "#377eb8") +
  ggplot2::ggtitle("Households with two or more cars or vans in Wolverhampton") +
  ggplot2::xlab("Total Household Spaces") +
  ggplot2::ylab("Vehcile Availability in households") +
  ggplot2::scale_y_log10() +
  ggplot2::theme_bw() 

#Maximum

k041_max <- 
  Wolverhampton_2011OAC %>%
  dplyr::select(OA, k041) %>%
  dplyr::filter(k041>20) %>%
  dplyr::slice_max(k041, n=20)

ggplot2::ggplot(k041_max,
                aes(
                  x = k041,
                  y = OA,
                )
)+
  ggplot2::geom_bar(position = "stack", stat = "identity", fill="#377eb8", colour="black") +
  ggplot2::ggtitle("Top 20 regions of Wolverhampton vehicle availability")+
  ggplot2::xlab("Households with two or more cars or vans")+
  ggplot2::ylab("Output Area (OA)")+
  ggplot2::theme_bw() 

#Minimum

k041_min <- 
  Wolverhampton_2011OAC %>%
  dplyr::select(OA, k041) %>%
  dplyr::filter(k041>20) %>%
  dplyr::slice_min(k041, n=20)

ggplot2::ggplot(k041_min,
                aes(
                  x = k041,
                  y = OA,
                )
)+
  ggplot2::geom_bar(position = "stack", stat = "identity", fill="#377eb8", colour="black") +
  ggplot2::ggtitle("Bottom 20 regions of Wolverhamptom vehicle availability")+
  ggplot2::xlab("Households with two or more cars or vans")+
  ggplot2::ylab("Output Area (OA)")+
  ggplot2::theme_bw() 


#k046

summary(Wolverhampton_2011OAC$k046)
  
#Histogram

Wolverhampton_2011OAC %>%
  ggplot2::ggplot (
    aes(
      x = k046
    )
  ) +
  ggplot2::geom_histogram(binwidth = 5, fill="#ffff33", colour="black") +
  ggplot2::ggtitle("k046:Employed persons aged between 16 and 74 who work part-time") +
  ggplot2::xlab("Employment_16_to_74 works in part-time") +
  ggplot2::ylab("Count") +
  ggplot2::theme_bw()

#Scatterplot

Wolverhampton_2011OAC %>%
  ggplot2::ggplot(
    aes(
      x = Total_Household_Spaces,
      y = k046
    )
  )+
  ggplot2::geom_point(color= "black", shape = 23, size = 1, fill = "#ffff33") +
  ggplot2::ggtitle("Employed persons aged between 16 and 74 who work part-time") +
  ggplot2::xlab("Total Persons Employed aged 16 to 74") +
  ggplot2::ylab("Employment Hours who works part-time") +
  ggplot2::scale_y_log10() +
  ggplot2::theme_bw() 

#Maximum

k046_max <- 
  Wolverhampton_2011OAC %>%
  dplyr::select(OA, k046) %>%
  dplyr::slice_max(k046, n=20)

ggplot2::ggplot(k046_max,
                aes(
                  x = k046,
                  y = OA,
                )
)+
  ggplot2::geom_bar(position = "stack", stat = "identity", fill="#ffff33", colour="black") +
  ggplot2::ggtitle("Bottom 20 regions of Wolverhamptom Employed persons who work part-time")+
  ggplot2::xlab("Employed persons aged between 16 and 74")+
  ggplot2::ylab("Output Area (OA)")+
  ggplot2::theme_bw() 

#Minimum

k046_min <- 
  Wolverhampton_2011OAC %>%
  dplyr::select(OA, k046) %>%
  dplyr::slice_min(k046, n=20)

ggplot2::ggplot(k046_min,
                aes(
                  x = k046,
                  y = OA,
                )
)+
  ggplot2::geom_bar(position = "stack", stat = "identity", fill="#ffff33", colour="black") +
  ggplot2::ggtitle("Bottom 20 regions of Wolverhamptom Employed persons who work part-time")+
  ggplot2::xlab("Employed persons aged between 16 and 74")+
  ggplot2::ylab("Output Area (OA)")+
  ggplot2::theme_bw() 

#Exploratory Analysis

Percentage <- 
  Wolverhampton_2011OAC %>%
  dplyr::mutate(
    Perc_k004 = (k004 / Total_Population) * 100,
    Perc_k009 = (k009 / Total_Population_16_and_over) * 100,
    Perc_k010 = (k010 / Total_Population_16_and_over) * 100,
    Perc_k027 = (k027 / Total_Household_Spaces) * 100,
    Perc_k031 = (k031 / Total_Households) * 100,
    Perc_k041 = (k041 / Total_Households) * 100,
    Perc_k046 = (k046 / Total_Employment_16_to_74) * 100
  ) %>%
  dplyr::select(OA, Perc_k004, Perc_k009, Perc_k010, 
                Perc_k027, Perc_k031, Perc_k041, Perc_k046
                )
  

#Descriptive
wolverhampton_stat_desc <- 
  Percentage %>%
  dplyr::select(Perc_k004, Perc_k009, Perc_k010, 
                Perc_k027, Perc_k031, Perc_k041, Perc_k046) %>%
  pastecs::stat.desc(norm =TRUE)

wolverhampton_stat_desc %>%
  knitr::kable(digits = 5)

# Shapiro test and QQ plot

#004
Percentage %>%
  dplyr::pull(Perc_k004) %>%
  stats::shapiro.test()

Percentage %>%
  ggplot2::ggplot(
    aes(
      x = Perc_k004
    )
  ) +
  ggplot2::geom_histogram(
    aes(
      y =..density..
    ),
    binwidth = 5,
    fill = "black",
    colour = "grey"
  ) + 
  ggplot2::stat_function(
    fun = dnorm, 
    args = list(
      mean = Percentage %>% pull(Perc_k004) %>% mean(),
      sd = Percentage %>% pull(Perc_k004) %>% sd()
    ),
    colour = "red", size = 1
  )

Percentage %>%
  ggplot2::ggplot(
    aes(
      sample = Perc_k004
    )
  ) +
  ggplot2::stat_qq() +
  ggplot2::stat_qq_line(col = "darkblue")

#009

Percentage %>%
  dplyr::pull(Perc_k009) %>%
  stats::shapiro.test()  

Percentage %>%
  ggplot2::ggplot(
    aes(
      x = Perc_k009
    )
  ) +
  ggplot2::geom_histogram(
    aes(
      y =..density..
    ),
    binwidth = 5,
    fill = "black",
    colour = "grey"
  ) + 
  ggplot2::stat_function(
    fun = dnorm, 
    args = list(
      mean = Percentage %>% pull(Perc_k009) %>% mean(),
      sd = Percentage %>% pull(Perc_k009) %>% sd()
    ),
    colour = "red", size = 1
  )

Percentage %>%
  ggplot2::ggplot(
    aes(
      sample = Perc_k009
    )
  ) +
  ggplot2::stat_qq() +
  ggplot2::stat_qq_line(col = "darkblue")

#010

Percentage %>%
  dplyr::pull(Perc_k010) %>%
  stats::shapiro.test() 

Percentage %>%
  ggplot2::ggplot(
    aes(
      x = Perc_k010
    )
  ) +
  ggplot2::geom_histogram(
    aes(
      y =..density..
    ),
    binwidth = 5,
    fill = "black",
    colour = "grey"
  ) + 
  ggplot2::stat_function(
    fun = dnorm, 
    args = list(
      mean = Percentage %>% pull(Perc_k010) %>% mean(),
      sd = Percentage %>% pull(Perc_k010) %>% sd()
    ),
    colour = "red", size = 1
  )

Percentage %>%
  ggplot2::ggplot(
    aes(
      sample = Perc_k010
    )
  ) +
  ggplot2::stat_qq() +
  ggplot2::stat_qq_line(col = "darkblue")

#027

Percentage %>%
  dplyr::pull(Perc_k027) %>%
  stats::shapiro.test() 

Percentage %>%
  ggplot2::ggplot(
    aes(
      x = Perc_k027
    )
  ) +
  ggplot2::geom_histogram(
    aes(
      y =..density..
    ),
    binwidth = 5,
    fill = "black",
    colour = "grey"
  ) + 
  ggplot2::stat_function(
    fun = dnorm, 
    args = list(
      mean = Percentage %>% pull(Perc_k027) %>% mean(),
      sd = Percentage %>% pull(Perc_k027) %>% sd()
    ),
    colour = "red", size = 1
  )

Percentage %>%
  ggplot2::ggplot(
    aes(
      sample = Perc_k027
    )
  ) +
  ggplot2::stat_qq() +
  ggplot2::stat_qq_line(col = "darkblue")

#031

Percentage %>%
  dplyr::pull(Perc_k031) %>%
  stats::shapiro.test() 

Percentage %>%
  ggplot2::ggplot(
    aes(
      x = Perc_k031
    )
  ) +
  ggplot2::geom_histogram(
    aes(
      y =..density..
    ),
    binwidth = 5,
    fill = "black",
    colour = "grey"
  ) + 
  ggplot2::stat_function(
    fun = dnorm, 
    args = list(
      mean = Percentage %>% pull(Perc_k031) %>% mean(),
      sd = Percentage %>% pull(Perc_k031) %>% sd()
    ),
    colour = "red", size = 1
  )

Percentage %>%
  ggplot2::ggplot(
    aes(
      sample = Perc_k031
    )
  ) +
  ggplot2::stat_qq() +
  ggplot2::stat_qq_line(col = "darkblue")

#041

Percentage %>%
  dplyr::pull(Perc_k041) %>%
  stats::shapiro.test() 

Percentage %>%
  ggplot2::ggplot(
    aes(
      x = Perc_k041
    )
  ) +
  ggplot2::geom_histogram(
    aes(
      y =..density..
    ),
    binwidth = 5,
    fill = "black",
    colour = "grey"
  ) + 
  ggplot2::stat_function(
    fun = dnorm, 
    args = list(
      mean = Percentage %>% pull(Perc_k041) %>% mean(),
      sd = Percentage %>% pull(Perc_k041) %>% sd()
    ),
    colour = "red", size = 1
  )


Percentage %>%
  ggplot2::ggplot(
    aes(
      sample = Perc_k041
    )
  ) +
  ggplot2::stat_qq() +
  ggplot2::stat_qq_line(col = "darkblue")

# 046

Percentage %>%
  dplyr::pull(Perc_k046) %>%
  stats::shapiro.test() 

Percentage %>%
  ggplot2::ggplot(
    aes(
      x = Perc_k046
    )
  ) +
  ggplot2::geom_histogram(
    aes(
      y =..density..
    ),
    binwidth = 5,
    fill = "black",
    colour = "grey"
  ) + 
  ggplot2::stat_function(
    fun = dnorm, 
    args = list(
      mean = Percentage %>% pull(Perc_k046) %>% mean(),
      sd = Percentage %>% pull(Perc_k046) %>% sd()
    ),
    colour = "red", size = 1
  )

Percentage %>%
  ggplot2::ggplot(
    aes(
      sample = Perc_k046
    )
  ) +
  ggplot2::stat_qq() +
  ggplot2::stat_qq_line(col = "darkblue")

# Multiple Linear regression

#Select and normalize variables

library(stargazer)
library(lmtest)

Wolverhampton_Household <-
  Wolverhampton_2011OAC %>%
  dplyr::select(
    OA, Total_Population, Total_Population_16_and_over, Total_Household_Spaces,
    Total_Households, Total_Employment_16_to_74,
    k004, k009, k010, k027, k031, k041, k046 
  ) %>%

# percentage of dependent and independent variables
dplyr::mutate (
  k004 = ( k004 / Total_Population) * 100,
  k009 = ( k009 / Total_Population_16_and_over) * 100,
  k010 = ( k010 / Total_Population_16_and_over) * 100,
  k027 = ( k027 / Total_Household_Spaces) * 100,
  k031 = ( k031 / Total_Households) * 100,
  k041 = ( k041 / Total_Households) * 100,
  k046 = ( k046 / Total_Employment_16_to_74) * 100
  ) %>%
#  rename columns
  dplyr::rename_with(
    function(x) {(paste0("Perc_", x))},
    c(k004, k009, k010, k027, k031, k041, k046)
  )

# create model

Household_model <-
  Wolverhampton_Household %$%
  lm(
    Perc_k031 ~ 
      Perc_k004 + Perc_k009 + Perc_k010 + Perc_k027 + Perc_k041 + Perc_k046
    )

#print summary
Household_model %>%
  summary()

# Not rendered in bookdown
stargazer(Household_model, header=FALSE)

Household_model %>%
  rstandard() %>%
  shapiro.test()

Household_model %>% 
  bptest()

Household_model %>%
  dwtest()

library(car)
Household_model %>%
  vif()

library(lm.beta)
lm.beta(Household_model)

#plots

Household_model %>%
  plot(which = c(1))

Household_model %>%
  plot(which = c(2))

Household_model %>%
  plot(which = c(3))

Household_model %>%
  plot(which = c(5))

Household_model_corkendall <-
  Household_model %>%
  stats::cor.test(
    perc_031,
    method = "kendall"
    )
Household_model_corkendall

  


