

0. Data and Packages.
=====================

Load Data and packages.

``` r
library(tidyverse)
library(sp)
library(data.table)
library(rebus)
library(readxl)
library(ggplot2)
library(nlme)
library(cvTools)
library(splines)
library(gam)
library(ggmap)
source("bigcon_function.R")
HDong_CD <- readRDS("HDong_CD.rds") %>%
  mutate(HDONG_CD = as.character(HDONG_CD))
HDONG_map <- readRDS("HDONG_map.rds")
Dust_Warning <- readRDS("Dust_Warning.rds") %>%
  dplyr::select(Date, pm10_Wrn = PM_10, pm25_Wrn = PM_25)
Wth_Day_HDONG <- readRDS("Wth_Day_HDONG.rds") 

Card <- readRDS("Card.rds")
Dist <- readRDS("Dist.rds")
SK_Age <- readRDS("SK_Age.rds")
SK_Time <- readRDS("SK_Time.rds")

Holiday <- read_excel("Holiday_List.xlsx") %>%
  mutate(Date = as.character(Date))

Dust_level  <- c("Good", "Moderate", "Sens_Unhealthy", "Unhealthy", "Very Unhealthy", "Worst")
Wrn_level <- c("No_Wrn", "Warning", "Dust_Watch")

Wth_Day_HDONG <- Wth_Day_HDONG %>%
  mutate(pm10_CTG = factor(pm10_CTG, levels = Dust_level), 
         pm25_CTG = factor(pm25_CTG, levels = Dust_level), 
         pm10_Wrn = factor(pm10_Wrn, levels = Wrn_level), 
         pm25_Wrn = factor(pm25_Wrn, levels = Wrn_level))

Wth_Day_HDONG <- Wth_Day_HDONG %>%
  mutate(Week_Day = strftime(Day, format = "%a") %>% as.character, 
         Holiday = ifelse(Week_Day %in% c("Sun", "Sat"), 1, 0))

Holiday_idx <- Wth_Day_HDONG$Day %in% Holiday$Date %>% which
Wth_Day_HDONG[Holiday_idx, "Holiday"] <- 1

Wth_Day_HDONG <- mutate(Wth_Day_HDONG, Holiday = factor(Holiday, levels = c(0, 1)))
```

1) SK\_Age
----------

``` r
SK_Age_HDONG_Modified <- SK_Age %>%
  lapply(FUN = function(x) x %>% 
           gather(key = "Type", value = "Avg_pop", -STD_YM, -STD_YMD, -HDONG_CD, -HDONG_NM) %>%
           separate(Type, into = c("Sex", "Age"), sep = "_FLOW_POP_CNT_")) %>%
           bind_rows(.id = "MONTH") %>%
  mutate(HDONG_CD = str_remove_all(HDONG_CD, pattern = DGT %R% DGT %R% END), 
         Age = str_extract(Age, pattern = START %R% DGT %R% DGT),
         AGE_CTG = case_when(Age < 20 ~ "Youth", 
                         Age %in% 20:39 ~ "Rising", 
                         Age %in% 40:59 ~ "Middle", 
                         Age >= 60 ~ "Senior"), 
         AGE_CTG = factor(AGE_CTG, levels = c("Youth", "Rising", "Middle", "Senior")),
         Avg_pop = as.numeric(Avg_pop),
         Non_zero = ifelse(Avg_pop == 0, 0, 1), 
         Age = as.numeric(Age), 
         Sex = factor(Sex)) %>%
  left_join(Wth_Day_HDONG, by = c("STD_YMD" = "Day", "HDONG_CD"))
```

2) SK\_Time
-----------

``` r
SK_Time_HDONG_Modified <- SK_Time %>% 
  lapply(FUN = function(x) x %>%
           gather(key = "Time", value = "Avg_pop", -STD_YM, -STD_YMD, -HDONG_CD, -HDONG_NM) %>%
           mutate(Time = parse_number(Time), Avg_pop = as.numeric(Avg_pop))) %>%
  bind_rows() %>%
  mutate(HDONG_CD = str_remove_all(HDONG_CD, pattern = DGT %R% DGT %R% END)) %>%
  left_join(Wth_Day_HDONG, by = c("STD_YMD" = "Day", "HDONG_CD")) %>%
  mutate(Non_zero = ifelse(Avg_pop == 0, 0, 1)) %>%
  mutate(Time_Group = case_when(Time %in% 0:7 ~ "Dawn", 
                                Time %in% c(8:10, 17:19) ~ "Rush_Hour", 
                                Time %in% 11:16 ~ "Afternoon", 
                                Time %in% 20:23 ~ "Evening"), 
         Time_Group = factor(Time_Group), 
         Holiday = factor(Holiday))
```

3) Dist
-------

``` r
Dist_Modified <- Dist %>%
  ungroup %>%
  dplyr::select(OPER_DT, ADMD_CD, AMT_IND) %>%
  unique %>%
   mutate(OPER_DT = strptime(OPER_DT, format = "%Y%m%d") %>% as.character()) %>%
  left_join(Wth_Day_HDONG, by = c("OPER_DT" = "Day", "ADMD_CD" = "HDONG_CD")) %>%
  separate(OPER_DT, into = c("Year", "Month", "Day")) %>%
  mutate(Month = month.abb[as.numeric(Month)], 
         Month = factor(Month, levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
                                          "Dec", "Jan", "Feb", "Mar"))) %>%
  left_join(HDong_CD, by = c("ADMD_CD" = "HDONG_CD")) %>%
  mutate(ADMD_CD = factor(ADMD_CD))
```

``` r
Real_Dist <- Dist %>%
  ungroup() %>%
  mutate(Real_Value = value * AMT_IND, 
         OPER_DT = strptime(OPER_DT, format = "%Y%m%d") %>% as.character()) %>%
  left_join(Wth_Day_HDONG, by = c("OPER_DT" = "Day", "ADMD_CD" = "HDONG_CD")) %>%
  separate(OPER_DT, into = c("Year", "Month", "Day")) %>%
  mutate(Month = month.abb[as.numeric(Month)], 
         Month = factor(Month, levels = c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", 
                                          "Dec", "Jan", "Feb", "Mar")), 
         ADMD_CD = factor(ADMD_CD), 
         Category = factor(Category))
```

1. Modelling
============

This is final model for the inferences.

1) SK\_Age
----------

Gamma-Hurdle Model for SK\_Age Data.

``` r
glm_SK_Age_Binom <- glm(Non_zero ~ ., data = SK_Age_HDONG_Modified %>% select(-MONTH, -STD_YM, -STD_YMD, -HDONG_NM, -Avg_pop, -Holiday), family = binomial(link = "logit"))
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
glm_SK_Age_Gamma_Hol_HDCTG <- glm(Avg_pop ~ HDONG_CD + Sex + Age + AGE_CTG + avg_temp + avg_pm10 + avg_pm25 + pm10_CTG + pm25_CTG + pm25_Wrn + Week_Day + Holiday + Sex:AGE_CTG + Holiday:Sex + Holiday:AGE_CTG + Holiday:pm10_CTG + HDONG_CD:pm25_CTG + Sex:pm25_CTG + pm25_CTG:AGE_CTG, 
                         data = SK_Age_HDONG_Modified %>% filter(Non_zero == 1) %>%
                          select(-MONTH, -STD_YM, -STD_YMD, -HDONG_NM, -Non_zero), 
                         family = Gamma(link = "log"))
```

2) SK\_Time
-----------

Gamma-Hurdle Model for SK\_Time Data

``` r
glm_SK_Time_Binom <- glm(Non_zero ~ ., data = SK_Time_HDONG_Modified %>% select(-STD_YM, -STD_YMD, -HDONG_NM, -Avg_pop), family = binomial)
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
 glm_SK_Time_Gamma_Hol_HD_CTG <- glm(Avg_pop ~ HDONG_CD + Time + Week_Day + avg_temp + avg_pm10 + avg_pm25 +
                                       pm10_CTG + pm25_CTG + pm10_Wrn + pm25_Wrn + Holiday + Time_Group + 
                                       Holiday:Time_Group + Holiday:pm10_CTG + Holiday:pm25_CTG + 
                                       Holiday:pm10_Wrn + Holiday:pm25_Wrn + HDONG_CD:pm25_CTG +
                                       Time_Group:pm25_CTG + Week_Day:pm25_CTG + Week_Day:Holiday + Week_Day:pm25_CTG:Holiday, 
                                     data = SK_Time_HDONG_Modified %>% filter(Non_zero == 1), 
                                     family = Gamma(link = "log"))
```

3) Dist
-------

``` r
glm_Dist_Best <- glm(AMT_IND ~ Month + avg_temp + avg_pm10 + ADMD_CD + 
    pm10_CTG + pm25_CTG + Week_Day + pm10_Wrn + pm25_Wrn + Holiday + 
    pm10_CTG:Holiday + pm25_CTG:Holiday + pm10_Wrn:Holiday + 
    Month:pm10_CTG + ADMD_CD:pm10_CTG, data = Dist_Modified %>% na.omit(), family = Gamma(link = "log"))
```

``` r
gam_Dist <- mgcv::gam(Real_Value ~ Month + ADMD_CD + Category + s(avg_temp) + s(avg_pm25) + s(avg_pm10) + pm10_CTG + pm25_CTG + pm10_Wrn + pm25_Wrn + ADMD_CD:Category + ADMD_CD:Month + Month:Category + Holiday + Holiday:Category + Holiday:pm25_CTG + Week_Day +  Category:pm10_CTG + Category:pm25_CTG + Holiday:Category:pm25_CTG, data = Real_Dist, select = TRUE)
```

2. Inferences
=============

Inference for the Models

1) SK\_Age
----------

``` r
summary_SK_Age <- summary(glm_SK_Age_Gamma_Hol_HDCTG)$coefficients %>% data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  tbl_df %>%
  `names<-`(value = c("Variable", "Estimate", "Std_Er", "t_val", "p_val"))
```

``` r
SK_Age_Main_Eff_Dust <- summary_SK_Age %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("pm25_CTG"), 
         !Variable %>% str_detect("HDONG_CD|Sex|AGE_CTG")) %>%
  mutate(pm25_CTG = Variable %>% str_remove("pm25_CTG")) %>%
  select(pm25_CTG, val_Dust = Estimate)

SK_Age_Main_Eff_HDONG <- summary_SK_Age %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("HDONG_CD"), 
         !Variable %>% str_detect("pm25_CTG")) %>%
  mutate(HDONG_CD = Variable %>% str_remove("HDONG_CD")) %>%
  select(HDONG_CD, val_HDONG = Estimate)

SK_Age_Dust_HDONG <- summary_SK_Age %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("HDONG_CD"), 
         Variable %>% str_detect("pm25_CTG")) %>%
  separate(Variable, into = c("HDONG_CD", "pm25_CTG"), sep = ":") %>%
  mutate(HDONG_CD = HDONG_CD %>% str_remove("HDONG_CD"), 
         pm25_CTG = pm25_CTG %>% str_remove("pm25_CTG")) %>%
  select(HDONG_CD, pm25_CTG, val_Dust_HDONG = Estimate)
```

``` r
SK_Age_pm25_coef <-  glm_SK_Age_Gamma_Hol_HDCTG$coefficients["avg_pm25"]


tibble(HDONG_CD = rep(HDong_CD$HDONG_CD, each = 6), 
       pm25_CTG = rep(Dust_level, 36)) %>%
  left_join(SK_Age_Main_Eff_Dust, by = "pm25_CTG") %>%
  left_join(SK_Age_Main_Eff_HDONG, by = "HDONG_CD") %>%
  left_join(SK_Age_Dust_HDONG, by = c("HDONG_CD", "pm25_CTG")) %>%
  mutate_all(replace_na, replace = 0) %>%
  mutate(Correction = case_when(pm25_CTG == "Good" ~ 10, 
                                pm25_CTG == "Moderate" ~ 20, 
                                pm25_CTG == "Sens_Unhealthy" ~ 30, 
                                pm25_CTG == "Unhealthy" ~ 43, 
                                pm25_CTG == "Very Unhealthy" ~ 60, 
                                pm25_CTG == "Worst" ~ 115), 
         Estimate = val_Dust + val_HDONG + val_Dust_HDONG + SK_Age_pm25_coef * Correction) %>%
  select(HDONG_CD, pm25_CTG, Estimate) %>%
  left_join(HDONG_map, by = "HDONG_CD") %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Estimate), color = "black") +
  scale_fill_gradient2(low = "red", high = "blue") +
  facet_wrap(~pm25_CTG) 
```

![](Inferences_files/figure-markdown_github/HDONG_CD%20and%20pm25_CTG%20(Age)-1.png)

``` r
tibble(HDONG_CD = rep(HDong_CD$HDONG_CD, each = 6), 
       pm25_CTG = rep(Dust_level, 36)) %>%
  left_join(SK_Age_Main_Eff_Dust, by = "pm25_CTG") %>%
  left_join(SK_Age_Main_Eff_HDONG, by = "HDONG_CD") %>%
  left_join(SK_Age_Dust_HDONG, by = c("HDONG_CD", "pm25_CTG")) %>%
  mutate_all(replace_na, replace = 0) %>%
  mutate(Correction = case_when(pm25_CTG == "Good" ~ 10, 
                                pm25_CTG == "Moderate" ~ 20, 
                                pm25_CTG == "Sens_Unhealthy" ~ 30, 
                                pm25_CTG == "Unhealthy" ~ 43, 
                                pm25_CTG == "Very Unhealthy" ~ 60, 
                                pm25_CTG == "Worst" ~ 115), 
         Estimate = val_Dust + val_Dust_HDONG + SK_Age_pm25_coef * Correction) %>%
  select(HDONG_CD, pm25_CTG, Estimate) %>%
  filter(pm25_CTG != "Good") %>% 
  left_join(HDONG_map, by = "HDONG_CD") %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Estimate), color = "black") +
  scale_fill_gradient2(low = "red", high = "blue") +
  facet_wrap(~pm25_CTG) 
```

![](Inferences_files/figure-markdown_github/HDONG_CD%20and%20pm25_CTG(Age,%20exclude%20HDONG_CD)-1.png)

-   Correction 값으로는 pm25\_CTG 별 avg\_pm25의 평균 값 사용

``` r
tibble(pm10_CTG = rep(Dust_level, 2), 
       Holiday = rep(c(0, 1), each = 6) %>% as.character) %>%
  left_join(summary_SK_Age %>%
              filter(Variable %>% str_starts("pm10")) %>%
              filter(p_val <= 0.05) %>%
              separate(Variable, into = c("pm10_CTG", "Holiday"), sep = ":") %>%
              mutate(pm10_CTG = pm10_CTG %>% str_remove("pm10_CTG"), 
                     Holiday = Holiday %>% str_remove("Holiday")), 
            by = c("pm10_CTG", "Holiday")) %>%
  select(pm10_CTG, Holiday, Estimate) %>%
  mutate_all(replace_na, replace = 0) %>%
  mutate(Estimate_Hol = ifelse(Holiday == 1, 
                               summary_SK_Age %>% filter(Variable == "Holiday1") %>% .$Estimate, 0), 
         Estimate = Estimate + Estimate_Hol) %>%
  ggplot() +
  geom_line(aes(x = pm10_CTG, y = Estimate, group = Holiday, color = Holiday))
```

![](Inferences_files/figure-markdown_github/pm10_CTG%20and%20Holiday%20(SK%20Age)-1.png)

``` r
tibble(Sex = rep(c("MAN", "WMAN"), each = 6), 
       pm25_CTG = rep(Dust_level, 2)) %>%
  left_join(SK_Age_Main_Eff_Dust, by = "pm25_CTG") %>%
  mutate_all(replace_na, 0) %>%
  mutate(val_Sex = rep(c(0, summary_SK_Age %>% filter(Variable == "SexWMAN") %>% 
                                .$Estimate), each = 6),
         Correction = case_when(pm25_CTG == "Good" ~ 10, 
                                pm25_CTG == "Moderate" ~ 20, 
                                pm25_CTG == "Sens_Unhealthy" ~ 30, 
                                pm25_CTG == "Unhealthy" ~ 43, 
                                pm25_CTG == "Very Unhealthy" ~ 60, 
                                pm25_CTG == "Worst" ~ 115), 
         Estimate = val_Dust + val_Sex + SK_Age_pm25_coef * Correction) %>%
  ggplot() +
  geom_line(aes(x = pm25_CTG, y = Estimate, group = Sex, color = Sex))
```

![](Inferences_files/figure-markdown_github/pm25_CTG%20and%20Sex-1.png)

``` r
SK_Age_Main_Eff_AGE_CTG <- summary_SK_Age %>%
  filter(p_val <= 0.05, 
         Variable %>% str_starts("AGE_CTG"), 
         !Variable %>% str_detect("Holiday|pm25_CTG")) %>%
  mutate(AGE_CTG = str_remove(Variable, "AGE_CTG")) %>%
  select(AGE_CTG, val_AGE = Estimate)

SK_Age_Dust_Age <- summary_SK_Age %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("AGE_CTG"), 
         Variable %>% str_detect("pm25_CTG")) %>%
  separate(Variable, into = c("AGE_CTG", "pm25_CTG"), sep = ":") %>%
  mutate(AGE_CTG = AGE_CTG %>% str_remove("AGE_CTG"), 
         pm25_CTG = pm25_CTG %>% str_remove("pm25_CTG")) %>%
  select(AGE_CTG, pm25_CTG, val_Age_Dust = Estimate)


tibble(AGE_CTG = rep(c("Youth", "Rising", "Middle", "Senior"), 6), 
       pm25_CTG = rep(Dust_level, each = 4)) %>%
  left_join(SK_Age_Dust_Age, by = c("AGE_CTG", "pm25_CTG")) %>%
  left_join(SK_Age_Main_Eff_Dust, by = "pm25_CTG", suffix = c("_Inter", "_Dust")) %>%
  left_join(SK_Age_Main_Eff_AGE_CTG, by = "AGE_CTG") %>%
  mutate_all(replace_na, replace = 0) %>%
  mutate(Correction = case_when(pm25_CTG == "Good" ~ 10, 
                                pm25_CTG == "Moderate" ~ 20, 
                                pm25_CTG == "Sens_Unhealthy" ~ 30, 
                                pm25_CTG == "Unhealthy" ~ 43, 
                                pm25_CTG == "Very Unhealthy" ~ 60, 
                                pm25_CTG == "Worst" ~ 115), 
         Estimate = val_Dust + val_Age_Dust + SK_Age_pm25_coef * Correction) %>%
  ggplot() +
  geom_line(aes(x = pm25_CTG, y = Estimate, group = AGE_CTG, color = AGE_CTG)) 
```

![](Inferences_files/figure-markdown_github/pm25_CTG%20and%20AGE_CTG-1.png)

2) SK\_Time
-----------

``` r
summary_SK_Time <- summary(glm_SK_Time_Gamma_Hol_HD_CTG)$coefficients %>% data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  tbl_df %>%
  `names<-`(value = c("Variable", "Estimate", "Std_Er", "t_val", "p_val"))
```

``` r
SK_Time_Main_Eff_Week_Day <- summary_SK_Time %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("Week_Day"), 
         !Variable %>% str_detect("pm25_CTG|Holiday")) %>%
  mutate(Week_Day = Variable %>% str_remove("Week_Day")) %>%
  select(Week_Day, val_Week = Estimate)

SK_Time_Main_Eff_Dust <- summary_SK_Time %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("pm25_CTG"), 
         !Variable %>% str_detect("Holiday|HDONG_CD|Week_Day|Time_Group")) %>%
  mutate(pm25_CTG = Variable %>% str_remove("pm25_CTG")) %>%
  select(pm25_CTG, val_Dust = Estimate)

SK_Time_Main_Eff_Hol <- summary_SK_Time %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("Holiday"), 
         !Variable %>% str_detect("pm10_CTG|pm25_CTG|Week_Day|Time_Group|Wrn")) %>%
  mutate(Holiday = Variable %>% str_remove("Holiday")) %>%
  select(Holiday, val_Hol = Estimate)

SK_Time_Week_pm25_CTG <- summary_SK_Time %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("Week_Day"), 
         Variable %>% str_detect("pm25_CTG"), 
         !Variable %>% str_detect("Holiday")) %>%
  separate(Variable, into = c("Week_Day", "pm25_CTG"), sep = ":") %>%
  mutate(Week_Day = Week_Day %>% str_remove("Week_Day"), 
         pm25_CTG = pm25_CTG %>% str_remove("pm25_CTG")) %>%
  select(Week_Day, pm25_CTG, val_Week_pm25 = Estimate)


SK_Time_Week_Holiday <- summary_SK_Time %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("Week_Day"), 
         Variable %>% str_detect("Holiday"), 
         !Variable %>% str_detect("pm25_CTG")) %>%
  separate(Variable, into = c("Week_Day", "Holiday"), sep = ":") %>%
  mutate(Week_Day = Week_Day %>% str_remove("Week_Day"), 
         Holiday = Holiday %>% str_remove("Holiday")) %>%
  select(Week_Day, Holiday, val_Week_Hol = Estimate)

SK_Time_pm25_CTG_Hol <- summary_SK_Time %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("Holiday"), 
         Variable %>% str_detect("pm25_CTG"), 
         !Variable %>% str_detect("Week_Day")) %>%
  separate(Variable, into = c("pm25_CTG", "Holiday"), sep = ":") %>%
  mutate(pm25_CTG = pm25_CTG %>% str_remove("pm25_CTG"), 
         Holiday = Holiday %>% str_remove("Holiday")) %>%
  select(pm25_CTG, Holiday, val_pm25_Hol = Estimate)

SK_Time_inter_three <- summary_SK_Time %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("pm25_CTG"), 
         Variable %>% str_detect("Holiday"), 
         Variable %>% str_detect("Week_Day")) %>%
  separate(Variable, into = c("Week_Day", "pm25_CTG", "Holiday"), sep = ":") %>%
  mutate(Week_Day = Week_Day %>% str_remove("Week_Day"), 
         pm25_CTG = pm25_CTG %>% str_remove("pm25_CTG"), 
         Holiday = Holiday %>% str_remove("Holiday")) %>%
  select(Week_Day, pm25_CTG, Holiday, val_inter = Estimate)

SK_Time_Main_Eff_Time_Gr <- summary_SK_Time %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("Time_Group"), 
         !Variable %>% str_detect("Holiday|pm25_CTG")) %>%
  mutate(Time_Group = Variable %>% str_remove("Time_Group")) %>%
  select(Time_Group, val_Time_GR = Estimate)

SK_Time_Gr_Hol <- summary_SK_Time %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("Time_Group"), 
         Variable %>% str_detect("Holiday")) %>%
  separate(Variable, into = c("Holiday", "Time_Group"), sep = ":") %>%
  mutate(Holiday = Holiday %>% str_remove("Holiday"), 
         Time_Group = Time_Group %>% str_remove("Time_Group")) %>%
  select(Holiday, Time_Group, val_Time_Hol = Estimate)

SK_Time_Gr_pm25 <- summary_SK_Time %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("Time_Group"), 
         Variable %>% str_detect("pm25_CTG")) %>%
  separate(Variable, into = c("pm25_CTG", "Time_Group"), ":") %>%
  mutate(pm25_CTG = pm25_CTG %>% str_remove("pm25_CTG"), 
         Time_Group = Time_Group %>% str_remove("Time_Group")) %>%
  select(pm25_CTG, Time_Group, val_Time_pm25 = Estimate)

SK_Time_Main_Eff_HDONG_CD  <- summary_SK_Time %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("HDONG_CD"), 
         !Variable %>% str_detect("pm25_CTG")) %>%
  mutate(HDONG_CD = parse_number(Variable) %>% as.character) %>%
  select(HDONG_CD, val_HDONG_CD = Estimate)

SK_Time_HDONG_Dust <- summary_SK_Time %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("HDONG_CD"), 
         Variable %>% str_detect("pm25_CTG")) %>%
  separate(Variable, into = c("HDONG_CD", "pm25_CTG"), sep = ":") %>%
  mutate(HDONG_CD = HDONG_CD %>% str_remove("HDONG_CD"), 
         pm25_CTG = pm25_CTG %>% str_remove("pm25_CTG")) %>%
  select(HDONG_CD, pm25_CTG, val_HDONG_Dust = Estimate)

SK_Time_Main_Eff_Wrn <- summary_SK_Time %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("pm25_Wrn"), 
         !Variable %>% str_detect("Holiday")) %>%
  mutate(pm25_Wrn = Variable %>% str_remove("pm25_Wrn")) %>%
  select(pm25_Wrn, val_Wrn = Estimate)

SK_Time_Wrn_Hol <- summary_SK_Time %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("pm25_Wrn"), 
         Variable %>% str_detect("Holiday")) %>%
  separate(Variable, into = c("pm25_Wrn", "Holiday"), sep = ":") %>%
  mutate(pm25_Wrn = pm25_Wrn %>% str_remove("pm25_Wrn"), 
         Holiday = Holiday %>% str_remove("Holiday") %>% as.character) %>%
  select(pm25_Wrn, Holiday, val_Wrn_Hol = Estimate)
```

``` r
Week_Day_lev <- c("Fri", "Sat", "Sun", "Mon", "Tue", "Wed", "Thu")
SK_Time_pm25_coef <- glm_SK_Time_Gamma_Hol_HD_CTG$coefficients["avg_pm25"]

SK_Time_coef <- tibble(Week_Day = rep(Week_Day_lev, each = 12), 
       pm25_CTG = rep(Dust_level,7) %>% rep(each = 2), 
       Holiday = rep(c(0, 1), 42) %>% as.character) %>%
  left_join(SK_Time_Main_Eff_Week_Day, by = "Week_Day") %>%
  left_join(SK_Time_Main_Eff_Dust, by = "pm25_CTG") %>%
  left_join(SK_Time_Main_Eff_Hol, by = "Holiday") %>%
  left_join(SK_Time_Week_Holiday, by = c("Week_Day", "Holiday")) %>%
  left_join(SK_Time_Week_pm25_CTG, by = c("Week_Day", "pm25_CTG")) %>%
  left_join(SK_Time_pm25_CTG_Hol, by = c("pm25_CTG", "Holiday")) %>%
  left_join(SK_Time_inter_three, by = c("Week_Day", "pm25_CTG", "Holiday")) %>%
  mutate_all(replace_na, 0) %>%
  mutate(Week_Day = Week_Day %>% factor(levels = Week_Day_lev))
  
SK_Time_coef %>%
  mutate(Correction = case_when(pm25_CTG == "Good" ~ 10, 
                                pm25_CTG == "Moderate" ~ 20, 
                                pm25_CTG == "Sens_Unhealthy" ~ 30, 
                                pm25_CTG == "Unhealthy" ~ 43, 
                                pm25_CTG == "Very Unhealthy" ~ 60, 
                                pm25_CTG == "Worst" ~ 115), 
    Estimate = val_Week + val_Dust + val_Hol + val_Week_Hol + val_Week_pm25 + val_pm25_Hol + 
      val_inter + SK_Time_pm25_coef * Correction) %>%
  select(Week_Day, pm25_CTG, Holiday, Estimate) %>%
  filter(!(Holiday == "1"& Week_Day != "Sat"& Week_Day != "Sun")) %>%
  ggplot() +
  geom_line(aes(x = pm25_CTG, y = Estimate, group = Week_Day, color = Week_Day)) +
  facet_wrap(~Holiday) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](Inferences_files/figure-markdown_github/Holiday_pm25_CTG%20and%20Week_Day-1.png)

``` r
SK_Time_coef %>%
  mutate(Estimate =  val_Week + val_Hol + val_Week_Hol) %>%
  select(Week_Day, Holiday, Estimate) %>% unique %>%
  ggplot() +
  geom_line(aes(x = Holiday, y = Estimate, group = Week_Day, color = Week_Day))
```

![](Inferences_files/figure-markdown_github/Holiday%20and%20Week_Day-1.png)

-   평일 중 공휴일은 1년 중에서 15일

-   Holiday 에서는 토, 일만 그렸다.

-   Tue, Wed, Thu, Fri 는 Interaction값 동일!

``` r
tibble(Time_Group = rep(c("Afternoon", "Dawn", "Evening", "Rush_Hour"), each = 2), 
       Holiday = rep(c(0, 1), 4) %>% as.character) %>%
  left_join(SK_Time_Main_Eff_Time_Gr, by = "Time_Group") %>%
  left_join(SK_Time_Main_Eff_Hol, by = "Holiday") %>%
  left_join(SK_Time_Gr_Hol, by = c("Time_Group", "Holiday")) %>%
  mutate_all(replace_na, 0) %>%
  mutate(Estimate = val_Time_GR + val_Hol + val_Time_Hol) %>%
  select(Time_Group, Holiday, Estimate) %>%
  ggplot() +
  geom_line(aes(x = Holiday, y = Estimate, group = Time_Group, color = Time_Group))
```

![](Inferences_files/figure-markdown_github/Holiday%20and%20Time_Group-1.png)

``` r
tibble(Time_Group = rep(c("Afternoon", "Dawn", "Evening", "Rush_Hour"), each = 6), 
       pm25_CTG = rep(Dust_level, 4)) %>%
  left_join(SK_Time_Main_Eff_Time_Gr, by = "Time_Group") %>%
  left_join(SK_Time_Main_Eff_Dust, by = "pm25_CTG") %>%
  left_join(SK_Time_Gr_pm25, by = c("Time_Group", "pm25_CTG")) %>%
  mutate_all(replace_na, 0) %>%
  mutate(Correction = case_when(pm25_CTG == "Good" ~ 10, 
                                pm25_CTG == "Moderate" ~ 20, 
                                pm25_CTG == "Sens_Unhealthy" ~ 30, 
                                pm25_CTG == "Unhealthy" ~ 43, 
                                pm25_CTG == "Very Unhealthy" ~ 60, 
                                pm25_CTG == "Worst" ~ 115), 
    Estimate = val_Dust + val_Time_pm25 + SK_Time_pm25_coef * Correction)  %>%
  select(Time_Group, pm25_CTG, Estimate) %>%
  ggplot() +
  geom_line(aes(x = pm25_CTG, y = Estimate, group = Time_Group, color = Time_Group))
```

![](Inferences_files/figure-markdown_github/pm25_CTG%20and%20Time_Group-1.png)

``` r
tibble(HDONG_CD = rep(HDong_CD$HDONG_CD, each = 6), 
       pm25_CTG = rep(Dust_level, 36)) %>%
  left_join(SK_Time_Main_Eff_HDONG_CD, by = "HDONG_CD") %>%
  left_join(SK_Time_Main_Eff_Dust, by = "pm25_CTG") %>%
  left_join(SK_Time_HDONG_Dust, by = c("HDONG_CD", "pm25_CTG")) %>%
  mutate_all(replace_na, 0) %>%
  mutate(Correction = case_when(pm25_CTG == "Good" ~ 10, 
                                pm25_CTG == "Moderate" ~ 20, 
                                pm25_CTG == "Sens_Unhealthy" ~ 30, 
                                pm25_CTG == "Unhealthy" ~ 43, 
                                pm25_CTG == "Very Unhealthy" ~ 60, 
                                pm25_CTG == "Worst" ~ 115), 
         Estimate = val_HDONG_CD + val_Dust + val_HDONG_Dust + 
           SK_Time_pm25_coef * Correction) %>%
  select(HDONG_CD, pm25_CTG, Estimate) %>%
  left_join(HDONG_map, by = "HDONG_CD") %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Estimate), color = "black") +
  scale_fill_gradient2(low = "red", high = "blue") +
  facet_wrap(~pm25_CTG)
```

![](Inferences_files/figure-markdown_github/HDONG_CD%20and%20pm25_CTG%20(Time)-1.png)

``` r
tibble(HDONG_CD = rep(HDong_CD$HDONG_CD, each = 6), 
       pm25_CTG = rep(Dust_level, 36)) %>%
  left_join(SK_Time_Main_Eff_HDONG_CD, by = "HDONG_CD") %>%
  left_join(SK_Time_Main_Eff_Dust, by = "pm25_CTG") %>%
  left_join(SK_Time_HDONG_Dust, by = c("HDONG_CD", "pm25_CTG")) %>%
  mutate_all(replace_na, 0) %>%
  mutate(Correction = case_when(pm25_CTG == "Good" ~ 10, 
                                pm25_CTG == "Moderate" ~ 20, 
                                pm25_CTG == "Sens_Unhealthy" ~ 30, 
                                pm25_CTG == "Unhealthy" ~ 43, 
                                pm25_CTG == "Very Unhealthy" ~ 60, 
                                pm25_CTG == "Worst" ~ 115), 
         Estimate = val_Dust + val_HDONG_Dust +
           SK_Time_pm25_coef * Correction) %>%
  select(HDONG_CD, pm25_CTG, Estimate) %>%
  filter(pm25_CTG != "Good") %>%
  left_join(HDONG_map, by = "HDONG_CD") %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Estimate), color = "black") +
  scale_fill_gradient2(low = "red", high = "blue") +
  facet_wrap(~pm25_CTG)
```

![](Inferences_files/figure-markdown_github/HDONG_CD%20and%20pm25_CTG(Time,%20Exclude%20HDONG%20Effect)-1.png)

``` r
Age_seq <- seq(0, 100, by = 5)
JONGNO_GU <- read_excel("Report_Population.xls", range = "C8:C58", col_names = "HDONG_NM") %>% 
  pull() %>% unique
NOWON_GU <- read_excel("Report_Population.xls", range = "C509:C565", col_names = "HDONG_NM") %>% 
  pull() %>% unique

People_Jongno <- read_excel("Report_Population.xls", range = "D1:Z58", col_types = "text") %>%
  `names<-`(value = c("Type", "Total", paste0("Age_", Age_seq))) %>%
  filter(Type == "계")
People_Jongno <- People_Jongno[-(1:2), ] %>%
  mutate(Type = JONGNO_GU)
People_Nowon <- read_excel("Report_Population.xls", range = "D509:Z565", col_names = FALSE, 
                           col_types = "text") %>%
  `names<-`(value = c("Type", "Total", paste0("Age_", Age_seq))) %>%
  filter(Type == "계") %>%
  mutate(Type = NOWON_GU)

People <- bind_rows(People_Jongno, People_Nowon) %>%
  mutate(Type = str_replace_all(Type, pattern = "·", replacement = ".")) %>%
  left_join(y = HDong_CD, by = c("Type" = "HDONG_NM"))
```

``` r
Target_HDONG <- c("11350619", "11350600", "11110640", "11110680", "11110550", "11350560")
People %>%
  gather(key = "AGE", value = "value", -Type, -Total, -HDONG_CD, -GU_CD, -GU_NM) %>%
  select(HDONG_CD, AGE, value) %>%
  mutate(AGE = parse_number(AGE), value = as.numeric(value)) %>%
  filter(HDONG_CD %in% Target_HDONG) %>%
  mutate(Type = ifelse(HDONG_CD %in% c(11110550, 11350560), "-", "+"))%>%
  left_join(HDong_CD, by = "HDONG_CD") %>%
  ggplot() +
  geom_col(aes(x = AGE, y = value, fill = Type), color = "white") +
  facet_wrap(~HDONG_NM, scales = "free") +
  ggtitle("Population for Suspicious HDONG")
```

11350619 (중계본동) (45, 50 대가 제일 많다.) 11350600 (공릉2동) (20, 45,
50 대가 제일 많다.) 11110640(이화동) 11110680(창신2동) \*\*\* 11110550
(부암동) 11350560(월계1동)

``` r
tibble(pm25_Wrn = rep(Wrn_level, each = 2), 
       Holiday = rep(c(0, 1), 3) %>% as.character) %>%
  left_join(SK_Time_Main_Eff_Wrn, by = "pm25_Wrn") %>%
  left_join(SK_Time_Main_Eff_Hol, by = "Holiday") %>%
  left_join(SK_Time_Wrn_Hol, by = c("pm25_Wrn", "Holiday")) %>%
  mutate_all(replace_na, 0) %>%
  mutate(pm25_Wrn = factor(pm25_Wrn, levels = Wrn_level), 
         Correction = case_when(pm25_Wrn == "No_Wrn" ~ 0,
                                pm25_Wrn == "Warning" ~ 75, 
                                pm25_Wrn == "Dust_Watch" ~ 150), 
         Estimate = val_Wrn + val_Hol + val_Wrn_Hol + SK_Time_pm25_coef * Correction) %>%
  ggplot() +
  geom_line(aes(x = pm25_Wrn, y = Estimate, group = Holiday, color = Holiday))
```

![](Inferences_files/figure-markdown_github/Holiday%20and%20pm25_Wrn-1.png)

3) Dist
-------

### (1) ADMD\_CD

Use Gamma-glm

``` r
summary_glm_Dist <- summary(glm_Dist_Best)$coefficients %>% data.frame() %>%
  rownames_to_column(var = "Variable") %>%
  tbl_df %>%
  `names<-`(value = c("Variable", "Estimate", "Std_Er", "t_val", "p_val"))
```

``` r
glm_Dist_Main_Eff_pm25 <- summary_glm_Dist %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("pm25_CTG"), 
         !Variable %>% str_detect("Holiday")) %>%
  mutate(pm25_CTG = Variable %>% str_remove("pm25_CTG")) %>%
  select(pm25_CTG, val_pm25 = Estimate)

glm_Dist_Main_Eff_Hol <- summary_glm_Dist %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("Holiday"), 
         !Variable %>% str_detect(":")) %>%
  mutate(Holiday = Variable %>% str_remove("Holiday")) %>%
  select(Holiday, val_Hol = Estimate)

glm_Dist_pm25_Hol <- summary_glm_Dist %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("pm25_CTG"), 
         Variable %>% str_detect("Holiday")) %>%
  separate(Variable, into = c("pm25_CTG", "Holiday"), ":") %>%
  mutate(pm25_CTG = pm25_CTG %>% str_remove("pm25_CTG"), 
         Holiday = Holiday %>% str_remove("Holiday")) %>%
  select(pm25_CTG, Holiday, val_pm25_Hol = Estimate)

glm_Dist_pm10_Hol <- summary_glm_Dist %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("pm10_CTG"), 
         Variable %>% str_detect("Holiday")) %>%
  separate(Variable, into = c("pm10_CTG", "Holiday"), sep = ":") %>%
  mutate(pm10_CTG = pm10_CTG %>% str_remove("pm10_CTG"), 
         Holiday = Holiday %>% str_remove("Holiday")) %>%
  select(pm10_CTG, Holiday, val_pm10_Hol = Estimate)

glm_Dist_pm10_Wrn <- summary_glm_Dist %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("pm10_Wrn"), 
         !Variable %>% str_detect("Holiday")) %>%
  mutate(pm10_Wrn = Variable %>% str_remove("pm10_Wrn")) %>%
  select(pm10_Wrn, val_pm10_Wrn = Estimate)

glm_Dist_pm10_Wrn_Hol <- summary_glm_Dist %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("pm10_Wrn"), 
         Variable %>% str_detect("Holiday")) %>%
  separate(Variable, into = c("pm10_Wrn", "Holiday"), sep = ":") %>%
  mutate(pm10_Wrn = pm10_Wrn %>% str_remove("pm10_Wrn"), 
         Holiday = Holiday %>% str_remove("Holiday")) %>%
  select(pm10_Wrn, Holiday, val_pm10_Wrn_Hol = Estimate)

glm_Dist_Main_Eff_Mon <- summary_glm_Dist %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("Month"),
         !Variable %>% str_detect(":")) %>%
  mutate(Month = Variable %>% str_remove("Month")) %>%
  select(Month, val_Mon = Estimate)

glm_Dist_Mon_pm10_CTG <- summary_glm_Dist %>%
  filter(p_val <= 0.05, 
         Variable %>% str_detect("Month"),
         Variable %>% str_detect(":")) %>%
  separate(Variable, into = c("Month", "pm10_CTG"), sep = ":") %>%
  mutate(Month = Month %>% str_remove("Month"), 
         pm10_CTG = pm10_CTG %>% str_remove("pm10_CTG")) %>%
  select(Month, pm10_CTG, val_Mon_pm10 = Estimate)

glm_Dist_Main_Eff_HDONG <- summary_glm_Dist %>% 
  filter(p_val <= 0.05, 
         Variable %>% str_detect("ADMD_CD"), 
         !Variable %>% str_detect(":")) %>%
  mutate(HDONG_CD = Variable %>% str_remove("ADMD_CD")) %>%
  select(HDONG_CD, val_HDONG = Estimate)

glm_Dist_HDONG_pm10_CTG <- summary_glm_Dist %>% 
  filter(p_val <= 0.05, 
         Variable %>% str_detect("ADMD_CD"), 
         Variable %>% str_detect(":")) %>%
  separate(Variable, into = c("HDONG_CD", "pm10_CTG"), sep = ":") %>%
  mutate(HDONG_CD = HDONG_CD %>% str_remove("ADMD_CD"), 
         pm10_CTG = pm10_CTG %>% str_remove("pm10_CTG")) %>%
  select(HDONG_CD, pm10_CTG, val_HDONG_pm10 = Estimate)
```

-   pm10\_CTG 의 Main\_Eff 는 없음!

``` r
tibble(pm25_CTG = rep(Dust_level, each = 2), 
       Holiday = rep(c(0, 1), 6) %>% as.character) %>%
  left_join(glm_Dist_Main_Eff_pm25, by = "pm25_CTG") %>%
  left_join(glm_Dist_Main_Eff_Hol, by = "Holiday") %>%
  left_join(glm_Dist_pm25_Hol, by = c("pm25_CTG", "Holiday")) %>%
  mutate_all(replace_na, 0) %>%
  mutate(Estimate = val_pm25 + val_Hol + val_pm25_Hol) %>%
  ggplot() +
  geom_line(aes(x = pm25_CTG, y = Estimate, group = Holiday, color = Holiday))
```

![](Inferences_files/figure-markdown_github/pm25_CTG%20and%20Holiday-1.png)

-   avg\_pm25 없음…

``` r
glm_Dist_pm10_coef <- summary_glm_Dist %>% filter(Variable == "avg_pm10") %>% .$Estimate

tibble(pm10_CTG = rep(Dust_level, each = 2), 
       Holiday = rep(c(0, 1), 6) %>% as.character) %>%
  left_join(glm_Dist_Main_Eff_Hol, by = "Holiday") %>%
  left_join(glm_Dist_pm10_Hol, by = c("pm10_CTG", "Holiday")) %>%
  mutate_all(replace_na, 0) %>%
  mutate(Correction = case_when(pm10_CTG == "Good" ~ 22, 
                                pm10_CTG == "Moderate" ~ 40, 
                                pm10_CTG == "Sens_Unhealthy" ~ 61, 
                                pm10_CTG == "Unhealthy" ~ 86, 
                                pm10_CTG == "Very Unhealthy" ~ 117, 
                                pm10_CTG == "Worst" ~ 207), 
         Estimate = val_Hol + val_pm10_Hol + glm_Dist_pm10_coef * Correction) %>%
  ggplot() +
  geom_line(aes(x = pm10_CTG, y = Estimate, group = Holiday, color = Holiday))
```

![](Inferences_files/figure-markdown_github/pm10_CTG%20and%20Holiday%20(Dist)-1.png)

``` r
tibble(pm10_Wrn = rep(Wrn_level, each = 2), 
       Holiday = rep(c(0, 1), 3) %>% as.character) %>%
  left_join(glm_Dist_Main_Eff_Hol, by = "Holiday") %>%
  left_join(glm_Dist_pm10_Wrn_Hol, by = c("pm10_Wrn", "Holiday")) %>%
  mutate_all(replace_na, 0) %>%
  mutate(pm10_Wrn = factor(pm10_Wrn, levels = Wrn_level), 
         Correction = case_when(pm10_Wrn == "No_Wrn" ~ 0, 
                                pm10_Wrn == "Warning" ~ 150, 
                                pm10_Wrn == "Dust_Watch" ~ 300), 
         Estimate = val_Hol + val_pm10_Wrn_Hol + glm_Dist_pm10_coef * Correction) %>%
  ggplot() +
  geom_line(aes(x = pm10_Wrn, y = Estimate, group = Holiday, color = Holiday))
```

![](Inferences_files/figure-markdown_github/pm10_Wrn%20and%20Holiday-1.png)

``` r
tibble(Month = rep(c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"),
                   each = 6), 
       pm10_CTG = rep(Dust_level, 12)) %>%
  left_join(glm_Dist_Main_Eff_Mon, by = "Month") %>%
  left_join(glm_Dist_Mon_pm10_CTG, by = c("Month", "pm10_CTG")) %>%
  mutate_all(replace_na, 0) %>%
  mutate(Correction = case_when(pm10_CTG == "Good" ~ 22, 
                                pm10_CTG == "Moderate" ~ 40, 
                                pm10_CTG == "Sens_Unhealthy" ~ 61, 
                                pm10_CTG == "Unhealthy" ~ 86, 
                                pm10_CTG == "Very Unhealthy" ~ 117, 
                                pm10_CTG == "Worst" ~ 207), 
         Estimate = val_Mon + val_Mon_pm10 + glm_Dist_pm10_coef * Correction) %>%
  ggplot() +
  geom_line(aes(x = pm10_CTG, y = Estimate, group = Month, color = Month))
```

![](Inferences_files/figure-markdown_github/pm10_CTG%20and%20Month-1.png)

``` r
tibble(HDONG_CD = rep(HDong_CD$HDONG_CD, each = 6), 
       pm10_CTG = rep(Dust_level, 36)) %>%
  left_join(glm_Dist_Main_Eff_HDONG, by = "HDONG_CD") %>%
  left_join(glm_Dist_HDONG_pm10_CTG, by = c("HDONG_CD", "pm10_CTG")) %>%
  mutate_all(replace_na, 0) %>%
  mutate(Correction = case_when(pm10_CTG == "Good" ~ 22, 
                                pm10_CTG == "Moderate" ~ 40, 
                                pm10_CTG == "Sens_Unhealthy" ~ 61, 
                                pm10_CTG == "Unhealthy" ~ 86, 
                                pm10_CTG == "Very Unhealthy" ~ 117, 
                                pm10_CTG == "Worst" ~ 207), 
         Estimate = val_HDONG + val_HDONG_pm10 + glm_Dist_pm10_coef * Correction) %>%
  left_join(HDONG_map, by = "HDONG_CD") %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Estimate), color = "black") +
  scale_fill_gradient2(low = "red", high = "blue") +
  facet_wrap(~pm10_CTG)
```

![](Inferences_files/figure-markdown_github/ADMD_CD%20and%20pm10_CTG-1.png)

``` r
tibble(HDONG_CD = rep(HDong_CD$HDONG_CD, each = 6), 
       pm10_CTG = rep(Dust_level, 36)) %>%
  left_join(glm_Dist_Main_Eff_HDONG, by = "HDONG_CD") %>%
  left_join(glm_Dist_HDONG_pm10_CTG, by = c("HDONG_CD", "pm10_CTG")) %>%
  mutate_all(replace_na, 0) %>%
  mutate(Correction = case_when(pm10_CTG == "Good" ~ 22, 
                                pm10_CTG == "Moderate" ~ 40, 
                                pm10_CTG == "Sens_Unhealthy" ~ 61, 
                                pm10_CTG == "Unhealthy" ~ 86, 
                                pm10_CTG == "Very Unhealthy" ~ 117, 
                                pm10_CTG == "Worst" ~ 207), 
         Estimate = val_HDONG_pm10 + glm_Dist_pm10_coef * Correction) %>%
  filter(pm10_CTG != "Good") %>%
  left_join(HDONG_map, by = "HDONG_CD") %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Estimate), color = "black") +
  scale_fill_gradient2(low = "red", high = "blue") +
  facet_wrap(~pm10_CTG)
```

![](Inferences_files/figure-markdown_github/HDONG_CD%20and%20pm10_CTG(Exclude%20HDONG%20effect)-1.png)
