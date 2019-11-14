library(rio)
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(hrbrthemes)
library(inspectdf)
library(gghighlight)
library(grid)
library(ggrepel)
library(gganimate)

dt <- import("raw_data/b_sao_ke2.csv") %>% rename_all(tolower)
reg <- import("ref/MA_CN_QTRR.xlsx") %>% rename_all(tolower)

names(dt) %>% intersect(names(reg))

dt %>% class()

dt[, "gbbb1"] %>% head()
dt[, "gbbb2"] %>% head()

# Filter ko phai MBN
dt %<>% filter(company_mbn == 'NO')

dt %<>% 
  mutate(dayid = mdy(dayid),
         value_date = mdy(value_date)) %>% 
  mutate(gbbb3 = case_when(year(value_date) == year(dayid) ~ 'NEW BOOK',
                           year(value_date) < 2015 ~ 'OLD BOOK < 2015',
                           year(value_date) >= 2015 ~ 'OLD BOOK >= 2015',
                           as.numeric(substr(contract, 5, 6)) + 2000 == year(dayid) ~ 'NEW BOOK',
                           as.numeric(substr(contract, 5, 6)) + 2000 < 2015 ~ 'OLD BOOK < 2015',
                           as.numeric(substr(contract, 5, 6)) + 2000 >= 2015 ~ 'OLD BOOK >= 2015',
                           TRUE ~ 'OLD BOOK < 2015'
  )) %>% 
  mutate(value_date = format(value_date, "%Y-%m")) %>% 
  rename(region_name_old = region_name,
         branch_name_old = branch_name) %>% 
  left_join(reg)

save(dt, file = "dt.rdata")

bal <- dt %>% 
  group_by(dayid) %>% 
  summarise(du_no = sum(du_no, na.rm = TRUE))


save(bal, file = "bal.rdata")


eval(parse("R/graph_pqr.R", encoding = "UTF-8"))

eval(parse("R/tmp.R", encoding = "UTF-8"))



load("dt.rdata")
    
