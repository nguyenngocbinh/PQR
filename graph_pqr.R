#--------------------------------------------------------------------------#
report_date <- ceiling_date(Sys.Date(), "month") + months(-1) - days(1)    #
#--------------------------------------------------------------------------#

# Tính chỉ tiêu dòng tiền (dư nợ)
#-----------------------------------------------------------------------------
## Tăng trưởng dư nợ qua các tháng
p <- dt %>% 
  group_by(dayid) %>% 
  summarise(du_no = sum(du_no, na.rm = TRUE)/1e9,
            nbr_contract = n(),
            nbr_customer = n_distinct(customer_code)) %>% 
  gather(key = "key", value = "value", -dayid)

p %>% 
  ggplot(aes(x = dayid, y = value))+
  geom_point(aes(color = key), alpha = 0.5)+
  geom_line(aes(color = key), alpha = 0.5)+
  geom_text_repel(aes(x = dayid, y = value, label = scales::comma(round(value, 0))), 
                  color = 'white',
                  data = p[p$dayid == report_date, ]
  )+
  scale_color_discrete(name = element_blank(),
                       breaks = c("du_no", "nbr_contract", "nbr_customer"),
                       labels = c("Dư nợ (tỷ đồng)", "Số hợp đồng", "Số khách hàng"))+
  labs(title = 'Tổng dư nợ, số hợp đồng, số khách hàng',
       y = NULL,
       x = 'Tháng')+
  theme_ft_rc(grid="XY", axis="xy")+
  scale_y_continuous(label=scales::comma)+
  theme(legend.position = "bottom")

ggsave("figures/g1.png", width = 7, height = 5)

#-----------------------------------------------------------------------------
## Tăng trưởng dư nợ theo region_name

f_ani <- function(cat){
  cat <- enquo(cat)	
  # custom axis length
  nc <- dt %>% pull(region_name) %>% nchar() %>% max(na.rm = TRUE)
  #
  agg <- dt %>% 
    mutate(yearmon = format(dayid, "%Y-%m")) %>% 
    group_by(!!cat, yearmon) %>% 
    summarise(du_no = sum(du_no, na.rm = TRUE)) %>% 
    filter(!!cat != "") %>% 
    group_by(yearmon) %>%
    # The * 1 makes it possible to have non-integer ranks while sliding
    mutate(value = round(du_no/1e9, 0),
           rank = min_rank(-value) * 1,
           value_lbl = paste0(" ", scales::comma(value))) %>%
    ungroup() 
  pp <- agg %>% 
    ggplot(aes(rank,
               group = !!cat, 
               fill = as.factor(!!cat), color = as.factor(!!cat))) +
    geom_tile(aes(y = value/2,
                  height = value,
                  width = 0.9), alpha = 0.8, color = NA)+
    geom_text(aes(y = 0, label = paste(!!cat, " ")), hjust = 1)+
    geom_text(aes(y = value, label = value_lbl, hjust = 0))+
    coord_flip(clip = "off", expand = FALSE)+
    scale_y_continuous(labels = scales::comma) +
    scale_x_reverse(labels = NULL)+
    guides(color = FALSE, fill = FALSE)+
    labs(title = "Tổng dư nợ theo tháng",
         subtitle='{closest_state}', x = "", y = "(tỷ đồng)")+
    theme_ft_rc()+
    theme(axis.ticks.length.y = unit(nc * 0.12,"cm"))+
    transition_states(yearmon, transition_length = 4, state_length = 1) +
    ease_aes('cubic-in-out')
  
  animate(pp, 
          fps = 25,
          duration = 40,
          width = 700,
          height = 500,
          renderer = gifski_renderer(paste0("figures/", rlang::quo_text(cat), ".gif")))
  
  return(pp)
}

p <- f_ani(region_name)
p <- f_ani(customer_type)

#-----------------------------------------------------------------------------
## Tăng trưởng dư nợ theo book
p <- dt %>% 
  group_by(dayid, gbbb2) %>% 
  summarise(du_no = sum(du_no, na.rm = TRUE)/1e9,
            nbr_contract = n()) 

p %>% 
  ggplot(aes(dayid, du_no))+
  geom_col(aes(fill = gbbb2, color = gbbb2), alpha = 0.3)+
  labs(title = 'Tổng dư nợ theo tháng và nhóm book',
       y = 'Dư nợ (tỷ đồng)',
       x = 'Tháng')+
  theme_ft_rc(grid="XY", axis="xy")+
  scale_y_continuous(label=scales::comma)+
  theme(legend.position = "bottom",
        legend.title = element_blank())


ggsave("figures/g3.png", width = 7, height = 5)

# 30-Oct-2019 ------------------------------
# Hàm xử lý outlier
f_outlier <- function(x){
  threshold <- quantile(x, probs = c(0.005, 0.95), na.rm = TRUE, type = 3)
  y <- case_when(x > threshold[2] ~ threshold[2],
          x < threshold[1] ~ threshold[1],
          TRUE ~ x) 
  return(y)
}

p <- dt %>% 
  filter(dayid == report_date) %>% 
  # group_by(customer_type) %>% 
  mutate(du_no_grp = cut(du_no/1e6, 
                         breaks = c(0, 10, 20, 30, 40, 50, 100, 200, 300, 400, 
                                    500, max(du_no/1e6, na.rm = TRUE)), 
                         include.lowest = TRUE)) %>% 
  group_by(du_no_grp) %>% 
  summarise(cnt = n())

p %>% ggplot(aes(du_no_grp, cnt))+
  geom_col()+
  labs(title = 'Số lượng hợp đồng theo các khoảng dư nợ',
       subtitle = paste0("Tháng ", format(report_date, "%m/%Y")),
       y = 'Số lượng HĐ',
       x = 'Dư nợ (triệu đồng)')+
  theme_ft_rc(grid="XY", axis="xy")+
  scale_y_continuous(label=scales::comma)+
  theme(axis.text.x = element_text(angle = 30))

ggsave("figures/g4.png", width = 7, height = 5)


# Đặc điểm danh mục khách hàng tháng 9/2019 ------------------------------
p <- dt %>% 
  filter(dayid == report_date) %>% # Change here
  select(customer_type, gbbb1, gbbb2, datasource, loans_term, region_name,
         product_group2, term, bucket, campaign_id) 


p %>% 
  inspect_cat() %>% 
  show_plot()+
  labs(title = "Cơ cấu số lượng hợp đồng",
       subtitle = paste("Tháng", format(report_date, "%m/%Y")))+
  theme_ft_rc()

ggsave("figures/g5.png", width = 7, height = 5)

#=============================================================================
# ĐẶC ĐIỂM QUÁ HẠN
#=============================================================================
# Tỷ lệ nợ xấu theo đầu khách hàng
p <- dt %>% 
  group_by(dayid, customer_code) %>% 
  summarise(max_ovd = max(no_days_overdue)) %>% 
  mutate(npl = case_when(max_ovd >= 91 ~ 1, TRUE ~ 0)) %>% 
  group_by(dayid) %>% 
  summarise(npl = mean(npl) * 100)

p %>% 
  ggplot(aes(dayid, npl))+
  geom_point()+
  geom_line()+
  geom_point(aes(dayid, npl), data = p[p$dayid == report_date,], color = 'red', size = 5, shape = 1)+
  geom_text(aes(dayid, npl, label = paste0(round(npl, 1), "%")), 
            data = p[p$dayid == report_date,], 
            color = 'red',
            hjust = 1.2)+
  labs(title = 'Tỷ lệ nợ xấu theo tháng',
       caption = 'Ghi chú: NPL tính theo đầu khách hàng',
       y = 'Tỷ lệ (%)',
       x = 'Tháng')+
  theme_ft_rc(grid="XY", axis="xy")
  

ggsave("figures/g6.png", width = 7, height = 5)

#-----------------------------------------------------------------------------
p <- dt %>% 
  filter(dayid == report_date, no_days_overdue > 0) %>% 
  group_by(dayid, customer_code) %>% 
  summarise(max_ovd = max(no_days_overdue)) %>% 
  mutate(ovd = cut(max_ovd,
                   breaks = c(1, 10, 30, 90, 180, 360, 5000), 
                   include.lowest = TRUE)) %>% 
  group_by(ovd) %>% 
  summarise(cnt = n_distinct(customer_code))


p %>%   
  ggplot(aes(ovd, cnt))+
  geom_col()+
  labs(title = 'Số lượng KH theo số ngày quá hạn',
       subtitle = paste0("Tháng ", format(report_date, "%m/%Y")),
       y = 'Số lượng HĐ',
       x = 'Số ngày quá hạn')+
  theme_ft_rc(grid="XY", axis="xy")+
  scale_y_continuous(label=scales::comma)+
  theme(axis.text.x = element_text(angle = 20))

ggsave("figures/g7.png", width = 7, height = 5)

#-----------------------------------------------------------------------------

p <- dt %>% 
  filter(dayid == report_date, no_days_overdue > 0) %>% 
  mutate(ovd = cut(no_days_overdue,
                   breaks = c(1, 10, 30, 90, 180, 360, 5000), 
                   include.lowest = TRUE)) %>% 
  group_by(ovd, customer_type) %>% 
  summarise(du_no = sum(du_no)/1e9)


p %>%   
  ggplot(aes(customer_type, du_no))+
  geom_col(aes(fill = ovd), alpha = 0.3)+
  labs(title = 'Dư nợ theo số ngày quá hạn',
       subtitle = paste0("Tháng ", format(report_date, "%m/%Y")),
       y = 'Dư nợ (tỷ đồng)',
       x = NULL)+
  theme_ft_rc(grid="XY", axis="xy")+
  scale_y_continuous(label=scales::comma)+
  theme(axis.text.x = element_text(angle = 20))+
  coord_flip()

ggsave("figures/g8.png", width = 7, height = 5)
#-----------------------------------------------------------------------------
top5 <- dt %>% 
  filter(dayid == report_date, no_days_overdue > 0) %>% 
  group_by(campaign_id) %>% 
  summarise(du_no_by_cp = sum(du_no, na.rm = TRUE)) %>% 
  arrange(-du_no_by_cp) %>% 
  head(5) %>% 
  mutate(nr = row_number()) %>% 
  select(campaign_id, nr)

p <- dt %>% 
  filter(dayid == report_date, no_days_overdue > 0) %>% 
  mutate(ovd = cut(no_days_overdue,
                   breaks = c(1, 10, 30, 90, 180, 360, 5000), 
                   include.lowest = TRUE)) %>% 
  group_by(campaign_id, ovd) %>% 
  summarise(du_no = sum(du_no, na.rm = TRUE)/1e9) %>% 
  inner_join(top5)

p %>%   
  ggplot(aes(reorder(campaign_id, - nr), du_no))+
  geom_col(aes(fill = ovd), alpha = 0.3)+
  labs(title = 'Top 5 campaign có dư nợ quá hạn cao nhất',
       subtitle = paste0("Tháng ", format(report_date, "%m/%Y")),
       y = 'Dư nợ (tỷ đồng)',
       x = NULL)+
  theme_ft_rc(grid="XY", axis="xy")+
  scale_y_continuous(label=scales::comma)+
  coord_flip()

ggsave("figures/g9.png", width = 7, height = 5)

#-----------------------------------------------------------------------------
# Đặc điểm của nhóm KH có OVD
# Đặc điểm danh mục khách hàng tháng 9/2019 ------------------------------
p <- dt %>% 
  filter(dayid == report_date, no_days_overdue >= 1) %>% # Change here
  select(customer_type, gbbb2, datasource, loans_term, region_name,
         segment_code, product_group, term, value_date, bucket, campaign_id) 


p %>% 
  inspect_cat() %>% 
  show_plot()+
  labs(title = "Đặc điểm các hợp đồng OVD",
       subtitle = paste("Tháng", format(report_date, "%m/%Y")),
       caption = "Số ngày quá hạn từ 1 trở lên")+
  theme_ft_rc()

ggsave("figures/g10.png", width = 7, height = 5)
