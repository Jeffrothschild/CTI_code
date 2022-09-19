library(tidyverse)
library(patchwork)
library(ggtext)
library(ggpp)


# vuelta ------------------------------------------------------------------
#Data from Strobel et al 2022 "Case Study: The application of daily carbohydrate periodisation throughout a cycling Grand Tour"  

vuelta_df <- structure(list(stage = c("1", "2", "3", "4", "5", "6", "7", "8", 
                                      "9", "rest", "10", "11", "12", "13", "14", "15", "rest", "16", 
                                      "17", "18", "19", "20"), bike_EE = c(1701, 2681, 4150, 2780, 
                                                                           2715, 3293, 4800, 3083, 5086, 566, 4672, 3411, 3307, 3202, 4343, 
                                                                           3014, 560, 3671, 5306, 5592, 4769, 5830), carb_gkg = c(8.03353108597, 
                                                                                                                                  10.94024831193, 13.34152011631, 12.06212005867, 10.33113092011, 
                                                                                                                                  10.51304214951, 16.66763938619, 10.71442406037, 16.76079362311, 
                                                                                                                                  5.051367445286, 14.88703269214, 11.4421482076, 11.67805442952, 
                                                                                                                                  12.63598026535, 13.86457432276, 11.19536771872, 5.621167133154, 
                                                                                                                                  12.94883826465, 13.87047693016, 17.64331265873, 15.40727424674, 
                                                                                                                                  12.088503525)), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, 
                                                                                                                                                                                                          -22L)) %>% as_tibble()
vuelta_cor <- round(cor(vuelta_df$bike_EE, vuelta_df$carb_gkg),2)  %>% format(nsmall = 2)
vuelta_range <- round(max(vuelta_df$carb_gkg) - min(vuelta_df$carb_gkg),1)
vuelta_monotony <- round(mean(vuelta_df$carb_gkg) / sd(vuelta_df$carb_gkg),1)  %>% format(nsmall = 1)
vuelta_index <- round(vuelta_range*(cor(vuelta_df$bike_EE, vuelta_df$carb_gkg))/(mean(vuelta_df$carb_gkg) / sd(vuelta_df$carb_gkg)),1)  %>% format(nsmall = 1)

vuelta_plot <- vuelta_df  %>% 
  ggplot(aes(carb_gkg, bike_EE))+
  geom_smooth(se = F, method = "lm", color = "grey80")+
  annotate("text", x = 17, y = 2500, label = paste0("Correlation: ",  vuelta_cor ))+
  annotate("text", x = 17, y = 2000, label = paste0("CHO range: ",  vuelta_range ))+
  annotate("text", x = 17, y = 1500, label = paste0("CHO monotony: ",  vuelta_monotony))+
  annotate("richtext", x = 17, y = 900, label = paste0("<b>CTI: <b> ", vuelta_index), label.color = NA, size = 5)+
  geom_point(alpha = 1, size = 2, color = "#801d1d")+
  scale_x_continuous(limits = c(3, 20))+
  scale_y_continuous(limits = c(500, 6200))+
  labs(x =  "Daily CHO (g/kg)", y = "Exercise energy expenditure\n(kcal)")


vuelta_plot


# giro ---------------------------------------------------------------------


giro_df <- structure(list(stage = structure(c(1L, 2L, 3L, 4L, 5L, 10L, 6L, 
                                                 7L, 8L, 9L), .Label = c("11", "12", "13", "14", "15", "16", "17", 
                                                                         "18", "19", "rest"), class = "factor"), time = new("Period", 
                                                                                                                            .Data = c(37, 34, 25, 31, 28, NA, 35, 57, 45, 26), year = c(0, 
                                                                                                                                                                                        0, 0, 0, 0, NA, 0, 0, 0, 0), month = c(0, 0, 0, 0, 0, NA, 
                                                                                                                                                                                                                               0, 0, 0, 0), day = c(0, 0, 0, 0, 0, NA, 0, 0, 0, 0), hour = c(3, 
                                                                                                                                                                                                                                                                                             4, 3, 5, 4, NA, 0, 3, 5, 5), minute = c(26, 49, 56, 25, 39, 
                                                                                                                                                                                                                                                                                                                                     NA, 40, 19, 6, 12)), kcal_pre = c(523, 548, 754, 1043, 1319, 
                                                                                                                                                                                                                                                                                                                                                                       361, 819, 879, 1061, 997), kcal_post = c(516, 576, 1023, 782, 
                                                                                                                                                                                                                                                                                                                                                                                                                1093, 583, 0, 1007, 1371, 2384), kcal_dinner = c(452, 355, 1324, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                 1241, 740, 783, 686, 1057, 1325, 972), kcal_during = c(981, 1185, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        1107, 1422, 1502, 0, 0, 1450, 1654, 2060), carb_pre = c(1.1, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                1.2, 2, 2.9, 3.5, 0.5, 2.3, 2.4, 3, 2.7), carb_post = c(0.7, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        0.9, 2.2, 1.3, 2.1, 1.3, 0, 2.4, 5.4, 6.3), carb_dinner = c(0.7, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    0.8, 3.3, 3.2, 0.7, 1.6, 1.6, 2.7, 3.2, 2.7), carb_during_total_g = c(235, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          280, 233, 324, 322, 0, 0, 318, 384, 522), carb_daily_gkg = c(5.904606227166, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       6.599936178388, 10.40259714887, 12.01721881044, 11.18076494761, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       3.429434787989, 8.501429473862, 12.17351663595, 17.20116181384, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       18.81396000078), EEE = c(3635, 4015, 4056.67411109851, 5405, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                5034, 853, 1042, 3939, 4227, 6180), time_mins = c(206.616666666667, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  289.566666666667, 236.416666666667, 325.516666666667, 279.466666666667, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  NA, 40.5833333333333, 199.95, 306.75, 312.433333333333)), class = c("tbl_df", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "tbl", "data.frame"), row.names = c(NA, -10L)) %>% as_tibble()

giro_cor <- round(cor(giro_df$EEE, giro_df$carb_daily_gkg, use = "pairwise.complete.obs"),2) %>% format(nsmall = 2)
giro_range <- round(max(giro_df$carb_daily_gkg, na.rm = T) - min(giro_df$carb_daily_gkg, na.rm = T),1)
giro_monotony <- round(mean(giro_df$carb_daily_gkg, na.rm = T) / sd(giro_df$carb_daily_gkg, na.rm = T),2)
giro_index <- round(giro_range*(cor(giro_df$EEE, giro_df$carb_daily_gkg, use = "pairwise.complete.obs"))/giro_monotony,1)



giro_plot <- giro_df  %>% 
  ggplot(aes(carb_daily_gkg, EEE))+
  geom_smooth(se = F, method = "lm", color = "grey80")+
  annotate("text", x = 17, y = 2500, label = paste0("Correlation: ",  giro_cor ))+
  annotate("text", x = 17, y = 2000, label = paste0("CHO range: ",  giro_range ))+
  annotate("text", x = 17, y = 1500, label = paste0("CHO monotony: ",  giro_monotony))+
  annotate("richtext", x = 17, y = 900, label = paste0("<b>CTI: <b> ", giro_index), label.color = NA, size = 5)+
  geom_point(alpha = 1, size = 2, color = "#801d1d")+
  scale_x_continuous(limits = c(3, 20))+
  scale_y_continuous(limits = c(500, 6200))+
  labs(x =  "Daily CHO (g/kg)", y = NULL)

giro_plot



vuelta_plot+ giro_plot+
  plot_annotation(title = "", tag_levels = 'a') &
  theme(plot.tag = element_text(size = 17, face="bold"))


# corr simulations --------------------------------------------------------

set.seed(123)
corr <- 0.71
cov.mat <- matrix(c(1, corr, corr, 1), nrow = 2)
df <- data.frame(MASS::mvrnorm(n = 50, mu = c(3.36, 1.22), Sigma = cov.mat)) %>% 
  as_tibble() %>% 
  transmute(
    carb = (X1-1)*1.2,
    ee = (X2 ) * 700,
  ) 

cor1 <- df %>%
  ggplot( aes(carb, ee)) +
  scale_x_continuous(limits = c(0,6), breaks = seq(0,6,1.5), labels = seq(0,12,3)) +
  scale_y_continuous(limits = c(0,2800), breaks = seq(0,2500,500))+
  geom_smooth(se = F, method = "lm", color = "grey80")+
  geom_point(alpha = 1, size = 2, color = "#6CC458")+
  ggpubr::stat_cor(r.accuracy = 0.01, cor.coef.name = 'r', aes(label = paste(..r.label..)))+
  labs(x =  "Daily CHO (g/kg)", y = "Exercise energy expenditure\n(kcal)")


set.seed(1234)
corr2 <- 0.8
cov.mat2 <- matrix(c(1, corr2, corr2, 1), nrow = 2)
df2 <- data.frame(MASS::mvrnorm(n = 50, mu = c(3.36, 1.22), Sigma = cov.mat2)) %>% 
  as_tibble()  %>% 
  transmute(
    carb = X1, #(X1 +2) * 2.7,
    ee = (X2 ) * 700,
  )

cor2 <- df2 %>%
  ggplot( aes(carb, ee)) +
  scale_x_continuous(limits = c(0,12), breaks = seq(0,12,3), labels = seq(0,12,3)) +
  scale_y_continuous(limits = c(0,2800), breaks = seq(0,2500,500))+
  geom_smooth(se = F, method = "lm", color = "grey80")+
  geom_point(alpha = 1, size = 2, color = "#6CC458")+
  ggpubr::stat_cor(r.accuracy = 0.01, cor.coef.name = 'r', aes(label = paste(..r.label..)))+
  labs(x =  "Daily CHO (g/kg)", y = NULL)



cor1 + cor2+
  plot_annotation(title = "", tag_levels = 'a') &
  theme(plot.tag = element_text(size = 17, face="bold"))



# monotony sim ------------------------------------------------------------

set.seed(333)

df3 <- tibble(
  carb = rnorm(50, 5.4, 0.8),
  day = seq(1,50,1 )
) %>% 
  filter(carb>0) 

df3$carb[4] <- 0.9
df3$carb[10] <- 2
df3$carb[20] <- 9.7
df3$carb[41] <- 11.5

mon3_value <- round(mean(df3$carb)/sd(df3$carb),1) %>% format(nsmall = 1)
mon3_range <- round(max(df3$carb)-min(df3$carb),1) %>% format(nsmall = 1)



mon3 <- df3 %>% 
  mutate() %>% 
  ggplot(aes(day, carb))+
  geom_point(alpha = 1, size = 2, color = "#1d1d80")+
  annotate("text_npc", npcx = 0.05, npcy = 0.98, label = paste0("CHO range = ", mon3_range), hjust = 0)+
  annotate("text_npc", npcx = 0.05, npcy = 0.9, label = paste0("CHO monotony = ", mon3_value), hjust = 0)+
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12,3), labels = seq(0,12,3)) +
  labs(x = "Day of analysis period", y = "Daily CHO (g/kg)")


set.seed(444)
df4 <- tibble(
  carb = rnorm(50, 5.4, 3.2),
  day = seq(1,50,1 )
) %>% 
  filter(carb>0)

mon4_value <- round(mean(df4$carb)/sd(df4$carb),1) %>% format(nsmall = 1)
mon4_range <- round(max(df4$carb)-min(df4$carb),1) %>% format(nsmall = 1)


mon4 <- df4 %>% 
  mutate(day = seq(1,nrow(.),1 )) %>% 
  ggplot(aes(day, carb))+
  geom_point(alpha = 1, size = 2, color = "#1d1d80") +
  annotate("text_npc", npcx = 0.05, npcy = 0.98, label = paste0("CHO range = ", mon4_range), hjust = 0)+
  annotate("text_npc", npcx = 0.05, npcy = 0.9, label = paste0("CHO monotony = ", mon4_value), hjust = 0)+  
  scale_y_continuous(limits = c(0,12), breaks = seq(0,12,3), labels = seq(0,12,3)) +
  labs(x = "Day of analysis period", y = NULL)




mon3 + mon4+
  plot_annotation(title = "", tag_levels = 'a') &
  theme(plot.tag = element_text(size = 17, face="bold"))






# CTI simulations -----------------------------------------------------------

sim_carb_range <-  seq(0.4, 12, .2)
sim_cor <-  seq(0.1, .85, .05)
sim_monotony <-  seq(1, 6.2, .2)



sim_tbl <- expand_grid(sim_carb_range, sim_cor, sim_monotony) %>%
  mutate(
    per_index_2 = sim_carb_range * sim_cor / sim_monotony,
  )

sim_a <- sim_tbl %>% 
  mutate(
    scenario = case_when(
      sim_monotony == 2 &  sim_cor == .2 ~ "Low monotony Low correlation",
      sim_monotony == 6 &  sim_cor == .2 ~ "High monotony Low correlation",
      sim_monotony == 2 &  sim_cor == .8 ~ "Low monotony High correlation",
      sim_monotony == 6 &  sim_cor == .8 ~ "High monotony High correlation",
    )
  ) %>% drop_na() %>% 
  ggplot(aes(sim_carb_range, per_index_2)) +
  geom_line(size = 1)+
  scale_y_continuous(limits = c(0, 6))+
  scale_x_continuous(breaks = c(0, 3,6,9,12)) +
  labs(x=expression(bold(Daily~CHO~range~(g~kg^-1))), y = "CTI")+
  facet_wrap(~ scenario)+
  theme(
    strip.background = element_rect(fill = "lightblue1", color = "black", size = 1),
    strip.text = element_text(colour = "black", face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.background = element_rect(fill = "azure")
  )



sim_b <- sim_tbl %>% 
  mutate(
    scenario = case_when(
      sim_monotony == 2 &  sim_carb_range == 2 ~ "Low monotony Low range",
      sim_monotony == 6 &  sim_carb_range == 2 ~ "High monotony Low range",
      sim_monotony == 2 &  sim_carb_range == 12 ~ "Low monotony High range",
      sim_monotony == 6 &  sim_carb_range == 12 ~ "High monotony High range",
    )
  ) %>% drop_na() %>% 
  ggplot(aes(sim_cor, per_index_2)) +
  geom_line(size = 1)+
  scale_y_continuous(limits = c(0, 6))+
  labs(x = "Correlation between CHO intake and training load", y = "CTI")+
  facet_wrap(~ scenario) +
  theme(
    strip.background = element_rect(fill = "limegreen", color = "black", size = 1),
    strip.text = element_text(colour = "black", face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.background = element_rect(fill = "honeydew1")
  )



sim_c <- sim_tbl %>% 
  mutate(
    scenario = case_when(
      sim_cor == .2 &  sim_carb_range == 2 ~ "Low correlation Low range",
      sim_cor == .8 &  sim_carb_range == 2 ~ "High correlation Low range",
      sim_cor == .2 &  sim_carb_range == 12 ~ "Low correlation High range",
      sim_cor == .8 &  sim_carb_range == 12 ~ "High correlation High range",
    )
  ) %>% drop_na() %>% 
  ggplot(aes(sim_monotony, per_index_2)) +
  geom_line(size = 1)+
  scale_y_continuous(limits = c(0, 6))+
  labs(x = "CHO monotony (mean intake / SD)", y = "CTI")+
  facet_wrap(~ scenario)+
  theme(
    strip.background = element_rect(fill = "orange", color = "black", size = 1),
    strip.text = element_text(colour = "black", face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.background = element_rect(fill = "moccasin")
  )

sim_explain <- ggplot()+
  annotate("text", x = 1, y = 1,
           label = "CTI =  correlation * range / monotony

Fixed values are set as follows:

Low monotony: 2
High monotony: 6
Low correlation: 0.2
High correlation: 0.8
Low CHO range: 2
High CHO range: 12  ")+
  scale_y_continuous(limits = c(0,2))+
  theme_void()

sim_b + sim_a + sim_c + sim_explain+
  plot_layout(nrow = 2)+
  plot_annotation(title = "", tag_levels = list(c('a', 'b', 'c', ' '))) &
  theme(plot.tag = element_text(size = 17, face="bold"))


