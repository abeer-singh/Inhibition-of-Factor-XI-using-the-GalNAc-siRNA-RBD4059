#Figure 3e

#Load libraries
library(tidyverse)
library(DescTools)
library(broom)
library(ggthemes)
library(ggsignif)

#functions
#calculate aptt ratio
aptt_ratio_function <- function(aptt, animal_number, ...){
  data_select <- data_time_1 |>
    filter(animal_number_1 == animal_number)
  aptt / data_select[[1,"aptt_1"]]
}

#anova time-course function
anova_timecourse_function <- function(d){
  aov(aptt_ratio~time_days, data = data_wrangled_aptt_ratio |>
        filter(dosage == d) |>
        mutate(time_days = as.character(time_days))) |>
    tidy() |>
    mutate(significance = if_else(p.value <0.0001, true = "****",
                                  false = if_else(p.value < 0.001, true = "***",
                                                  false = if_else(p.value < 0.01, true = "**",
                                                                  false = if_else(p.value < 0.05, true = "*", 
                                                                                  false = "ns"))))) |>
    mutate(dosage = d, .before = term)  
}

#Dunnett timecourse function
dunnett_timecourse_function <- function(d){
  dunnett_test <- DunnettTest(aptt_ratio~time_days, data = data_wrangled_aptt_ratio |>
                                filter(dosage == d), control = "1")
  
  dunnett_test[["1"]] |>
    as_tibble(rownames = "comparison") |>
    mutate(significance = if_else(pval <0.0001, true = "****",
                                  false = if_else(pval < 0.001, true = "***",
                                                  false = if_else(pval < 0.01, true = "**",
                                                                  false = if_else(pval < 0.05, true = "*", 
                                                                                  false = "ns"))))) |>
    mutate(dosage = d, .before = comparison)
}

# specify colorblind palette
cols <- colorblind_pal()(8)

#read raw data
raw_data <- read_xlsx("SR059-PK-22-01-APTT-data.xlsx")

#Wrangling data
data_wrangled <- raw_data |>
  select("dosage" = "剂量（mg/kg）", "time" = "取材时间", "animal_number" = "动物编号",
         "aptt" = "APTT时间（s）") |>
  filter(!row_number() %in% c(1, 164)) |>
  fill(dosage, time, .direction = "down") |>
  mutate(time_days = NA,
         time_days = if_else(str_detect(time, "pre-dose"), true = 1, false = time_days),
         time_days = if_else(str_detect(time, "D2[(]"), true = 2, false = time_days),
         time_days = if_else(str_detect(time, "D8[(]"), true = 8, false = time_days),
         time_days = if_else(str_detect(time, "D15[(]"), true = 15, false = time_days),
         time_days = if_else(str_detect(time, "D29[(]"), true = 29, false = time_days),
         time_days = if_else(str_detect(time, "D43[(]"), true = 43, false = time_days),
         time_days = if_else(str_detect(time, "D57[(]"), true = 57, false = time_days),
         time_days = if_else(str_detect(time, "D71[(]"), true = 71, false = time_days),
         time_days = if_else(str_detect(time, "D85[(]"), true = 85, false = time_days),
         time_days = if_else(str_detect(time, "D113[(]"), true = 113, false = time_days),
         dosage = as.character(dosage),
         dosage = str_c("RBD4059-", dosage, " mg/kg"))

#renaming columns
data_time_1 <- data_wrangled |>
  filter(time_days == 1) |>
  rename (dosage_1 = dosage, time_days_1 = time_days,
          animal_number_1 = animal_number, aptt_1 = aptt)

#calculate aptt ratio buy calling function
data_wrangled_aptt_ratio <- data_wrangled |>
  bind_cols(aptt_ratio = pmap_dbl(data_wrangled, aptt_ratio_function))

#Perform one-way anova on the data for each dosage
data_anova <- data_wrangled_aptt_ratio$dosage |>
  unique() |>
  map(anova_timecourse_function) |>
  list_rbind()

#write anova analysis file
write_csv(x = data_anova, file = "3e_anova.csv")

#Dunnett post-hoc analyses
data_dunnett <- data_wrangled_aptt_ratio$dosage |>
  unique() |>
  map(dunnett_timecourse_function) |>
  list_rbind()

write_csv(x = data_dunnett, file = "3e_dunnett.csv")

#calculate mean and sd for aptt ratio for plotting
data_plot <- data_wrangled_aptt_ratio |>
  group_by(time_days, dosage) |>
  summarise(mean = mean(aptt_ratio, na.rm = T), sd = sd(aptt_ratio, na.rm = T)) |>
  mutate(upper = mean + sd, lower = mean - sd)

data_dunnett_plot <- data_dunnett |>
  separate_wider_delim(cols = comparison, delim = "-", names = c("x1", "x2")) |>
  mutate(x1 = as.double(x1), x2 = as.double(x2)) |>
  filter(significance != "ns") |>
  mutate(y = if_else(dosage == "RBD4059-3 mg/kg", true = 1.8,
                     false = if_else(dosage == "RBD4059-9 mg/kg", true = 1.7, false = 1.6)))

#plot the data
plot_timecourse <- data_plot |>
  ggplot(aes(x = time_days, y = mean, group = dosage, color = dosage)) +
  geom_line(linewidth = 0.5) +
  geom_errorbar(aes(ymin = mean, ymax = upper), width = 1.6, linewidth = 0.3) +
  geom_point(aes(shape = dosage, fill = dosage), color = "black", size = 1, stroke = 0.3) +
  scale_y_continuous(limits = c(0.5, 2), breaks = c(0.5, 1, 1.5, 2)) +
  theme_classic() +
  labs(x = "Time (Days)", y = "APTT Ratio") +
  theme(text = element_text(size = 8),
        title = element_text(size = 8),
        legend.title = element_blank(),
        legend.box.spacing = unit(0.2, "cm")) +
  scale_color_manual(values = cols[c(2, 3, 4, 6, 7, 8)]) +
  scale_fill_manual(values = cols[c(2, 3, 4, 6, 7, 8)]) +
  scale_shape_manual(values = 21:25) +
  scale_x_continuous(breaks = c(1, 2, 8, 15, 29, 43, 57, 71, 85, 113), guide = guide_axis(n.dodge = 2)) +
  geom_text(data = data_dunnett_plot, mapping = aes(x = x1, y = y,
                                                    label = significance), show.legend = F)

#save the plot
ggsave("3e_plot_timecourse.pdf", plot = plot_timecourse, scale = 1, width = 15,
       height = 6, units = "cm", dpi = 300)

#calculate area under the curve
data_auc <- data_wrangled_aptt_ratio |>
  select(dosage, time_days, animal_number, aptt_ratio) |>
  pivot_wider(names_from = c(dosage, animal_number), values_from = aptt_ratio, id_cols = time_days) |>
  filter(time_days != 113) |> #removed day 113 data points 
  select_if(~ !any(is.na(.))) #removes columns with any NA values (removed monkey #4501 that died)

auc <- data_auc |>
  select(-time_days) |>
  map(.f = ~AUC(x = data_auc$time_days, y = .x, method = "trapezoid", na.rm = T)) |>
  as_tibble() |>
  pivot_longer(cols = everything(), values_to = "auc_value", names_to = "sample") |>
  separate_wider_delim(cols = sample, delim = "_", names = c("dosage", "animal_number"))

#Statistics: Kruskal-Wallis test for non-parametric alternative to one-way ANOVA
# Assumption: the sample data does not come from a normal distribution
kruskal_test_auc <- auc |>
  kruskal.test(formula = auc_value ~ dosage, data = auc) |>
  tidy() |>
  mutate(significance = if_else(p.value <0.0001, true = "****",
                                false = if_else(p.value < 0.001, true = "***",
                                                false = if_else(p.value < 0.01, true = "**",
                                                                false = if_else(p.value < 0.05, true = "*", 
                                                                                false = "ns")))))
write_csv(x = kruskal_test_auc, file = "3e_kruskal_auc.csv")

#Since p-value from Kruskal-Wallis test is <0.05, it means atleast one pairwaise
# group comparison should be positive. For pairwaise comparison we do Dunn's multiple
# comparison test
dunn_test <- DunnTest(auc_value ~ dosage, data = auc, method = "holm", alternative = "two.sided")
dunn_test_data <- dunn_test[[1]] |>
  as_tibble(rownames = "comparison") |>
  mutate(significance = if_else(pval <0.0001, true = "****",
                                false = if_else(pval < 0.001, true = "***",
                                                false = if_else(pval < 0.01, true = "**",
                                                                false = if_else(pval < 0.05, true = "*", 
                                                                                false = "ns")))))

write_csv(x = dunn_test_data, file = "3e_dunn_auc.csv")
auc_significances <- dunn_test_data |>
  select(significance)

#plot AUC graph and put significance levels
plot_auc <- auc |>
  ggplot(aes(x = dosage, y = auc_value)) +
  geom_boxplot(aes(fill = dosage), width = 0.5, linewidth = 0.15, outlier.size = 0.05) +
  theme_classic() +
  labs(x = NULL, y = "AUC\nNormalized to baseline") +
  theme(text = element_text(size = 8),
        title = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        legend.position = "none") +
  scale_fill_manual(values = cols[c(2, 3, 4, 6, 7, 8)]) +
  geom_signif(
    y_position = c(120, 125, 115), xmin = c(1, 1, 2), xmax = c(2, 3, 3),
    annotations = auc_significances$significance, textsize = 2.8, size = 0.3) +
  scale_y_continuous(limits = c(60, 130), breaks = c(60, 90, 120))

#Save the auc plot
ggsave("3e_plot_auc.pdf", plot = plot_auc, scale = 1, width = 5,
       height = 6, units = "cm", dpi = 300)

#------------end of code---------------------------------------------------------
