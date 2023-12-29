library(tidyverse)
library(officer)
library(ggthemes)

# specify colorblind palette
cols <- colorblind_pal()(8)

#extracting mouse data for APTT and FXI activity pre-dosing
#mouse in fig 3a
raw_data_3a_mouse <- read_docx("Starting levels APTT and FXI activity (mouse 3A).docx")

data_fxi_activity_3a <-  docx_summary(raw_data_3a_mouse) |>
  as_tibble() |>
  slice(130:137) |>
  select(fxi_activity = text) |>
  mutate(fxi_activity = as.double(fxi_activity))

data_aptt_3a <- docx_summary(raw_data_3a_mouse) |>
  as_tibble() |>
  slice(256:263) |>
  select(aptt = text) |>
  mutate(aptt = as.double(aptt))

data_mouse_3a <- data_fxi_activity_3a |>
  bind_cols(data_aptt_3a) |>
  pivot_longer(cols = everything()) |>
  mutate(species = "mouse")

#mouse in fig 3B
raw_data_3b_mouse <- read_docx("Starting levels APTT and FXI activity (mouse 3B).docx")

data_fxi_activity_3b <- docx_summary(raw_data_3b_mouse) |>
  as_tibble() |>
  slice(154:159) |>
  select(fxi_activity = text) |>
  mutate(fxi_activity = as.double(fxi_activity))

data_aptt_3b <- docx_summary(raw_data_3b_mouse) |>
  as_tibble() |>
  slice(304:309) |>
  select(aptt = text) |>
  mutate(aptt = as.double(aptt))

data_mouse_3b <- data_fxi_activity_3b |>
  bind_cols(data_aptt_3b) |>
  pivot_longer(cols = everything()) |>
  mutate(species = "mouse")

#extracting monkey data for APTT and FXI activity pre-dosing
raw_data_monkey <- read_docx("Starting levels APTT and FXI activity (monkey).docx")

data_fxi_activity_monkey <- docx_summary(raw_data_monkey) |>
  as_tibble() |>
  slice(65:82) |>
  select(fxi_activity = text) |>
  mutate(fxi_activity = as.double(fxi_activity))

data_aptt_monkey <- docx_summary(raw_data_monkey) |>
  as_tibble() |>
  slice(125:142) |>
  select(aptt = text) |>
  mutate(aptt = as.double(aptt))

data_monkey <- data_fxi_activity_monkey |>
  bind_cols(data_aptt_monkey) |>
  pivot_longer(cols = everything()) |>
  mutate(species = "monkey")

#combine all data
final_data <- data_mouse_3a |>
  bind_rows(data_mouse_3b, data_monkey) |>
  mutate(name = if_else(name == "fxi_activity", "FXI Activity", "APTT"),
         species = if_else(species == "mouse", "Mouse", "Cynomolgus Monkey"))

#calculate x_y coordinates for geom_jitter
x_y_coordinates <- final_data |>
  mutate(x = as.double(NA), .after = value) |>
  rename(y = value) |>
  rows_update(tibble(name = c("FXI Activity", "APTT", "FXI Activity", "APTT"),
                     species = c("Mouse", "Mouse", "Cynomolgus Monkey", "Cynomolgus Monkey"),
                     x = c(2.1, 1.1, 1.9, 0.9)), by = c("name", "species"))

plot <- final_data |>
  ggplot(aes(x = name, y = value)) +
  geom_boxplot(aes(fill = species), alpha = 0.5, width = 0.5, linewidth = 0.15,
               show.legend = T, outlier.shape = NA) +
  theme_bw() +
  geom_jitter(data = x_y_coordinates, aes(x = x, y = y), height = 0, width = 0.08,
              color = "red", size = 0.05, stroke = 0.25) +
  labs(x = NULL, y = "FXI Activity (%) / APTT (s)") +
  theme(text = element_text(size = 8),
        title = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        legend.title = element_blank()) +
  scale_fill_manual(values = cols[c(2, 3, 4, 6, 7, 8)]) +
  scale_color_manual(values = cols[c(2, 3, 4, 6, 7, 8)])

#Save the auc plot
ggsave("summary_plot.pdf", plot = plot, scale = 1, width = 10,
       height = 6, units = "cm", dpi = 300)

#summary statistics
summary_statistics <- final_data |>
  group_by(name, species) |>
  summarise(n = n(),
            mean = mean(value),
            min = min(value),
            lower_quartile = quantile(value)[[2]],
            median = median(value),
            upper_quartile = quantile(value)[[4]],
            max = max(value),
            standard_deviation = sd(value))

#print summary statistics
write_csv(x = summary_statistics, file = "summary_statistics.csv")



  
