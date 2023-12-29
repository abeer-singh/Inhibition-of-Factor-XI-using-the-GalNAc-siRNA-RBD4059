#Fig 2B

#Load library
library(tidyverse)
library(readxl)
library(ggthemes)

# specify colorblind palette
cols <- colorblind_pal()(8)

#read the data 1 mg/kg, 3 mg/kg, 9 mg/kg
dose_1 <- read_xls(path = "2b.xls", range = c("A3:K18"), col_types = "numeric") |>
  rename("time" = "Time (h)...1", "RBD4059_AS 1 mg/kg_1" = "C2001",
          "RBD4059_AS 1 mg/kg_2" = C2002,
          "RBD4059_AS 1 mg/kg_3" = C2003,
          "RBD4059_AS 1 mg/kg_4" = C2501,
          "RBD4059_AS 1 mg/kg_5" = C2502,
          "RBD4059_AS 1 mg/kg_6" = C2503) |>
  select(-c("Mean", "SD", "CV (%)", "Time (h)...8"))

dose_2 <- read_xls(path = "2b.xls", range = c("A21:K36"), col_types = "numeric") |>
  rename("time" = "Time (h)...1", "RBD4059_AS 3 mg/kg_1" = "C3001",
         "RBD4059_AS 3 mg/kg_2" = "C3002",
         "RBD4059_AS 3 mg/kg_3" = "C3003",
         "RBD4059_AS 3 mg/kg_4" = "C3501",
         "RBD4059_AS 3 mg/kg_5" = "C3502",
         "RBD4059_AS 3 mg/kg_6" = "C3503") |>
  select(-c("Mean", "SD", "CV (%)", "Time (h)...8"))

dose_3 <- read_xls(path = "2b.xls", range = c("A39:K54"), col_types = "numeric") |>
  rename("time" = "Time (h)...1","RBD4059_AS 9 mg/kg_1" = "C4001",
         "RBD4059_AS 9 mg/kg_2" = "C4002",
         "RBD4059_AS 9 mg/kg_3" = "C4003",
         "RBD4059_AS 9 mg/kg_4" = "C4501",
         "RBD4059_AS 9 mg/kg_5" = "C4502",
         "RBD4059_AS 9 mg/kg_6" = "C4003") |>
  select(-c("Mean", "SD", "CV (%)", "Time (h)...8"))

#Combine data for all doses and data wrangling
data <- dose_1 |>
  full_join(dose_2, by = "time") |>
  full_join(dose_3, by = "time") |>
  pivot_longer(cols = -time, names_to = "dosage", values_to = "concentration") |>
  mutate(dosage = str_remove(dosage, "_[0-9]+$"))

#Calculate data summaries for plotting
data_plot <- data |>
  group_by(time, dosage) |>
  summarise(mean = mean(concentration, na.rm = T), sd = sd(concentration, na.rm = T)) |>
  mutate(upper = mean + sd, lower = mean - sd) |>
  filter(!is.na(sd))

#Plot the data
p <- data_plot |>
  ggplot(aes(x = time, y = mean, color = dosage)) +
  geom_line(linewidth = 0.5) +
  scale_y_log10(breaks = c(50, 100, 250, 500, 1000, 2000, 4000)) +
  scale_x_continuous(limits = c(0, 12.1), breaks = c(2, 4, 6, 8, 10, 12)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, linewidth = 0.3) +
  geom_point(aes(shape = dosage, fill = dosage), color = "black", size = 1,
             stroke = 0.3) +
  theme_classic() +
  labs(x = "Time (h)", y = "Plasma concentration (ng/mL)") +
  theme(text = element_text(size = 8),
        title = element_text(size = 8),
        legend.title = element_blank(),
        legend.box.spacing = unit(0.2, "cm")) +
  scale_color_manual(values = cols[c(2, 3, 4, 6, 7, 8)]) +
  scale_fill_manual(values = cols[c(2, 3, 4, 6, 7, 8)]) +
  scale_shape_manual(values = 21:25) +
  geom_hline(yintercept = 20, linetype = "dashed") +
  annotate(geom = "text", x = 10, y = 26, label = "LLOQ", size = 3)

#save plot
ggsave(filename = "2B.pdf", plot = p, scale = 1, width = 15,
       height = 6, units = "cm", dpi = 300)

