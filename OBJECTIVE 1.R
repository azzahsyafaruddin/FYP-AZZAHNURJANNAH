
# Purpose: Data cleaning, descriptive statistics, and visualization

library(dplyr)
library(ggplot2)
library(forcats)
library(scales)
library(treemapify)
library(readr)
library(tidyr)

# 1. Load and Clean Data
data_cars <- read_csv("C:/Users/Azzah Syafaruddin/OneDrive - Universiti Kebangsaan Malaysia/FYP2/updated_dataset.csv")

# Filter out records with missing reserve prices
data_cars <- data_cars %>% 
  filter(!is.na(reserveprice))

# Create 'car_age' column (Current Year - Manufacturing Year)
if (!"car_age" %in% names(data_cars) && "manufacturing_year" %in% names(data_cars)) {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  data_cars <- data_cars %>% mutate(car_age = current_year - manufacturing_year)
}

# Create 'brand_price_label' column (High/Medium/Low based on quantiles)
if (!"brand_price_label" %in% names(data_cars)) {
  brand_tiers <- data_cars %>%
    group_by(car_brand) %>%
    summarise(avg_p = mean(reserveprice, na.rm = TRUE)) %>%
    mutate(
      brand_price_label = case_when(
        avg_p >= quantile(avg_p, 0.66) ~ "Tinggi",
        avg_p >= quantile(avg_p, 0.33) ~ "Sederhana",
        TRUE ~ "Rendah"
      )
    ) %>%
    select(car_brand, brand_price_label)
  
  data_cars <- data_cars %>% left_join(brand_tiers, by = "car_brand")
}

# 2. Table 4.3: Descriptive Statistics (Reserve Price)
deskriptif_stats <- data_cars %>%
  summarise(
    `Bilangan pemerhatian` = n(),
    `Harga minimum (RM)` = min(reserveprice),
    `Kuartil pertama, Q1 (RM)` = quantile(reserveprice, 0.25),
    `Median (RM)` = median(reserveprice),
    `Purata (RM)` = mean(reserveprice),
    `Kuartil ketiga, Q3 (RM)` = quantile(reserveprice, 0.75),
    `Harga maksimum (RM)` = max(reserveprice),
    `Sisihan piawai (RM)` = sd(reserveprice)
  ) %>%
  pivot_longer(cols = everything(), names_to = "Statistik", values_to = "Nilai") %>%
  mutate(Nilai = comma(round(Nilai, 0)))

print(as.data.frame(deskriptif_stats))

# 3. Table 4.2: Categorical Variable Analysis
# Function to calculate frequency and percentage with 'Top N' filtering
kira_statistik_kategori <- function(data, col_name, top_n_val = 0) {
  
  # Calculate base counts
  df_count <- data %>%
    group_by(across(all_of(col_name))) %>%
    summarise(Kekerapan = n(), .groups = 'drop') %>%
    rename(Kategori = 1) %>%
    mutate(Kategori = as.character(Kategori)) %>%
    arrange(desc(Kekerapan))
  
  # Apply Top N logic if applicable
  if (top_n_val > 0 && nrow(df_count) > top_n_val) {
    top_items <- df_count %>% slice_head(n = top_n_val)
    baki_baris <- nrow(df_count) - top_n_val
    
    others <- df_count %>% 
      slice_tail(n = baki_baris) %>%
      summarise(Kategori = "Lain-lain", Kekerapan = sum(Kekerapan))
    
    df_count <- bind_rows(top_items, others)
  }
  
  # Calculate Percentages
  total_obs <- sum(df_count$Kekerapan)
  df_final <- df_count %>%
    mutate(`Peratusan (%)` = round((Kekerapan / total_obs) * 100, 2)) %>%
    select(Kategori, Kekerapan, `Peratusan (%)`)
  
  print(as.data.frame(df_final), row.names = FALSE)
}

# Run function for categorical variables
kira_statistik_kategori(data_cars, "car_brand", top_n_val = 15)
kira_statistik_kategori(data_cars, "car_model", top_n_val = 15)
kira_statistik_kategori(data_cars, "car_variant", top_n_val = 15)
kira_statistik_kategori(data_cars, "car_transmission", top_n_val = 0)
kira_statistik_kategori(data_cars, "car_type", top_n_val = 0)

if("brand_price_label" %in% names(data_cars)) {
  kira_statistik_kategori(data_cars, "brand_price_label", top_n_val = 0)
}

# 4. Figure 4.1: Histogram (Reserve Price Distribution)
ggplot(data_cars, aes(x = reserveprice)) +
  geom_histogram(bins = 30, fill = "#4682B4", colour = "black", alpha = 0.8) +
  labs(
    title = "Taburan Harga Rizab Kereta Terpakai",
    x = "Harga rizab (RM)",
    y = "Kekerapan"
  ) +
  scale_x_continuous(labels = comma) +
  theme_minimal()

# 5. Figure 4.4: Scatter Plot (Car Age vs Reserve Price)
if("car_age" %in% names(data_cars)) {
  ggplot(data_cars, aes(x = car_age, y = reserveprice)) +
    geom_point(alpha = 0.4, size = 1, color = "darkslategrey") +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(
      title = "Hubungan Umur Kenderaan dengan Harga Rizab",
      x = "Umur kenderaan (tahun)",
      y = "Harga rizab (RM)"
    ) +
    scale_y_continuous(labels = comma) +
    theme_minimal()
}

# 6. Figure 4.2: Boxplot (Price by Brand)
# Filter for Top 25 brands to ensure readability
top_brands <- data_cars %>%
  count(car_brand) %>%
  top_n(25, wt = n) %>%
  pull(car_brand)

data_top_brands <- data_cars %>% filter(car_brand %in% top_brands)

ggplot(data_top_brands, aes(x = fct_reorder(car_brand, reserveprice, median), y = reserveprice)) +
  geom_boxplot(fill = "lightgreen", outlier.size = 1, alpha = 0.7) +
  coord_flip() +
  labs(
    title = "Taburan Harga Rizab Kereta Terpakai Mengikut Jenama",
    x = "Jenama kereta",
    y = "Harga rizab (RM)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# 7. Figure 4.3: Boxplot (Price by Car Type)
ggplot(data_cars %>% filter(!is.na(car_type)), aes(x = car_type, y = reserveprice)) +
  geom_boxplot(fill = "lightblue", outlier.size = 1) +
  labs(
    title = "Perbandingan Harga Rizab Mengikut Jenis Kenderaan",
    x = "Jenis Kenderaan",
    y = "Harga Rizab (RM)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

# 8. Figure 4.5: Treemap (Brand Distribution)
treemap_data <- data_cars %>%
  group_by(car_brand) %>%
  summarise(
    Bilangan = n(),
    Purata_Harga = mean(reserveprice, na.rm = TRUE)
  ) %>%
  mutate(
    Label_Penuh = paste0(car_brand, "\n(", Bilangan, " unit)")
  )

ggplot(treemap_data, aes(area = Bilangan, fill = Purata_Harga, label = Label_Penuh)) +
  geom_treemap(colour = "white", size = 1.5) +
  geom_treemap_text(
    colour = "white",
    place = "centre",
    grow = TRUE,
    reflow = TRUE,
    min.size = 0 
  ) +
  scale_fill_gradient(
    low = "#6baed6",
    high = "#08306b",
    name = "Purata harga (RM)",
    labels = comma
  ) +
  labs(
    title = "Carta Peta Bersarang (Tree Map) Jenama Kereta",
    subtitle = "Saiz = Bilangan kereta, Warna = Purata harga rizab"
  ) +
  theme_minimal()

# 9. Top 25 Brands Table
jadual_jenama <- data_cars %>%
  group_by(car_brand) %>%
  summarise(
    `Bilangan kereta` = n(),
    `Purata harga rizab (RM)` = mean(reserveprice, na.rm = TRUE)
  ) %>%
  arrange(desc(`Bilangan kereta`)) %>%
  mutate(`Purata harga rizab (RM)` = comma(round(`Purata harga rizab (RM)`, 2))) %>%
  head(25)

print(as.data.frame(jadual_jenama))

