#Statistiques descriptives 
database1 <- database[, !(names(database) %in% c("city", "host_country", "country"))]
database1_imputed <- database1
for (col in names(database1_imputed)) {
  database1_imputed[[col]][is.na(database1_imputed[[col]])] <- mean(database1_imputed[[col]], na.rm = TRUE)
}
mat_var_covar = cov(database1_imputed)                   
print(mat_var_covar)

mat_correlation <- cor(database1_imputed, use = "complete.obs")

print(mat_correlation)
library(ggplot2)
library(reshape2)

# Transformer la matrice en un format long pour ggplot2
mat_melt <- melt(mat_correlation)

# Créer le heatmap
ggplot(data = mat_melt, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Corrélation") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  coord_fixed()

#
test=database %>% 
  group_by(country) %>% 
  summarise(
    avg_medals = mean(total_medals, na.rm = TRUE),
    avg_gdppc = mean(gdppc, na.rm = TRUE),
    avg_pop = mean(pop, na.rm = TRUE),
  ) %>% 
  arrange(desc(avg_medals))



#Tendances temporelles des médailles et du PIB avec échelles différentes
data_plot <- database %>%
  group_by(year) %>%
  summarise(
    avg_medals = mean(total_medals, na.rm = TRUE),
    avg_gdppc = mean(gdppc, na.rm = TRUE)
  )


ggplot(data = data_plot, aes(x = year)) +
  geom_line(aes(y = avg_medals, color = "Total Medals"), linewidth = 1.2) +
  geom_line(aes(
    y = avg_gdppc / max(avg_gdppc, na.rm = TRUE) * max(avg_medals, na.rm = TRUE),
    color = "GDP per Capita"
  ), linewidth = 1.2) +
  scale_y_continuous(
    name = "Average Total Medals",
    sec.axis = sec_axis(~ . * max(data_plot$avg_gdppc, na.rm = TRUE) / max(data_plot$avg_medals, na.rm = TRUE), 
                        name = "GDP per Capita")
  ) +
  labs(
    title = "Tendances temporelles des médailles et du PIB",
    x = "Year",
    color = "Variable"
  ) +
  theme_minimal() +
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
    legend.position = "top"
  )
  
