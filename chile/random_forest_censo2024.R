libraries <- c("tidyverse", "fastDummies", "cluster", "poLCA", "ranger", "ggplot2", "vip", "pROC")
lapply(libraries, library, character.only = TRUE)
set.seed(666)

rm(libraries)

#-------------------------------------------------------------#
# Data Base adaptation                                        #
# Final name: chi_couples2024                                 #
#-------------------------------------------------------------#

# 1. LIMPIEZA Y RECODIFICACIÓN INDIVIDUAL
censo_recod <- personas_censo2024 %>%
  # Seleccionar variables necesarias
  dplyr::select(id_vivienda, id_hogar, id_persona, sexo, edad, area, parentesco,
                p25_lug_nacimiento_rec, p31_religion, p31_religion_rec,
                educ_cat, sit_fuerza_trabajo, cod_ciuo, p46a_tot_hijs_nac) %>%
  
  # Limpiar valores especiales
  mutate(across(where(is.numeric), ~if_else(. %in% c(-99, -66, 999), NA_real_, .))) %>%
  
  # Eliminar casos con NA críticos
  drop_na(id_vivienda:id_hogar, sexo, edad, area, parentesco, p25_lug_nacimiento_rec,
          educ_cat, sit_fuerza_trabajo, cod_ciuo) %>%
  
  # RECODIFICACIÓN
  mutate(
    native = case_when(
      p25_lug_nacimiento_rec == 1 ~ "Native",
      p25_lug_nacimiento_rec == 2 ~ "Nonnative",
      TRUE ~ NA_character_
    ),
    area = case_when(
      area == 1 ~ "Urbano",
      area == 2 ~ "Rural",
      TRUE ~ NA_character_
    ),
    educ_cat = case_when(
      educ_cat == "College or More" ~ "Universitario",
      educ_cat %in% c("Less than primary", "Primary", "Secondary") ~ "No Universitario",
      TRUE ~ NA_character_
    ),
    sit_fuerza_trabajo = case_when(
      sit_fuerza_trabajo == 1 ~ "Ocupado",
      sit_fuerza_trabajo %in% c(2, 3) ~ "Desocupado",
      TRUE ~ NA_character_
    ),
    mother = case_when(
      sexo == 2 & !is.na(p46a_tot_hijs_nac) & p46a_tot_hijs_nac > 0 ~ 1,
      sexo == 2 & !is.na(p46a_tot_hijs_nac) & p46a_tot_hijs_nac == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    ocupation = case_when(
      cod_ciuo %in% c(1, 2) ~ "Profesional o Directivo",
      cod_ciuo %in% c(3, 4, 5, 0) ~ "Técnico o Servicio",
      cod_ciuo %in% c(6, 7, 8) ~ "Obrero",
      TRUE ~ NA_character_
    ),
    
    ocupation_gt = case_when(
      cod_ciuo %in% c(1, 2) ~ "I. Clase Alta de Servicios",
      cod_ciuo == 3 ~ "II. Clase Media de Servicios",
      cod_ciuo %in% c(4,5,10) ~ "III. Rutina no manual",
      cod_ciuo == 6 ~ "IV. Pequeña Burguesía",
      cod_ciuo %in% c(7,8) ~ "VI. Trabajos Manuales clasificados",
      cod_ciuo == 9 ~ "VII. Trabajos Manuales no clasificados"
    )
  ) 

# 1. IDENTIFICAR PAREJAS
# jefes de hogar (parentesco = 1)
jefes_hogar <- censo_recod %>%
  filter(parentesco == 1) %>%
  dplyr::select(id_vivienda, id_hogar, id_persona, sexo, edad, area,
                educ_cat, sit_fuerza_trabajo, ocupation, ocupation_gt, mother,
                native, parentesco, p31_religion)

# conyuges (parentesco 2, 3, 4)
conyuges <- censo_recod %>%
  filter(parentesco %in% c(2, 3, 4)) %>%
  dplyr::select(id_vivienda, id_hogar, id_persona, sexo, edad, area,
                educ_cat, sit_fuerza_trabajo, ocupation, ocupation_gt, mother,
                native, parentesco, p31_religion)

# Unir bases
parejas <- inner_join(jefes_hogar, conyuges, 
                      by = c("id_vivienda", "id_hogar"),
                      suffix = c("_jefe", "_conyugue"))

# 2. BASE chi_couples2024 
chi_couples2024 <- parejas %>%
  # ID del hogar
  mutate(
    # id_hh: id_vivienda_id_hogar_id_persona_jefe
    id_hh = paste(id_vivienda, id_hogar, id_persona_jefe, sep = "_"),
    # Partner 1 (jefe de hogar)--------------------------------------------------------------#
    id_pp_partner1 = paste(id_vivienda, id_hogar, id_persona_jefe, "1", sep = "_"),
    sex_partner1 = sexo_jefe,
    age_partner1 = edad_jefe,
    relationhead_partner1 = parentesco_jefe,
    native_partner1 = native_jefe,
    area_partner1 = area_jefe,
    esc_cat_partner1 = educ_cat_jefe,
    status_wf_partner1 = sit_fuerza_trabajo_jefe,
    ocupation_partner1 = ocupation_jefe,
    ocupation_gt_partner1 = ocupation_gt_jefe,
    mother_partner1 = mother_jefe,
    religion_partner1 = p31_religion_jefe,
    
    # Partner 2 (conyugue)-------------------------------------------------------------------#
    id_pp_partner2 = paste(id_vivienda, id_hogar, id_persona_conyugue, "2", sep = "_"),
    sex_partner2 = sexo_conyugue,
    age_partner2 = edad_conyugue,
    relationhead_partner2 = parentesco_conyugue,
    native_partner2 = native_conyugue,
    area_partner2 = area_conyugue,
    esc_cat_partner2 = educ_cat_conyugue,
    status_wf_partner2 = sit_fuerza_trabajo_conyugue,
    ocupation_partner2 = ocupation_conyugue,
    ocupation_gt_partner2 = ocupation_gt_conyugue,
    mother_partner2 = mother_conyugue,
    religion_partner2 = p31_religion_conyugue
  )

chi_couples2024 <- chi_couples2024 %>% mutate(
  orientation_couple = if_else(sex_partner1 != sex_partner2, "Heterosexual", "Homosexual")
) %>%  filter(orientation_couple == "Heterosexual") 

#-------------------------------------------------------------#
# Partner role harmonization                                  #
# mp = male partner | fp = female partner                     #
#-------------------------------------------------------------#

vars <- c("age", "native", "area", "esc_cat", "status_wf", 
          "ocupation", "mother", "religion", "ocupation_gt")
chi_couples2024_g <- chi_couples2024
new_vars <- character(0)

for (v in vars) {
  
  v_p1 <- paste0(v, "_partner1")
  v_p2 <- paste0(v, "_partner2")
  
  chi_couples2024_g <- chi_couples2024_g %>%
    mutate(
      # Assign male/female partner values explicitly
      !!paste0(v, "_mp") := if_else(sex_partner1 == 1,
                                    .data[[v_p1]],
                                    .data[[v_p2]]),
      !!paste0(v, "_fp") := if_else(sex_partner1 == 2,
                                    .data[[v_p1]],
                                    .data[[v_p2]])
    )
  
  new_vars <- c(new_vars, paste0(v, c("_mp", "_fp")))
}

#-------------------------------------------------------------#
# Couple-level constructed measures                           #
#-------------------------------------------------------------#

chi_couples2024_g <- chi_couples2024_g %>%
  mutate(
    age_diff = age_mp - age_fp,
    age_diff = case_when(
      age_diff %in% c(-1,0,1) ~ "Misma Edad",
      age_diff > 1 ~ "Hombre Mayor",
      age_diff < -1 ~ "Mujer Mayor"
    ), 
    educ_cat_couple = paste(esc_cat_mp,  esc_cat_fp,  sep = "-"), 
    native_couple  = paste(native_mp,  native_fp,  sep = "-"),
    religion_couple = case_when(
      religion_mp != religion_fp ~ "Distinta Religion",
      religion_mp == 12 & religion_fp == 12 ~ "Sin Religion",
      religion_mp == religion_fp & religion_mp != 12 ~ "Misma Religion"
    ),
    status_wf_couple = paste(status_wf_mp, status_wf_fp, sep = "-"),
    ocupation_couple = paste(ocupation_mp, ocupation_fp, sep = "-"),
    ocupation_gt_couple = paste(ocupation_gt_mp, ocupation_gt_mp, sep = "-"),
    children_couple = if_else(mother_fp == 1, "Con Hijos", "Sin Hijos"),
    area_couple = area_jefe
  )

chi_couples_stats <- chi_couples2024_g %>%
  dplyr::select(age_diff, educ_cat_couple, native_couple, religion_couple, 
                status_wf_couple, ocupation_couple, children_couple, area_couple, ocupation_gt_couple)
#clean
rm(v, v_p1, v_p2, vars, new_vars)

#-------------------------------------------------------------#
# Dummy encoding for clustering                               #
#-------------------------------------------------------------#

filter <- c("Native-Nonnative", "Nonnative-Native", "Nonnative-Nonnative") #couple types filter

chi_clust <- chi_couples_stats %>% 
  filter(native_couple %in% filter) %>%
  drop_na() %>%
  dplyr::select(age_diff, educ_cat_couple, native_couple, religion_couple, 
                status_wf_couple, ocupation_couple, children_couple, area_couple) %>%
  dummy_cols(
    select_columns = c("native_couple", "age_diff", "educ_cat_couple", "religion_couple",
                       "status_wf_couple", "ocupation_couple", "children_couple", "area_couple"),
    remove_selected_columns = TRUE,
    remove_first_dummy = FALSE
  )
#-------------------------------------------------------------#
# Elbow test k-means                                          #
#-------------------------------------------------------------#

max_k <- 10
wcss <- numeric(max_k) # Within-Cluster Sum of Squares

for (k in 1:max_k) {
  set.seed(998213)
  model_kmeans <- kmeans(chi_clust, centers = k, nstart = 25)
  wcss[k] <- model_kmeans$tot.withinss
}

#VISUALIZACIÓN 
results <- data.frame(k = 1:max_k, wcss = wcss)

ggplot(results, aes(x = k, y = wcss)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  labs(title = "Elbow Test",
       x = "Number of clusters (k)",
       y = "WCSS") +
  theme_minimal() +
  scale_x_continuous(breaks = 1:max_k)

#-------------------------------------------------------------#
# K-means clustering (exclude migrant/ethnic structure)       #
#-------------------------------------------------------------#
k <- 5 #ver si lo cambiamos segun elbowtest========================

km_data <- chi_clust %>%
  drop_na()

km <- kmeans(
  km_data,
  centers = k,
  nstart = 25
)

chi_clust <- chi_clust %>%
  drop_na() %>%
  mutate(cluster = factor(km$cluster))

# Cluster profiles (means)
chi_clust %>%
  group_by(cluster) %>%
  summarise(across(everything(), ~mean(.x, na.rm = TRUE))) %>%
  as.matrix() %>%
  t()

#% per cluster 
chi_clust %>%
  group_by(cluster) %>%
  summarise(n = n()) %>%
  mutate(perc = n / sum(n) * 100)

#-------------------------------------------------------------#
# Random forest: predicting native couple type                #
#-------------------------------------------------------------#
chi_forest <- chi_couples_stats %>% 
  filter(native_couple %in% filter) %>%
  drop_na()

idx <- sample(seq_len(nrow(chi_forest)), size = 0.7 * nrow(chi_forest))
train <- chi_forest[idx, ]
test  <- chi_forest[-idx, ]

train_rf <- train %>%
  dplyr::select(age_diff, educ_cat_couple, native_couple, religion_couple, 
                status_wf_couple, ocupation_couple, children_couple, area_couple) %>%
  mutate(native_couple = as.factor(native_couple))

rf <- ranger(
  x = train_rf %>% dplyr::select(-native_couple),
  y = train_rf$native_couple,
  probability    = TRUE,
  num.trees      = 1000, 
  mtry           = 4, 
  min.node.size  = 50,
  importance     = "impurity",
  seed           = 998213
)
#-------------------------------------------------------------#
# Random forest: variable importance visualization            #
#-------------------------------------------------------------#
print(rf)

imp_df <- data.frame(
  variable = names(rf$variable.importance),
  importance = rf$variable.importance
) %>% arrange(desc(importance))

print(imp_df)

#VISUALIZACIÓN 
ggplot(imp_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Importancia de Variables - Random Forest",
       x = "Variable",
       y = "Importancia (disminución de impureza)") +
  theme_minimal()


