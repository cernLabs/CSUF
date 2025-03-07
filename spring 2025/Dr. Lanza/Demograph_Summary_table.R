# generate summary table

#### Summaries Table
Q1 <- demographics %>% 
  group_by(`Group ( 1-young, 2-older adults)`) %>%
  summarise(
    count = n(),
    age = paste0(round(mean(`Age (years old)`, na.rm = TRUE), 2), ' (', round(sd(`Age (years old)`, na.rm = TRUE), 2), ')'),
    male_count = paste0(sum(`Sex (1-male , 2-female)` == 1), '/', round((sum(`Sex (1-male , 2-female)` == 1) / count) * 100, 2), '%'),
    female_count = paste0(sum(`Sex (1-male , 2-female)` == 2), '/', round((sum(`Sex (1-male , 2-female)` == 2) / count) * 100, 2), '%'),
    right_dominant = paste0(sum(`Dominant Side (1- right, 2 -left)` == 1), '/', round((sum(`Dominant Side (1- right, 2 -left)` == 1) / count) * 100, 2), '%'),
    left_dominant = paste0(sum(`Dominant Side (1- right, 2 -left)` == 2), '/', round((sum(`Dominant Side (1- right, 2 -left)` == 2) / count) * 100, 2), '%'),
    Four_Square_Step_Test = paste0(round(mean(`4SST (time - s)`, na.rm = TRUE), 2), ' (', round(sd(`4SST (time - s)`, na.rm = TRUE), 2), ')'),
    Handgrip_Strength = paste0(round(mean(`Handgrip Strenght Test (kg.f)`, na.rm = TRUE), 2), ' (', round(sd(`Handgrip Strenght Test (kg.f)`, na.rm = TRUE), 2), ')')
  )

Q2 <- demographics %>% 
  summarise(
    count = n(),
    age = paste0(round(mean(`Age (years old)`, na.rm = TRUE), 2), ' (', round(sd(`Age (years old)`, na.rm = TRUE), 2), ')'),
    male_count = paste0(sum(`Sex (1-male , 2-female)` == 1), '/', round((sum(`Sex (1-male , 2-female)` == 1) / count) * 100, 2), '%'),
    female_count = paste0(sum(`Sex (1-male , 2-female)` == 2), '/', round((sum(`Sex (1-male , 2-female)` == 2) / count) * 100, 2), '%'),
    right_dominant = paste0(sum(`Dominant Side (1- right, 2 -left)` == 1), '/', round((sum(`Dominant Side (1- right, 2 -left)` == 1) / count) * 100, 2), '%'),
    left_dominant = paste0(sum(`Dominant Side (1- right, 2 -left)` == 2), '/', round((sum(`Dominant Side (1- right, 2 -left)` == 2) / count) * 100, 2), '%'),
    Four_Square_Step_Test = paste0(round(mean(`4SST (time - s)`, na.rm = TRUE), 2), ' (', round(sd(`4SST (time - s)`, na.rm = TRUE), 2), ')'),
    Handgrip_Strength = paste0(round(mean(`Handgrip Strenght Test (kg.f)`, na.rm = TRUE), 2), ' (', round(sd(`Handgrip Strenght Test (kg.f)`, na.rm = TRUE), 2), ')')
  )
######### Making table
# Extract the P-value
P_vals = c(
  '-',
  round(t.test(`Age (years old)` ~ `Group ( 1-young, 2-older adults)`, data = demographics)$p.value,4),
  '-',
  '-',
  '-',
  '-',
  round(t.test(`4SST (time - s)` ~ `Group ( 1-young, 2-older adults)`, data = demographics)$p.value,4),
  round(t.test(`Handgrip Strenght Test (kg.f)` ~ `Group ( 1-young, 2-older adults)`, data = demographics)$p.value,4)
)

######### T-Test
Q = t(rbind(Q2,Q1[,2:9],P_vals))
colnames(Q) = c("All Participants","Younger","Older","P-Vals")
library(kableExtra)
kable(Q)

