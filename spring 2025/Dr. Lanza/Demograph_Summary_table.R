#### Summaries Table
Q1 <- demographics %>% 
  group_by(`Group ( 1-young, 2-older adults)`) %>%
  summarise(
    count = n(),
    age = paste0(round(mean(`Age (years old)`, na.rm = TRUE), 2), ' (', round(sd(`Age (years old)`, na.rm = TRUE), 2), ')'),
    `female count` = paste0(sum(`Sex (1-male , 2-female)` == 2), '/', round((sum(`Sex (1-male , 2-female)` == 2) / count) * 100, 2), '%'),
    `left dominant` = paste0(sum(`Dominant Side (1- right, 2 -left)` == 2), '/', round((sum(`Dominant Side (1- right, 2 -left)` == 2) / count) * 100, 2), '%'),
    `Four Square Step Test` = paste0(round(mean(`4SST (time - s)`, na.rm = TRUE), 2), ' (', round(sd(`4SST (time - s)`, na.rm = TRUE), 2), ')'),
    `Handgrip Strength` = paste0(round(mean(`Handgrip Strenght Test (kg.f)`, na.rm = TRUE), 2), ' (', round(sd(`Handgrip Strenght Test (kg.f)`, na.rm = TRUE), 2), ')')
  )

Q2 <- demographics %>% 
  summarise(
    count = n(),
    age = paste0(round(mean(`Age (years old)`, na.rm = TRUE), 2), ' (', round(sd(`Age (years old)`, na.rm = TRUE), 2), ')'),
    `female count` = paste0(sum(`Sex (1-male , 2-female)` == 2), '/', round((sum(`Sex (1-male , 2-female)` == 2) / count) * 100, 2), '%'),
    `left dominant` = paste0(sum(`Dominant Side (1- right, 2 -left)` == 2), '/', round((sum(`Dominant Side (1- right, 2 -left)` == 2) / count) * 100, 2), '%'),
    `Four Square Step Test` = paste0(round(mean(`4SST (time - s)`, na.rm = TRUE), 2), ' (', round(sd(`4SST (time - s)`, na.rm = TRUE), 2), ')'),
    `Handgrip Strength` = paste0(round(mean(`Handgrip Strenght Test (kg.f)`, na.rm = TRUE), 2), ' (', round(sd(`Handgrip Strenght Test (kg.f)`, na.rm = TRUE), 2), ')')
  )
######### Making table
# Extract the P-value

XS1_pv <- function(col1,col2){
  sum(col1 == 1) -> s11
  sum(col1 == 2) -> s12
  sum(col2 == 1) -> s21
  sum(col2 == 2) -> s22
  M = matrix(c(s11,s12,s21,s22), byrow = TRUE)
  return(chisq.test(M)$p.value)
}
P_vals = c(
  '-',
  signif(t.test(`Age (years old)` ~ `Group ( 1-young, 2-older adults)`, data = demographics)$p.value,4),
  signif(XS1_pv(demographics$`Sex (1-male , 2-female)`,demographics$`Age (years old)`),4),
  signif(XS1_pv(demographics$`Dominant Side (1- right, 2 -left)`,demographics$`Age (years old)`),4),
  signif(t.test(`4SST (time - s)` ~ `Group ( 1-young, 2-older adults)`, data = demographics)$p.value,4),
  signif(t.test(`Handgrip Strenght Test (kg.f)` ~ `Group ( 1-young, 2-older adults)`, data = demographics)$p.value,4)
)

######### T-Test
Q = t(rbind(Q2,Q1[,2:7],P_vals))
colnames(Q) = c("All Participants","Younger","Older","P-Vals")
library(kableExtra)
kable(Q)
