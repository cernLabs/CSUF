
muscle_data$Z_SOL_Thickness <- (muscle_data$`Sol_Thickness(cm)` - mean(muscle_data$`Sol_Thickness(cm)`, na.rm =1))/sd(muscle_data$`Sol_Thickness(cm)`, na.rm =1)
