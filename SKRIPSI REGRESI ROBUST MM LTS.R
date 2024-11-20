# Load required library
library(readxl)
library(Hmisc)
library(lmtest)
library(car)
library(ggplot2)
library(robust)

# Load Data
Data <- read_excel("E:/SKRIPSI/Skripsi.xlsx")
# Display the first few rows of the data after conversion
head(Data)

# Analisis Deksriptif
summary(Data)
describe(Data)
# Menghitung dan menampilkan standar deviasi, variansi, dan rentang untuk setiap variabel
# Variabel numerik yang akan dianalisis
variables <- c("PDRB", "Pendapatan Asli Daerah", "Tenaga Kerja", "APS SMA", "APS PT", "PMA", "IPM")
for (var in variables) {
  cat(paste("\nAnalisis Deskriptif untuk", var, "\n"))
  
  sd_val <- sd(Data[[var]], na.rm = TRUE)
  var_val <- var(Data[[var]], na.rm = TRUE)
  range_val <- range(Data[[var]], na.rm = TRUE)
  range_diff <- diff(range_val)
  
  cat(paste("Standar Deviasi:", sd_val, "\n"))
  cat(paste("Variansi:", var_val, "\n"))
  cat(paste("Range:", range_val, "\n"))
  cat(paste("Selisih Range:", range_diff, "\n"))

}
# Check for missing values
missing_values <- sapply(Data, function(x) sum(is.na(x)))
print(missing_values)

# Fit a linear model to each numeric column
Model.MKT <- lm(PDRB ~`Pendapatan Asli Daerah`+`Tenaga Kerja` + `APS SMA` + `APS PT` + `PMA` + `IPM`, data = Data)
summary(Model.MKT)

#Uji Asumsi
#Uji Normalitas 
ks.test(Model.MKT$residuals,"pnorm")
#Uji Homokedasitas
bptest(Model.MKT)
# Uji Non Autokorelasi
dwtest(Model.MKT)
#Uji Non Multikolinearitas VIF
vif(Model.MKT)
#Deteksi Pencilan
dffits <- dffits(Model.MKT)
# Identifikasi outlier
outlier_indices <- which(dffits > quantile(dffits, 0.75) + 1.5 * IQR(dffits))
boxplot(dffits, main = "Boxplot of DFFITS", ylab = "DFFITS Values")
# Menambahkan label pada outlier
text(x = rep(1, length(outlier_indices)), 
     y = dffits[outlier_indices], 
     labels = outlier_indices, 
     pos = 4, 
     col = "red")

plot(Model.MKT$residuals)

# Install and load required libraries
if (!requireNamespace("robustbase", quietly = TRUE)) {
  install.packages("robustbase")
}
library(robustbase)

# Load required library
library(MASS)

# Fit a robust linear model using MM (M-estimation with Tukey-Bisquare weights)
model.MM <- lmrob(PDRB ~`Pendapatan Asli Daerah`+`Tenaga Kerja` +`APS SMA` + `APS PT` + `PMA` + `IPM`, data = Data, method = "MM")
# Display summary of the robust regression model
summary(model.MM)
iterasi <- model.MM$iter
model.MM$coefficients
model.MM$rweights
model.MM$iter
model.MM$init
model.MM$fitted.values
plot(model.MM$residuals)

# Nilai R-squared dari model MM
R2 <- 0.9873
# Jumlah pengamatan
n <- nrow(Data)
# Jumlah prediktor
p <- length(coef(model.MM)) - 1

# Uji F-statistic
F_statistic <- (R2 / (1 - R2)) * ((n - p - 1) / p)
F_statistic

# p-value dari uji F
p_value <- pf(F_statistic, p, n - p - 1, lower.tail = FALSE)
p_value


# Model LTS
residuals
residuals^2
model.LTS <- ltsReg(PDRB ~`Pendapatan Asli Daerah` +`Tenaga Kerja` + `APS SMA` + `APS PT` + `PMA` + `IPM`, data = Data, method = "lts")
summary(model.LTS)
model.LTS$raw.weights
model.LTS$lts.wt
model.LTS$residuals
model.LTS$residuals^2
model.LTS$best
model.LTS$crit
model.LTS$quan

