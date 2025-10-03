#########VIGTIG##############
#######Koden fra opgave 2 skal være kørt først#######################
###########OPGAVE3.1#####################
#Er der vækst ned eller op
PF2 <- PF[-(1:4), ]
PF2$pfveakst <- diff(log(PF$`P.31 Privatforbrug`), lag = 4) * 100
PF2$veakst <- as.integer(PF2$pfveakst>0)
PF2$veakst <- factor(PF2$veakst, levels = c(0,1), labels = c("ned","op"))
summary(PF2$veakst)

######################OPG3.2###############
# 1) Dummy for årlig vækst (1 = op, 0 = ned)
y <- as.integer(pfveakst > 0)

# 2) Forklarende variable: DI's spørgsmål og DST's spørgsmål
n <- length(y)
dat <- data.frame(
  spg1 = spg1[1:n],
  spg2 = spg2[1:n],
  spg3 = spg3[1:n],
  spg4 = spg4[1:n],
  spg5 = spg5[1:n],
  spg6 = spg6[1:n],
  y    = y
)

# 3) Logistisk regression for DI
sam_DI_spg <- c(spg1+spg3+spg5+spg6)/4
sam_log_DI <- glm(y ~ sam_DI_spg, data = dat, family = binomial())
PF2$pred_DI <- predict(sam_log_DI, type = "response")
pred_DI <- ifelse(PF2$pred_DI >= 0.5, 1, 0)

table_DI <- table(faktisk = dat$y, forudsigelse = pred_DI)
acc_DI <- mean(pred_DI == dat$y)

table_DI
acc_DI
####################

# 3) Logistisk regression for DST
sam_DST_spg <- (spg1 + spg2 + spg3 + spg4 + spg5) / 5
sam_log_DST <- glm(y ~ sam_DST_spg, data = dat, family = binomial())
PF2$pred_DST <- predict(sam_log_DST, type = "response")
pred_DST <- ifelse(PF2$pred_DST >= 0.5, 1, 0)

table_DST <- table(faktisk = dat$y, forudsigelse = pred_DST)
acc_DST <- mean(pred_DST == dat$y)

table_DST
acc_DST

##########NØGLETAL######
# DI
acc_DI  <- mean(pred_DI == dat$y)
prec_DI <- sum(pred_DI==1 & dat$y==1) / sum(pred_DI==1)
rec_DI  <- sum(pred_DI==1 & dat$y==1) / sum(dat$y==1)         # recall/sensitivitet
spec_DI <- sum(pred_DI==0 & dat$y==0) / sum(dat$y==0)         # specificitet
f1_DI   <- 2*prec_DI*rec_DI/(prec_DI+rec_DI)

# DST (samme mønster)
acc_DST  <- mean(pred_DST == dat$y)
prec_DST <- sum(pred_DST==1 & dat$y==1) / sum(pred_DST==1)
rec_DST  <- sum(pred_DST==1 & dat$y==1) / sum(dat$y==1)
spec_DST <- sum(pred_DST==0 & dat$y==0) / sum(dat$y==0)
f1_DST   <- 2*prec_DST*rec_DST/(prec_DST+rec_DST)


#####Her kigge på nøgletal for DI
acc_DI
prec_DI
rec_DI
spec_DI
f1_DI 
#####Her kigge på nøgletal for dst
acc_DST
prec_DST
rec_DST
spec_DST
f1_DST 

# 4) Forudsig for 2025K3 ved kun at give K3-værdierne
new_K3 <- data.frame(
  spg1 = DIK3_spg1,
  spg2 = DIK3_spg2,
  spg3 = DIK3_spg3,
  spg4 = DIK3_spg4,
  spg5 = DIK3_spg5,
  spg6 = DIK3_spg6
)

## DI-forudsigelse for 2025K3
ny_DI_K3 <- data.frame(
  sam_DI_spg = (DIK3_spg1 + DIK3_spg3 + DIK3_spg5 + DIK3_spg6) / 4
)
p_DI_K3 <- predict(sam_log_DI, newdata = ny_DI_K3, type = "response")
retning_DI <- ifelse(p_DI_K3 >= 0.5, "op", "ned")
retning_DI

## DST-forudsigelse for 2025K3
ny_DST_K3 <- data.frame(
  sam_DST_spg = (DIK3_spg1 + DIK3_spg2 + DIK3_spg3 + DIK3_spg4 + DIK3_spg5) / 5
)
p_DST_K3 <- predict(sam_log_DST, newdata = ny_DST_K3, type = "response")
retning_DST <- ifelse(p_DST_K3 >= 0.5, "op", "ned")
retning_DST

######################OPGAVE3.4##################
########################################
# Finder bedste spørgsmål
sam_bedst_spg <- (spg3) / 1
sam_log_bedst <- glm(y ~ sam_bedst_spg, data = dat, family = binomial())
PF2$pred_bedst <- predict(sam_log_bedst, type = "response")
pred_bedst <- ifelse(PF2$pred_bedst >= 0.5, 1, 0)

table_bedst <- table(faktisk = dat$y, forudsigelse = pred_bedst)
acc_bedst <- mean(pred_bedst == dat$y)

table_bedst
acc_bedst

#####Prøver at lave den mest optimale kombinations model

sam_opti_spg <- (spg3+ spg5) / 2
sam_log_opti <- glm(y ~ sam_opti_spg, data = dat, family = binomial())
PF2$pred_opti <- predict(sam_log_opti, type = "response")
pred_opti <- ifelse(PF2$pred_opti >= 0.5, 1, 0)

table_opti <- table(faktisk = dat$y, forudsigelse = pred_opti)
acc_opti <- mean(pred_opti == dat$y)

table_opti
acc_opti

######Ændre i vores tærskel i DI Forbrugertillid så rammer alle op

sam_DI_spg <- c(spg1+spg3+spg5+spg6)/4
sam_log_DI <- glm(y ~ sam_DI_spg, data = dat, family = binomial())
PF2$pred_DI <- predict(sam_log_DI, type = "response")
pred_DI <- ifelse(PF2$pred_DI >= 0.36, 1, 0)

table_DI <- table(faktisk = dat$y, forudsigelse = pred_DI)
acc_DI <- mean(pred_DI == dat$y)

table_DI
acc_DI


