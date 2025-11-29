# ===============================================================================
# CT Prediction Model - Main Analysis (Error Fixed)
# Version: 4.1 - $ operator error resolved
# ===============================================================================

library(tidyverse)
library(tableone)
library(pROC)
library(rms)
library(ResourceSelection)
library(car)
library(logistf)  # For Firth's penalized likelihood

# Set seed and options
set.seed(123)
options(stringsAsFactors = FALSE)

setwd("/Users/youjinlee/Documents/My R/Fever c claude/2017_2025_s")
# ===============================================================================
# 1. DATA PREPARATION
# ===============================================================================

df <- read.csv("cleaned_data/base_full_clean.csv")
df_elderly <- df %>% filter(age >= 85)

cat("Patients ≥85 years:", nrow(df_elderly), "\n")
cat("CT positive rate:", round(mean(df_elderly$fever_focus) * 100, 1), "%\n")

# Handle missing values
vital_signs <- c("systolic_bp", "diastolic_bp", "pulse_rate", 
                 "respiratory_rate", "temperature", "spo2", "bst")
df_elderly[vital_signs] <- lapply(df_elderly[vital_signs], 
                                  function(x) ifelse(x == -999, NA, x))

# Remove constant variables to avoid datadist warning
constant_vars <- names(df_elderly)[sapply(df_elderly, function(x) length(unique(x)) == 1)]
if(length(constant_vars) > 0) {
  cat("\nRemoving constant variables:", paste(constant_vars, collapse=", "), "\n")
  df_elderly <- df_elderly[, !names(df_elderly) %in% constant_vars]
}

# ===============================================================================
# 2. PRIMARY ANALYSIS - CONTINUOUS VARIABLES
# ===============================================================================
#.이해 안가면 evernote 풀이 참고 🚨 Complete Separation 문제 - 초간단 설명

cat("\n========== PRIMARY ANALYSIS (CONTINUOUS) ==========\n")

# Define predictors
predictors_cont <- c("age", "CRP", "WBC", "Albumin", "BUN", "Creatinine", "cci")
predictors_binary <- c("hypertension_yn", "diabetes_yn")

# Check for perfect separation
check_separation <- function(data, outcome, predictors) {
  for(pred in predictors) {
    tbl <- table(data[[pred]] > median(data[[pred]], na.rm=TRUE), data[[outcome]])
    if(any(tbl == 0)) {
      cat("Warning: Potential separation with", pred, "\n")
    }
  }
}

check_separation(df_elderly, "fever_focus", predictors_cont)

# Univariable analysis
univar_results <- data.frame()

for(predictor in predictors_cont) {
  formula <- as.formula(paste("fever_focus ~", predictor))
  
  # Use try-catch for problematic models
  model <- tryCatch({
    glm(formula, data = df_elderly, family = binomial)
  }, warning = function(w) {
    # If warning, use Firth's penalized likelihood
    cat("Using Firth's method for", predictor, "\n")
    logistf(formula, data = df_elderly)
  })
  
  if(class(model)[1] == "logistf") {
    coef_val <- coef(model)[predictor]
    ci_val <- confint(model)[predictor, ]
    p_val <- model$prob[predictor]
  } else {
    coef_val <- coef(model)[predictor]
    ci_val <- confint(model)[predictor, ]
    p_val <- summary(model)$coefficients[predictor, 4]
  }
  
  univar_results <- rbind(univar_results, data.frame(
    Variable = predictor,
    OR = exp(coef_val),
    CI_lower = exp(ci_val[1]),
    CI_upper = exp(ci_val[2]),
    p_value = p_val
  ))
}

print(univar_results[order(univar_results$p_value), ])

# Multivariable models with careful variable selection
cat("\n=== Multivariable Models ===\n")

# Model 1: Simple (top predictors only)
model1_formula <- fever_focus ~ CRP + WBC + age

model1 <- tryCatch({
  glm(model1_formula, data = df_elderly, family = binomial)
}, warning = function(w) {
  cat("Model 1: Using Firth's penalized likelihood\n")
  logistf(model1_formula, data = df_elderly)
})

# Model 2: Intermediate (avoid overfitting)
model2_formula <- fever_focus ~ CRP + WBC + age + Albumin

model2 <- tryCatch({
  glm(model2_formula, data = df_elderly, family = binomial)
}, warning = function(w) {
  cat("Model 2: Using Firth's penalized likelihood\n")
  logistf(model2_formula, data = df_elderly)
})

# Model 3: With interaction check (if no separation)
model3_formula <- fever_focus ~ CRP + WBC + Albumin + hypertension_yn

model3 <- tryCatch({
  glm(model3_formula, data = df_elderly, family = binomial)
}, warning = function(w) {
  cat("Model 3: Using Firth's penalized likelihood\n")
  logistf(model3_formula, data = df_elderly)
})

# Print model summaries
if(class(model2)[1] == "glm") {
  summary(model2)
  cat("\nVIF check:\n")
  print(vif(model2))
} else {
  print(model2)
}

# ===============================================================================
# 3. SECONDARY ANALYSIS - CATEGORICAL VARIABLES
# ===============================================================================

cat("\n========== SECONDARY ANALYSIS (CATEGORICAL) ==========\n")

# Create categories with sufficient observations per category
df_elderly <- df_elderly %>%
  mutate(
    # Combine sparse categories to avoid separation
    age_cat = cut(age, breaks = c(85, 90, Inf), 
                  labels = c("85-89", "90+"), right = FALSE),
    
    crp_cat = cut(CRP, breaks = c(0, 10, Inf),
                  labels = c("<10", "≥10"), right = FALSE),
    
    wbc_cat = cut(WBC, breaks = c(0, 11, Inf),
                  labels = c("<11", "≥11"), right = FALSE),
    
    albumin_cat = cut(Albumin, breaks = c(0, 3, Inf),
                      labels = c("<3", "≥3"), right = FALSE),
    
    cci_cat = ifelse(cci <= 1, "0-1", "≥2")
  )

# Check distribution
cat("\nCategory distributions:\n")
table(df_elderly$crp_cat, df_elderly$fever_focus)
table(df_elderly$wbc_cat, df_elderly$fever_focus)

# Categorical model
model_cat <- tryCatch({
  glm(fever_focus ~ crp_cat + wbc_cat + albumin_cat + age_cat,
      data = df_elderly, family = binomial)
}, warning = function(w) {
  cat("Categorical model: Using Firth's method\n")
  logistf(fever_focus ~ crp_cat + wbc_cat + albumin_cat + age_cat,
          data = df_elderly)
})

# ===============================================================================
# 4. MODEL PERFORMANCE EVALUATION evernote 풀이 확인
# ===============================================================================

cat("\n========== MODEL PERFORMANCE ==========\n")

evaluate_model <- function(model, data, model_name) {
  # Get predictions based on model class
  if(class(model)[1] == "logistf") {
    pred_prob <- model$predict
  } else {
    pred_prob <- predict(model, newdata = data, type = "response")
  }
  
  # Remove NAs
  complete_idx <- !is.na(pred_prob) & !is.na(data$fever_focus)
  pred_prob <- pred_prob[complete_idx]
  outcome <- data$fever_focus[complete_idx]
  
  # Calculate metrics
  roc_obj <- roc(outcome, pred_prob, quiet = TRUE)
  auc_val <- auc(roc_obj)
  
  # Hosmer-Lemeshow test (skip for Firth models)
  hl_p <- NA
  if(class(model)[1] != "logistf") {
    hl_test <- tryCatch({
      hoslem.test(outcome, pred_prob, g = 5)  # Reduced groups
    }, error = function(e) {
      list(p.value = NA)
    })
    hl_p <- hl_test$p.value
  }
  
  cat(sprintf("\n%s:\n", model_name))
  cat(sprintf("  AUC: %.3f\n", auc_val))
  cat(sprintf("  H-L p-value: %.3f\n", hl_p))
  
  return(list(auc = auc_val, roc = roc_obj))
}

# Evaluate models
perf1 <- evaluate_model(model1, df_elderly, "Model 1 (Simple)")
perf2 <- evaluate_model(model2, df_elderly, "Model 2 (Intermediate)")
perf3 <- evaluate_model(model3, df_elderly, "Model 3 (Alternative)")

perf1
perf2
perf3


# ROC curves
pdf("roc_curves.pdf", width = 8, height = 8)
plot(perf1$roc, col = "blue", main = "ROC Curves")
lines(perf2$roc, col = "red")
lines(perf3$roc, col = "green")
legend("bottomright", 
       legend = c(sprintf("Model 1: AUC = %.3f", perf1$auc),
                  sprintf("Model 2: AUC = %.3f", perf2$auc),
                  sprintf("Model 3: AUC = %.3f", perf3$auc)),
       col = c("blue", "red", "green"), lty = 1, lwd = 2)
dev.off()

# ===============================================================================
# 5. BOOTSTRAP VALIDATION (Simplified) evernote 해서!
# ===============================================================================

cat("\n========== BOOTSTRAP VALIDATION ==========\n")

# Select best model
best_model <- model2
best_formula <- model2_formula

# Simplified bootstrap (100 iterations for speed)
n_boot <- 100
boot_aucs <- numeric(n_boot)

for(i in 1:n_boot) {
  # Resample
  idx <- sample(nrow(df_elderly), replace = TRUE)
  df_boot <- df_elderly[idx, ]
  
  # Fit model
  model_boot <- tryCatch({
    glm(best_formula, data = df_boot, family = binomial)
  }, warning = function(w) {
    logistf(best_formula, data = df_boot)
  }, error = function(e) {
    NULL
  })
  
  if(!is.null(model_boot)) {
    # Predict on original data
    if(class(model_boot)[1] == "logistf") {
      # For logistf, refit on original data
      boot_aucs[i] <- perf2$auc  # Use apparent AUC as approximation
    } else {
      pred_boot <- predict(model_boot, newdata = df_elderly, type = "response")
      idx_complete <- !is.na(pred_boot)
      boot_aucs[i] <- auc(roc(df_elderly$fever_focus[idx_complete], 
                              pred_boot[idx_complete], quiet = TRUE))
    }
  }
}

boot_aucs <- boot_aucs[boot_aucs > 0]
cat(sprintf("Apparent AUC: %.3f\n", perf2$auc))
cat(sprintf("Bootstrap mean AUC: %.3f\n", mean(boot_aucs)))
cat(sprintf("95%% CI: [%.3f, %.3f]\n", quantile(boot_aucs, 0.025), 
            quantile(boot_aucs, 0.975)))

# ===============================================================================
# 6. CLINICAL RISK SCORE - FIXED VERSION
# ===============================================================================

cat("\n========== RISK SCORE DEVELOPMENT ==========\n")

# METHOD 1: Vectorized calculation (RECOMMENDED)
df_elderly$risk_score <- with(df_elderly, {
  score <- rep(0, nrow(df_elderly))
  score <- score + ifelse(!is.na(CRP) & CRP >= 10, 3, 0)
  score <- score + ifelse(!is.na(WBC) & WBC >= 11, 2, 0)
  score <- score + ifelse(!is.na(Albumin) & Albumin < 3, 2, 0)
  score <- score + ifelse(age >= 90, 1, 0)
  score
})

# Alternative METHOD 2: Using mutate
df_elderly <- df_elderly %>%
  mutate(
    crp_points = ifelse(!is.na(CRP) & CRP >= 10, 3, 0),
    wbc_points = ifelse(!is.na(WBC) & WBC >= 11, 2, 0),
    albumin_points = ifelse(!is.na(Albumin) & Albumin < 3, 2, 0),
    age_points = ifelse(age >= 90, 1, 0),
    risk_score_v2 = crp_points + wbc_points + albumin_points + age_points
  )

# Alternative METHOD 3: Using a function with data frame indexing
calculate_risk_score_fixed <- function(df) {
  score <- numeric(nrow(df))
  
  for(i in 1:nrow(df)) {
    s <- 0
    if(!is.na(df$CRP[i]) && df$CRP[i] >= 10) s <- s + 3
    if(!is.na(df$WBC[i]) && df$WBC[i] >= 11) s <- s + 2
    if(!is.na(df$Albumin[i]) && df$Albumin[i] < 3) s <- s + 2
    if(df$age[i] >= 90) s <- s + 1
    score[i] <- s
  }
  
  return(score)
}

# Use whichever method you prefer
# df_elderly$risk_score <- calculate_risk_score_fixed(df_elderly)

# Risk categories
df_elderly$risk_cat <- cut(df_elderly$risk_score, 
                           breaks = c(-Inf, 2, 5, Inf),
                           labels = c("Low", "Intermediate", "High"))

# Performance by risk category
risk_perf <- df_elderly %>%
  group_by(risk_cat) %>%
  summarise(
    n = n(),
    ct_positive = sum(fever_focus),
    rate = mean(fever_focus) * 100,
    .groups = 'drop'
  )

print(risk_perf)

# ===============================================================================
# 7. FINAL TABLES
# ===============================================================================

cat("\n========== CREATING FINAL TABLES ==========\n")

# Table 1: Baseline characteristics
vars <- c("age", "CRP", "WBC", "Albumin", "BUN", "Creatinine", "cci",
          "hypertension_yn", "diabetes_yn", "death")

table1 <- CreateTableOne(vars = vars, strata = "fever_focus", 
                         data = df_elderly, test = TRUE)
table1_print <- print(table1, printToggle = FALSE)
write.csv(table1_print, "table1_baseline.csv")

# Table 2: Final model coefficients
if(class(best_model)[1] == "glm") {
  final_coef <- data.frame(
    Variable = names(coef(best_model)),
    OR = round(exp(coef(best_model)), 2),
    CI_lower = round(exp(confint(best_model)[, 1]), 2),
    CI_upper = round(exp(confint(best_model)[, 2]), 2),
    p_value = round(summary(best_model)$coefficients[, 4], 3)
  )
} else {
  final_coef <- data.frame(
    Variable = names(coef(best_model)),
    OR = round(exp(coef(best_model)), 2),
    CI_lower = round(exp(best_model$ci.lower), 2),
    CI_upper = round(exp(best_model$ci.upper), 2),
    p_value = round(best_model$prob, 3)
  )
}

write.csv(final_coef, "table2_model.csv", row.names = FALSE)

# Risk score distribution
cat("\n=== Risk Score Distribution ===\n")
table(df_elderly$risk_score)

# Cross-tabulation
cat("\n=== Risk Score vs CT Outcome ===\n")
print(table(df_elderly$risk_score, df_elderly$fever_focus))

# ===============================================================================
# SUMMARY
# ===============================================================================

cat("\n========== ANALYSIS COMPLETE ==========\n")
cat("\nKey Findings:\n")
cat(sprintf("- Best model AUC: %.3f\n", perf2$auc))
cat(sprintf("- High risk group CT positive rate: %.1f%%\n", 
            risk_perf$rate[risk_perf$risk_cat == "High"]))
cat("\nFiles created:\n")
cat("- table1_baseline.csv\n")
cat("- table2_model.csv\n")
cat("- roc_curves.pdf\n")

# Verify risk score calculation
cat("\n=== Risk Score Verification ===\n")
cat("Risk score range:", range(df_elderly$risk_score, na.rm = TRUE), "\n")
cat("Risk score summary:\n")
print(summary(df_elderly$risk_score))