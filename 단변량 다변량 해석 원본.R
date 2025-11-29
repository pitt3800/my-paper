# ===============================================================================
# 2. PRIMARY ANALYSIS - CONTINUOUS VARIABLES
# ===============================================================================

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
