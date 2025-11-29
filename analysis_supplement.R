# ===============================================================================
# CT Prediction Model - Supplementary Analysis
# Version: 4.0 - Additional analyses (separate file)
# ===============================================================================

# Run after main analysis (analysis_main.R)
# Assumes df_elderly and models are already loaded

library(ggplot2)
library(gridExtra)

# ===============================================================================
# 1. CALIBRATION ANALYSIS
# ===============================================================================

cat("\n========== CALIBRATION ANALYSIS ==========\n")

create_calibration_plot <- function(model, data, title = "Calibration Plot") {
  
  # Get predictions
  if(class(model)[1] == "logistf") {
    pred_prob <- model$predict
  } else {
    pred_prob <- predict(model, type = "response")
  }
  
  # Remove missing
  complete_idx <- !is.na(pred_prob) & !is.na(data$fever_focus)
  pred_prob <- pred_prob[complete_idx]
  observed <- data$fever_focus[complete_idx]
  
  # Create groups (use fewer groups if sample size is small)
  n_groups <- min(8, floor(length(pred_prob)/20))
  
  if(n_groups >= 3) {
    groups <- cut(pred_prob, 
                  breaks = quantile(pred_prob, probs = seq(0, 1, length.out = n_groups + 1)),
                  include.lowest = TRUE, labels = FALSE)
    
    cal_data <- data.frame(pred = pred_prob, obs = observed, group = groups) %>%
      group_by(group) %>%
      summarise(
        mean_pred = mean(pred),
        mean_obs = mean(obs),
        n = n(),
        se = sqrt(mean_obs * (1 - mean_obs) / n),
        .groups = 'drop'
      )
    
    # Create plot
    pdf(paste0("calibration_", gsub(" ", "_", title), ".pdf"), width = 8, height = 8)
    
    plot(cal_data$mean_pred, cal_data$mean_obs,
         xlim = c(0, 1), ylim = c(0, 1),
         xlab = "Predicted Probability",
         ylab = "Observed Probability",
         main = title,
         pch = 16, cex = 1.5, col = "blue")
    
    # Error bars
    arrows(cal_data$mean_pred, 
           cal_data$mean_obs - 1.96 * cal_data$se,
           cal_data$mean_pred,
           cal_data$mean_obs + 1.96 * cal_data$se,
           length = 0.05, angle = 90, code = 3, col = "blue")
    
    # Reference line
    abline(0, 1, lty = 2, col = "gray")
    
    # Smooth line (if enough points)
    if(nrow(cal_data) >= 4) {
      smooth_cal <- lowess(pred_prob, observed, f = 0.5)
      lines(smooth_cal, col = "red", lwd = 2)
    }
    
    legend("topleft", 
           legend = c("Observed", "95% CI", "Perfect", "Smooth"),
           col = c("blue", "blue", "gray", "red"),
           pch = c(16, NA, NA, NA),
           lty = c(NA, 1, 2, 1))
    
    dev.off()
    
    cat(sprintf("%s - Calibration plot saved\n", title))
  } else {
    cat(sprintf("%s - Too few groups for calibration plot\n", title))
  }
}

# Create calibration plots for each model
if(exists("model1")) create_calibration_plot(model1, df_elderly, "Model 1")
if(exists("model2")) create_calibration_plot(model2, df_elderly, "Model 2")
if(exists("model3")) create_calibration_plot(model3, df_elderly, "Model 3")

# ===============================================================================
# 2. SENSITIVITY ANALYSIS
# ===============================================================================

cat("\n========== SENSITIVITY ANALYSIS ==========\n")

# 2.1 Different CRP cutpoints
cat("\n=== Testing different CRP cutpoints ===\n")

crp_cutpoints <- c(5, 7, 10, 12, 15)
cutpoint_results <- data.frame()

for(cutpoint in crp_cutpoints) {
  df_temp <- df_elderly %>%
    mutate(crp_high = ifelse(CRP >= cutpoint, 1, 0))
  
  model_temp <- tryCatch({
    glm(fever_focus ~ crp_high + WBC + Albumin, data = df_temp, family = binomial)
  }, warning = function(w) {
    logistf(fever_focus ~ crp_high + WBC + Albumin, data = df_temp)
  }, error = function(e) {
    NULL
  })
  
  if(!is.null(model_temp)) {
    if(class(model_temp)[1] == "glm") {
      or_val <- exp(coef(model_temp)["crp_high"])
      p_val <- summary(model_temp)$coefficients["crp_high", 4]
    } else {
      or_val <- exp(coef(model_temp)["crp_high"])
      p_val <- model_temp$prob["crp_high"]
    }
    
    cutpoint_results <- rbind(cutpoint_results, data.frame(
      CRP_cutpoint = cutpoint,
      OR = round(or_val, 2),
      p_value = round(p_val, 4)
    ))
  }
}

print(cutpoint_results)

# 2.2 Complete case vs simple imputation
cat("\n=== Missing data handling comparison ===\n")

# Complete cases only
complete_vars <- c("CRP", "WBC", "Albumin", "age")
complete_idx <- complete.cases(df_elderly[, complete_vars])
df_complete <- df_elderly[complete_idx, ]

cat(sprintf("Complete cases: %d/%d (%.1f%%)\n", 
            sum(complete_idx), nrow(df_elderly), 
            sum(complete_idx)/nrow(df_elderly)*100))

# Model on complete cases
model_complete <- tryCatch({
  glm(fever_focus ~ CRP + WBC + Albumin, data = df_complete, family = binomial)
}, warning = function(w) {
  logistf(fever_focus ~ CRP + WBC + Albumin, data = df_complete)
})

# Simple median imputation
df_imputed <- df_elderly
for(var in complete_vars) {
  if(sum(is.na(df_imputed[[var]])) > 0) {
    df_imputed[[var]][is.na(df_imputed[[var]])] <- 
      median(df_imputed[[var]], na.rm = TRUE)
  }
}

model_imputed <- tryCatch({
  glm(fever_focus ~ CRP + WBC + Albumin, data = df_imputed, family = binomial)
}, warning = function(w) {
  logistf(fever_focus ~ CRP + WBC + Albumin, data = df_imputed)
})

# Compare AUCs
if(class(model_complete)[1] == "logistf") {
  pred_complete <- model_complete$predict
} else {
  pred_complete <- predict(model_complete, type = "response")
}

if(class(model_imputed)[1] == "logistf") {
  pred_imputed <- model_imputed$predict
} else {
  pred_imputed <- predict(model_imputed, type = "response")
}

auc_complete <- auc(roc(df_complete$fever_focus, pred_complete, quiet = TRUE))
auc_imputed <- auc(roc(df_imputed$fever_focus, pred_imputed, quiet = TRUE))

cat(sprintf("Complete case AUC: %.3f\n", auc_complete))
cat(sprintf("Imputed data AUC: %.3f\n", auc_imputed))

# ===============================================================================
# 3. SUBGROUP ANALYSIS
# ===============================================================================

cat("\n========== SUBGROUP ANALYSIS ==========\n")

perform_subgroup_analysis <- function(data, subgroup_var, subgroup_val, model_formula) {
  
  subset_data <- data %>% filter(!!sym(subgroup_var) == subgroup_val)
  
  if(nrow(subset_data) >= 30 && sum(subset_data$fever_focus) >= 10) {
    model_sub <- tryCatch({
      glm(model_formula, data = subset_data, family = binomial)
    }, warning = function(w) {
      logistf(model_formula, data = subset_data)
    }, error = function(e) {
      NULL
    })
    
    if(!is.null(model_sub)) {
      if(class(model_sub)[1] == "logistf") {
        pred_sub <- model_sub$predict
      } else {
        pred_sub <- predict(model_sub, type = "response")
      }
      
      auc_sub <- auc(roc(subset_data$fever_focus, pred_sub, quiet = TRUE))
      
      return(data.frame(
        Subgroup = paste(subgroup_var, "=", subgroup_val),
        N = nrow(subset_data),
        Events = sum(subset_data$fever_focus),
        AUC = round(auc_sub, 3)
      ))
    }
  }
  
  return(NULL)
}

# Analyze by sex
subgroup_results <- data.frame()

for(sex_val in unique(df_elderly$sex)) {
  result <- perform_subgroup_analysis(df_elderly, "sex", sex_val, 
                                      fever_focus ~ CRP + WBC + Albumin)
  if(!is.null(result)) {
    subgroup_results <- rbind(subgroup_results, result)
  }
}

# Analyze by age groups
df_elderly$age_group_sub <- cut(df_elderly$age, 
                                breaks = c(85, 90, 95, Inf),
                                labels = c("85-89", "90-94", "95+"))

for(age_val in levels(df_elderly$age_group_sub)) {
  result <- perform_subgroup_analysis(df_elderly, "age_group_sub", age_val,
                                      fever_focus ~ CRP + WBC + Albumin)
  if(!is.null(result)) {
    subgroup_results <- rbind(subgroup_results, result)
  }
}

print(subgroup_results)

# ===============================================================================
# 4. DECISION CURVE ANALYSIS
# ===============================================================================

cat("\n========== DECISION CURVE ANALYSIS ==========\n")

calculate_net_benefit <- function(pred_prob, outcome, threshold) {
  n <- length(outcome)
  test_positive <- pred_prob >= threshold
  
  tp <- sum(test_positive & outcome == 1)
  fp <- sum(test_positive & outcome == 0)
  
  net_benefit <- (tp/n) - (fp/n) * (threshold/(1-threshold))
  return(net_benefit)
}

# Get predictions for best model
if(class(model2)[1] == "logistf") {
  pred_model2 <- model2$predict
} else {
  pred_model2 <- predict(model2, type = "response")
}

complete_idx <- !is.na(pred_model2)
pred_model2 <- pred_model2[complete_idx]
outcome <- df_elderly$fever_focus[complete_idx]

# Calculate net benefit across thresholds
thresholds <- seq(0.05, 0.50, by = 0.05)
nb_model <- sapply(thresholds, function(t) {
  calculate_net_benefit(pred_model2, outcome, t)
})

# Net benefit for treat all
prevalence <- mean(outcome)
nb_all <- prevalence - (1-prevalence) * thresholds/(1-thresholds)

# Plot
pdf("decision_curve.pdf", width = 10, height = 6)
plot(thresholds, nb_model, type = "l", col = "blue", lwd = 2,
     xlim = c(0, 0.5), ylim = c(-0.1, max(c(nb_model, nb_all))),
     xlab = "Threshold Probability", 
     ylab = "Net Benefit",
     main = "Decision Curve Analysis")

lines(thresholds, nb_all, col = "gray", lwd = 2, lty = 2)
abline(h = 0, col = "black", lty = 2)

legend("topright",
       legend = c("Prediction Model", "Treat All", "Treat None"),
       col = c("blue", "gray", "black"),
       lty = c(1, 2, 2),
       lwd = 2)

dev.off()

cat("Decision curve saved\n")

# ===============================================================================
# 5. EXPORT RISK SCORE TOOL
# ===============================================================================

cat("\n========== RISK SCORE TOOL ==========\n")

# Create a simple scoring sheet
scoring_sheet <- data.frame(
  Clinical_Finding = c("CRP ≥ 10 mg/dL", "WBC ≥ 11,000/μL", 
                       "Albumin < 3 g/dL", "Age ≥ 90 years"),
  Points = c(3, 2, 2, 1),
  stringsAsFactors = FALSE
)

risk_interpretation <- data.frame(
  Total_Score = c("0-2", "3-5", "6-8"),
  Risk_Category = c("Low", "Intermediate", "High"),
  Estimated_CT_Positive_Rate = c("~5%", "~20%", "~40-50%"),
  Recommendation = c("Consider observation", 
                     "Clinical judgment needed",
                     "CT recommended"),
  stringsAsFactors = FALSE
)

cat("\n=== Clinical Risk Score ===\n")
print(scoring_sheet)
cat("\n=== Risk Interpretation ===\n")
print(risk_interpretation)

# Save to CSV
write.csv(scoring_sheet, "risk_score_tool.csv", row.names = FALSE)
write.csv(risk_interpretation, "risk_interpretation.csv", row.names = FALSE)

cat("\n========== SUPPLEMENTARY ANALYSIS COMPLETE ==========\n")
cat("\nAdditional files created:\n")
cat("- calibration_*.pdf\n")
cat("- decision_curve.pdf\n")
cat("- risk_score_tool.csv\n")
cat("- risk_interpretation.csv\n")

# Total lines: ~395