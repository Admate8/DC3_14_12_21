library(dplyr)
library(ggplot2)
library(gridExtra)

######################################                                  
#                                    #
#                EDA                 #
#                                    #
######################################

### LOAD DATA ###
load("/Users/adrianwisnios/Desktop/Masters/Part 1/ST952 An Introduction to Statistical Practice /Assignment 2/PolishBR.Rdata")

# Initial look at the data
# View(skimr::skim(PolishBR))

# Missing values investigation
naniar::gg_miss_upset(PolishBR)

PolishBR_clean <- PolishBR %>% na.omit()

# Check the proportion in bust for missing/non-missing data
Missing <- PolishBR[rowSums(is.na(PolishBR)) > 0, ]
Not_missing <- PolishBR %>% na.omit()

Missing %>% count(bust) %>% summarise(bust, Prop = n/sum(n))
Not_missing %>% count(bust) %>% summarise(bust, Prop = n/sum(n))

# Table 2.1
knitr::kable((PolishBR_clean %>% filter(TLiabil < 0)),
             caption = "Suspicious observation", booktabs = T)
PolishBR_clean <- PolishBR_clean %>% filter(TLiabil > 0)

# Check that BookVal can be found using TAssets and TLiabil: Table 2.2
check <- PolishBR_clean %>% 
  mutate(BookVal_2 = TAssets - TLiabil,
         Diff = BookVal - BookVal_2)
knitr::kable(t(round(summary(check$Diff), 2)), 
             caption = "Book value of equity difference", booktabs = T)

# Proportion table for WkCap and bust: Table 2.3
basic_table <- table(PolishBR_clean$WkCap, PolishBR_clean$bust)
prop_table <- round(prop.table(basic_table, margin = 1) * 100, 2)
knitr::kable(t(prop_table), caption = "Working capital and bust (%)")

## Figure 2.1
# Left-hand side plot
cor_1 <- ggcorrplot::ggcorrplot(round(cor(PolishBR_clean[,-c(1,4)]), 2),
                                lab = T, type = "lower", show.legend = FALSE,
                                lab_size = 2, tl.cex = 8, title = "No Transformation") 

PolishBR_clean_logs <- PolishBR_clean %>%
  filter(TLiabil > 0) %>%
  mutate(TAssets = log(TAssets),
         TLiabil = log(TLiabil),
         TSales = log(TSales),
         StLiabil = log(StLiabil),
         CAssets = log(CAssets),
         OpExp = log(OpExp),
         CostPrS = log(CostPrS),
         CLiabil = log(CLiabil)) %>%
  select(-BookVal)

# Right-hand side plot
cor_2 <- ggcorrplot::ggcorrplot(round(cor(PolishBR_clean_logs[,-c(1,4)]), 2),
                                lab = T, type = "lower", show.legend = FALSE,
                                lab_size = 2, tl.cex = 8, title = "Transformed Variables") 

grid.arrange(cor_1, cor_2, nrow = 1, ncol = 2)

# Proportion of about 60%
the_same <- PolishBR_clean %>%
  mutate(Diff = OpExp - CostPrS,
         is_the_same = case_when(
           abs(Diff) < 1 ~ "almost identical",
           TRUE ~ "Different"
         )) 
the_same %>% count(is_the_same) %>% summarise(prop = n/sum(n))
for_cor <- the_same %>% filter(is_the_same == "almost identical")
# Correlation of 0.9999998
cor(for_cor$OpExp, for_cor$CostPrS)

# Table 2.4
table_CRatio <- PolishBR %>%
  na.omit() %>%
  mutate(CRatio = CAssets/StLiabil,
         Ratio_Below_1 = ifelse(CRatio < 1, "yes", "no")) %>%
  group_by(Ratio_Below_1) %>%
  count(bust) %>%
  summarise(Bust = ifelse(bust == 1, "yes", "no"),
            Percent_bust = round((n / sum(n) * 100), 2))
knitr::kable(table_CRatio, caption = "Current ratio and bust")

# Figure 2.2
plot_bust <- PolishBR %>%
  na.omit() %>%
  mutate(Equity_Ratio = (TAssets - TLiabil) / TAssets)  %>%
  filter(between(Equity_Ratio,0,1)) %>%
  ggplot(aes(bust, Equity_Ratio)) + 
  geom_boxplot(fill = "cyan") +
  geom_hline(yintercept = 0.5, col = "red", lty = 2) + 
  theme_bw() +
  xlab("Bust") + ylab("Shareholder Equity Ratio") 

plot_WkCap <- PolishBR %>%
  na.omit() %>%
  mutate(Equity_Ratio = (TAssets - TLiabil) / TAssets)  %>%
  filter(between(Equity_Ratio,0,1)) %>%
  ggplot(aes(WkCap, Equity_Ratio)) + 
  geom_boxplot(fill = "blue") +
  geom_hline(yintercept = 0.5, col = "red", lty = 2) + 
  theme_bw() +
  xlab("Working Capital") + theme(axis.title.y = element_blank(),
                                  axis.ticks.y = element_blank(),
                                  axis.text.y = element_blank()) 
grid.arrange(plot_bust, plot_WkCap, ncol = 2, nrow = 1)

# 70/246 (28.46%) companies with negative Equity ratio went bankrupt
PolishBR %>%
  na.omit() %>%
  mutate(Equity_Ratio = (TAssets - TLiabil) / TAssets)  %>%
  filter(Equity_Ratio < 0) %>%
  count(bust) %>%
  summarise(Percent = n/sum(n) * 100)

# Figure 2.3
library(Stat2Data)
myemplogit <- function(yvar = y,xvar = x, maxbins = 10, line=TRUE, ...){
  breaks  <<- unique(quantile(xvar, probs = 0:maxbins/maxbins))
  levs  <<- (cut(xvar, breaks, include.lowest = FALSE))
  num <<- as.numeric(levs)
  emplogitplot1(yvar ~ xvar, breaks = breaks, showline = line, ...)
}
par(mfrow=c(1,3))
myemplogit(PolishBR_clean$bust,
           PolishBR_clean_logs$OpExp,
           50, xlab="Operating expenses (log)", ylim = c(-4.5, -1.4)) 
myemplogit(PolishBR_clean$bust,
           PolishBR_clean_logs$TSales,
           50, xlab="Total sales (log)", ylim = c(-4.5, -1.4))
myemplogit(PolishBR_clean$bust,
           PolishBR_clean_logs$CostPrS,
           50, xlab="Cost of products sold (log)", ylim = c(-4.5, -1.4)) 

######################################                                  
#                                    #
#             Modelling              #
#                                    #
######################################

library(caret)
library(fastDummies)
library(glmnet)
library(ROCR)

# Remove negative TLiabil value
PolishBR_clean_ready <- PolishBR %>% na.omit() %>% filter(TLiabil > 0)

# Create dummy variables for WkCap (with Low as the base category)
PolishBR_clean_ready <- dummy_cols(PolishBR_clean_ready, select_columns = "WkCap",
                                   remove_first_dummy = TRUE) %>%
  select(-WkCap)

colnames(PolishBR_clean_ready)[13] <- "WkCap_Very_High"

set.seed(3456)
split <- createDataPartition(PolishBR_clean_ready$bust, p = 0.7, list = FALSE)
train <- PolishBR_clean_ready[split, ]
validate_and_test <- PolishBR_clean_ready[-split, ]
validate_and_test_split <- createDataPartition(validate_and_test$bust,
                                               p = 0.5, list = FALSE)
validate <- validate_and_test[validate_and_test_split, ]
test <- validate_and_test[-validate_and_test_split, ]

#########################################################
### Model_Base
# Basic model, no transformations, no cutoff adjustment 
#########################################################
model_base <- glm(bust ~ ., data = train, family = binomial)
model_base_pred_t <- predict(model_base, test[-1], type = "response")
pred_final <- as.data.frame(model_base_pred_t) %>%
  mutate(Prediction = ifelse(model_base_pred_t > 0.5, 1, 0)) %>%
  select(Prediction)
cMatrix <- confusionMatrix(table(pred_final$Prediction, test$bust), 
                           positive = "1")
Number_Params <- nrow(as.data.frame(coef(model_base)))
Model_Base <- rbind(as.data.frame(round(c(cMatrix$overall[c("Accuracy", "Kappa")],
                                          cMatrix$byClass[c("Sensitivity", "Specificity")]), 3)),
                    Parameters = Number_Params)

#########################################################
### Data Preparations for further modelling
#########################################################

# Transform the data
Data_Transformed <- PolishBR_clean_ready %>%
  mutate(BookVal = TAssets - TLiabil,
         CRatio = CAssets/StLiabil,
         ERatio = BookVal/TAssets,
         log_TAssets = log(TAssets),
         log_TLiabil = log(TLiabil),
         log_TSales = log(TSales),
         log_StLiabil = log(StLiabil),
         log_CAssets = log(CAssets),
         log_OpExp = log(OpExp),
         log_CostPrS = log(CostPrS)) %>%
  select(-TAssets, - TLiabil, -TSales, -StLiabil, 
         -CAssets, -OpExp, -CostPrS, -CLiabil)

# Split on train/validate/test  
set.seed(3456)
split <- createDataPartition(Data_Transformed$bust, p = 0.7, list = FALSE)
train <- Data_Transformed[split, ]
validate_and_test <- Data_Transformed[-split, ]
validate_and_test_split <- createDataPartition(validate_and_test$bust,
                                               p = 0.5, list = FALSE)
validate <- validate_and_test[validate_and_test_split, ]
test <- validate_and_test[-validate_and_test_split, ]

#########################################################
### A function for finding the optimal probability cutoff
#########################################################

optimal_cutoff <- function(model, validate){
  # Find the optimal cutoff
  predictions <- prediction(model, validate[1])
  sens <- data.frame(x=unlist(performance(predictions, "sens")@x.values), 
                     y=unlist(performance(predictions, "sens")@y.values))
  spec <- data.frame(x=unlist(performance(predictions, "spec")@x.values), 
                     y=unlist(performance(predictions, "spec")@y.values))
  optimal <- sens[which.min(apply(sens, 1, 
                                  function(x) min(colSums(abs(t(spec) - x))))), 1]
  
  # Visualise the cutoff 
  optimal_plot <- sens %>% 
    ggplot(aes(x, y)) + 
    geom_line() + 
    geom_line(data = spec, aes(x, y , col = "red")) +
    scale_y_continuous(sec.axis = sec_axis(~ . , name = "Specificity")) +
    labs(x = 'Cutoff', y = "Sensitivity") +
    theme_bw() +
    theme(axis.title.y.right = element_text(colour = "red"),
          legend.position = "none") +
    geom_vline(xintercept = optimal, col = "blue", lty = 2)
  
  return(list(optimal, optimal_plot))
}

#########################################################
### Model_Red_Back
# Transformed variables with added features. Reduced using stepwise selection
#########################################################

# Train the model
model_back <- glm(bust ~ ., family = binomial, data = train)
model_red_back <- step(model_back, direction = "both",
                       test = "Chisq", k = 3.841)

# Find the optimal cutoff probability
model_red_back_pred_v <- predict(model_red_back, validate[-1], type = "response")
optimal <- optimal_cutoff(model_red_back_pred_v, validate)[[1]]

# Assess performance
model_red_back_pred_t <- predict(model_red_back, test[-1], type = "response")
pred_final <- as.data.frame(model_red_back_pred_t) %>%
  mutate(Prediction = ifelse(model_red_back_pred_t > optimal, 1, 0)) %>%
  select(Prediction)
cMatrix <- confusionMatrix(table(pred_final$Prediction, test$bust),
                           positive = "1")
Number_Params <- nrow(as.data.frame(coef(model_red_back)))
Model_Red_Back <- rbind(as.data.frame(round(c(cMatrix$overall[c("Accuracy", "Kappa")],
                                              cMatrix$byClass[c("Sensitivity", "Specificity")]), 3)), 
                        Parameters = Number_Params)

#########################################################
### Model_Red_LASSO
# Transformed variables with added features. Reduced using LASSO selection
#########################################################

# Train the model
model_red_lasso <- glmnet(train[, -1], train[, 1], family = "binomial", alpha = 1)

# Find an optimal value of lambda using cross-validation on the training set using bootstrap
lambdas <- c()
for (i in 1:100){
  cv_model <- cv.glmnet(as.matrix(train[, -1]), train[, 1], 
                        family = "binomial", alpha = 1)
  lambdas[i] <- cv_model$lambda.1se
}
lambda_optimal <- mean(lambdas)

# Find the cutoff using validation set
model_red_lasso_pred_v <- predict(model_red_lasso, s = lambda_optimal,
                                  newx = as.matrix(validate[-1]))
optimal <- optimal_cutoff(model_red_lasso_pred_v, validate)[[1]]

# Assess performance
model_red_lasso_pred_t <- predict(model_red_lasso, s = lambda_optimal,
                                  newx = as.matrix(test[-1]))
pred_final <- as.data.frame(model_red_lasso_pred_t) %>%
  mutate(Prediction = ifelse(model_red_lasso_pred_t > optimal, 1, 0)) %>%
  select(Prediction)
cMatrix <- confusionMatrix(table(pred_final$Prediction, test$bust),
                           positive = "1")
coeffs_model_red_lasso <- as.data.frame(as.matrix(coef(model_red_lasso,
                                                       s = lambda_optimal)))
non_zero_coeffs_red_lasso <- coeffs_model_red_lasso %>% filter(s1 != 0)
Number_Params <- nrow(non_zero_coeffs_red_lasso)
names(non_zero_coeffs_red_lasso) <- ""
Model_Red_LASSO <- rbind(as.data.frame(round(c(cMatrix$overall[c("Accuracy", "Kappa")],
                                               cMatrix$byClass[c("Sensitivity", "Specificity")]), 3)),
                         N_of_Pred = Number_Params)

# Table 3.1
library(car)
knitr::kable(Anova(model_back), caption = "Initial model", booktabs = T) %>%
  kableExtra::kable_styling(latex_options = "HOLD_position")

# Table 3.2
knitr::kable(t(round(coef(model_red_back), 4)),
             caption="Model_Red_Back coefficients", booktabs = T) %>%
  kableExtra::kable_styling(latex_options = c("scale", "HOLD_position"))

# Log(lambda) vs. Coefficients Plot
plot(model_red_lasso, "lambda", label = T) 
abline(v = log(lambda_optimal), lty = 2)
text(-4.9, -3, expression(lambda))

# Figure 3.1
optimal_cutoff(model_red_back_pred_v, validate)[[2]]

# Table 3.3
all_models_comparison <- cbind(Model_Base, Model_Red_Back, Model_Red_LASSO)
names(all_models_comparison) <- c("Model_Base", "Model_Red_Back", "Model_Red_LASSO")
knitr::kable(all_models_comparison,
             caption="Models comparison", booktabs = T) %>%
  kableExtra::kable_styling(latex_options = "HOLD_position")
