##------------------------------------------------------------
## 제목:   열수축 (MD) 수원 07호기 분석 모형
## 날짜:   2018년 08월 30일 
## 작성자: 최기준
## Description
## - 필요 자료: PI/ CHIP/ IV/ HS 실험 자료
##   1) PI: 수기 데이터(15~17년) & 센서 데이터(18년) 활용
##   2) CHIP, HS 실험 자료: SKYPRO에서 내려받기
##   3) IV: 이론 IV 데이터 사용 (Master)
##------------------------------------------------------------


##------------------------------------------------------------
## -- 0. 라이브러리
##------------------------------------------------------------
# Library
library(data.table)
library(dplyr)
library(jsonlite)

# source
source(file = "./src/data_loader.R")
source(file = "./src/feature_extraction.R")
source(file = "./src/model.R")
source(file = "./src/utils.R")

# config file
config <- jsonlite::fromJSON(txt = "config.json")

##------------------------------------------------------------
## -- 1. Import Dataset
##------------------------------------------------------------
# define arguments
data_dir <- config$data_loader$args$data_dir
filename <- config$data_loader$args$filename
doff     <- config$variable$args$doff
species  <- config$variable$args$species
outlier_doff     <- config$variable$args$outlier_doff
outlier_species <- config$variable$args$outlier_species

# read data
df <- data_loader(  data_dir = data_dir, filename = filename
                    , doff = doff, species = species
                    , outlier_doff = outlier_doff
                    , outlier_species = outlier_species )

##------------------------------------------------------------
## -- 2. Derived Variable
##------------------------------------------------------------
# create Eta1 & Eta2 variable
df <- df %>%
  mutate(
    # -- ETA
    ETA1_SPEED = (LSM_Speed3 / LSM_Speed2) * (LSM_Speed3 - LSM_Speed2)
    , ETA1 = LSM_Speed3 / LSM_Speed2
    , ETA2 = PTN_11 / PTN_4
    
    # -- Speed of Tenter Direction
    , TD_SPEED = ((PTN_8 - PTN_5) * .001) / ((3.0 * 3) / (SPEED/60)) 
    
    # -- The ratio of Relaxation
    , RELAX = ((PTN_13 - PTN_15) / PTN_13) * 100
    , RELAX_1 = ((PTN_14 - PTN_17) / PTN_14) * 100
    , RELAX_2 = ((PTN_13 - PTN_17) / PTN_13) * 100
  ) %>%
  arrange( DOFF_NO )

# calculate actual temperature ( using Chain calculation method )
temperature <- local(expr = {
  tmp <- list()
  for(i in 1:13){
    
    # Initial value is set to 30 when calculating for the first time
    if( i == 1 ){
      tmp_s <- calculate_heat_calories( df = df, config = config$variable$args, direction = "S", zone = 1, init = 30)
      tmp_n <- calculate_heat_calories( df = df, config = config$variable$args, direction = "N", zone = 1, init = tmp_s )
      
      tmp[[i]] <- data.table(tmp_s, tmp_n)
      names(x = tmp[[i]]) <- paste("temp", c("S", "N"), i, sep = "_")
      
      # Set previous calculated value as initial value
    }else{
      tmp_s <- calculate_heat_calories( df = df, config = config$variable$args, direction = "S", zone = i, init = tmp_n )
      tmp_n <- calculate_heat_calories( df = df, config = config$variable$args, direction = "N", zone = i, init = tmp_s )
      
      tmp[[i]] <- data.table(tmp_s, tmp_n)
      names(x = tmp[[i]]) <- paste("temp", c("S", "N"), i, sep = "_")
    }
  }
  tmp <- do.call(what = "cbind", args = tmp)
  return( tmp)
})

df <- cbind(df, temperature)


##------------------------------------------------------------
## -- 3. Modeling
##------------------------------------------------------------
#---- validation
train_idx <- 1:(nrow(x = df) * .8)
valid_idx <- idx[-train_idx]
train_df <- df[train_idx, ]
valid_df <- df[-train_idx, ]

#---- Fit model
out <- model(  formula = MDHS ~ IV + ETA1 + RELAX
               + poly(x = temp_S_7, degree = 3)
               + poly(x = temp_N_10, degree = 2)
               + temp_N_13
               , data = train_df)

# rownames(x = out$coefficient) <- c("Intercept", "IV", "ETA1", "RELAX", "TEMP_S_7", "(TEMP_S_7)^2", "(TEMP_S_7)^3", "TEMP_N_10", "(TEMP_N_10)^2", "TEMP_N_13")
knitr::kable(round(x = out$coefficient, digits = 3), caption = "The Coefficients")

#---- Prediction
predicted_value    <- predict(object = out$model, newdata = df)
df$predicted_value <- predicted_value

# Quantitative evaluation
eval_quantitative(data = df[valid_idx, ], actual = "MDHS", predict = "predicted_value")

# Qualitative evaluation
df[, idx := 1:nrow(x = df)]
df %>% 
  ggplot() + 
  geom_vline(xintercept = valid_idx, linetype = "dashed", color = "grey", size = .5) + 
  geom_point(mapping = aes( x = idx, y = MDHS), size = .5) + 
  geom_point(mapping = aes( x = idx, y = predicted_value), color = "red", size = .5) + 
  geom_line(mapping = aes( x = idx, y = predicted_value), linetype = "dashed", color = "red", size = .1) + 
  labs(  title = "The Result of Regression"
       , subtitle = "Trend Chart of Actual value vs. Predicted value"
       , x = "", y = expression("MDHS "^" (Heat Shrinkage of MD direction)") ) + 
  theme_bw()
