rm(list=ls())
setwd("/home/dongjiaxing/CKB")
####1.read data####
#1.1 read basic questionnaires
library(dplyr)
library(survival)
library(plyr)
library(data.table)
library(arsenal) 
library(gtsummary)
library(tidyr)
library(patchwork)
library(ggridges)
library(ggplot2)
library(ggpmisc)
library(viridis)
library(ggthemes)
library(showtext)
library(scales)
library(forestplot)
library(forestploter)
library(ggh4x)
library(tidyverse)  
library(rms)  
library(rootSolve)  


screen_data <- read.delim2("data_baseline_questionnaires.csv",sep = ',',header = T) %>%
  select("csid", #id
         "study_date_year",#研究日期
         "study_date", #DD/MM/YYYY HH:mm:ss
         "cancer_site", #癌症部位
         "age_25_weight_jin", #25岁体重
         "bmi_calc",#BMI
         "age_at_study_date_x100", #年龄
         "bowel_movement_freq", #排便次数
         "dbp_mean", #血压
         "sbp_mean", #血压
         "hypertension_diag", #是否有高血压
         "has_diabetes","diabetes_diag", #糖尿病
         "random_glucose_x10", #随机血糖
         "fat_percent_x10", #FAT %
         "gall_diag", #胆石症
         "highest_education", #教育程度
         "hip_mm", #臀围
         "household_income", #家庭收入
         "impedance_ohms", #阻抗测量
         "is_female", #性别
         "kidney_dis_diag", #肾病
         "marital_status", #婚姻状态
         "peptic_ulcer_diag", #消化性溃疡
         "waist_hip_ratio", #腰臀比
         "standing_height_mm",#身高
         "sitting_height_mm", #坐高
         "region_is_urban", #是否城镇
         "waist_mm", # 腰围
         "taking_aspirin",#服用阿司匹林
         "taking_statins",#服用他汀类
         "cancer_diag_age",#诊断年龄
         "dob_anon",#出生日期
         "region_code",#地区
         "siblings_cancer",#兄弟姐妹癌症
         "father_cancer",#父亲癌症
         "mother_cancer",#母亲癌症
         "psych_disorder_diag",#心理疾病
         'weight_kg_x10'#体重
  )

#1.2 outcome
endpoint <- read.delim2('endpoints.csv',sep = ',')
smal <-  read.delim2("smoke.csv",sep = ',',header = T) %>%
  select("csid", #id
         "smoking_category",'alcohol_category')#研究日期

#1.3 merge
all_data <- merge(screen_data,endpoint)

####2.data-process####
#####2.1_BMI#####
data.exp <- all_data #备份一个数据
data.exp$bmi_calc <- as.numeric(data.exp$bmi_calc)
data.exp$height <- data.exp$standing_height_mm/1000
data.exp$weight25 <- data.exp$age_25_weight_jin/2
data.exp$BMI25 <-(data.exp$weight25/data.exp$height)/data.exp$height
data.exp$BMI25 <- as.numeric(data.exp$BMI25)
data.exp$BMI_change <-data.exp$bmi_calc-data.exp$BMI25
data.exp$BMI_change_ratio<- ((data.exp$bmi_calc-data.exp$BMI25)/data.exp$BMI25)*100
data.exp$weight_change_ratio<- (((data.exp$weight_kg_x10/10)-data.exp$weight25)/data.exp$weight25)*100
data.exp$BMI_5unit <- data.exp$bmi_calc/5
data.exp$BMI25_5unit <- data.exp$BMI25/5
data.exp$BMI_change_5unit <- data.exp$BMI_change/5

#####2.2 gender#####
data.exp$gender <- ifelse(data.exp$is_female == 1,"Female","Male")
# ddply(data.exp,.(gender),summarise,n=length(csid))
table(data.exp$gender)
#####2.3 hypertension#####
data.exp$hypertension <- ifelse(data.exp$hypertension_diag == 1,"hypertension","non_hypertension")
table(data.exp$hypertension)

#####2.4 diabetes_diag#####
data.exp$diabetes <- ifelse(data.exp$diabetes_diag == 1,"diabetes","non_diabetes")
table(data.exp$diabetes)

#####2.5 highest_education#####
data.exp$education <- ifelse(data.exp$highest_education == 0,"No formal school",
                             ifelse(data.exp$highest_education == 1, "Primary School",
                                    if_else(data.exp$highest_education == 2,"Middle School",
                                            ifelse(data.exp$highest_education == 3,"High School",
                                                   ifelse(data.exp$highest_education == 4,"Technical school / college","University")))))
data.exp <- data.exp %>%
  mutate(
    education = case_when(
      education %in% c("No formal school", "Primary School") ~ "No formal/Primary School",
      education %in% c("Technical school / college", "University") ~ "Technical school/college/University",
      TRUE ~ education
    )
  )
table(data.exp$education)

#####2.6 cancer#####
data.exp$cancer <- ifelse(data.exp$cancer_site == 0,"Lung",
                          ifelse(data.exp$cancer_site == 1, "Esophagus",
                                 if_else(data.exp$cancer_site == 2,"Stomach",
                                         ifelse(data.exp$cancer_site == 3,"Liver",
                                                ifelse(data.exp$cancer_site == 4,"Intestine",
                                                       ifelse(data.exp$cancer_site == 5,"Breast",
                                                              if_else(data.exp$cancer_site == 6,"Prostate",
                                                                      ifelse(data.exp$cancer_site == 7,"Cervix",
                                                                             ifelse(data.exp$cancer_site == 8,"Other",
                                                                                    ifelse(data.exp$cancer_site %in% NA,"none_cancer","YES"))))))))))

data.exp$cancer <- ifelse(data.exp$cancer_site %in% NA,"none_cancer",data.exp$cancer)
table(data.exp$cancer)

#####2.7 income#####
data.exp$income <- ifelse(data.exp$household_income == 0,"<2,500 yuan",
                          ifelse(data.exp$household_income == 1, "2,500-4,999 yuan",
                                 if_else(data.exp$household_income == 2,"5,000-9,999 yuan",
                                         ifelse(data.exp$household_income == 3,"10,000-19,999 yuan",
                                                ifelse(data.exp$household_income == 4,"20,000-34,999 yuan",">=35,000 yuan")))))
data.exp <- data.exp %>%
  mutate(
    income = case_when(
      income %in% c("<2,500 yuan", "2,500-4,999 yuan", "5,000-9,999 yuan") ~ "<10000 yuan",
      TRUE ~ income
    )
  )
table(data.exp$income)

#####2.8  marital_status#####
data.exp$marital <- ifelse(data.exp$marital_status == 0,"Married",
                           ifelse(data.exp$marital_status == 1, "Widowed",
                                  if_else(data.exp$marital_status == 2,"Separated / divorced","Never married")))

data.exp <- data.exp %>%
  mutate(
    marital= case_when(
      marital == "Married" ~ "Married",
      TRUE ~ "Other marital status"
    )
  )
table(data.exp$marital)  

#####2.9   "peptic_ulcer_diag", #消化性溃疡#####
data.exp$peptic_ulcer <- ifelse(data.exp$peptic_ulcer_diag == 1,"peptic_ulcer","none_peptic_ulcer")
table(data.exp$peptic_ulcer)

#####2.10   "age_at_study_date_x100", #年龄#####
data.exp$age <- data.exp$age_at_study_date_x100/100
summary(data.exp$age)

#####2.11   "region_is_urban"#####
data.exp$region <- ifelse(data.exp$region_is_urban == 1,"Urban","Rural")
table(data.exp$region)

#####2.12 outcome-cancer #####
data.exp$outcome <- if_else(data.exp$ep_CKB0017_oinc_ep=='1'|data.exp$ep_CKB0018_oinc_ep=='1','1','0')
data.exp$Gastrointestinal <- if_else(data.exp$ep_CKB0017_oinc_ep=='1'|data.exp$ep_CKB0018_oinc_ep=='1','Gastrointestinal cancer','none_cancer')
table(data.exp$outcome)
data.exp$cancer_type <- ifelse(data.exp$ep_CKB0017_oinc_ep=='1','gastric cancer',
                               if_else(data.exp$ep_CKB0018_oinc_ep=='1',"colon cancer",'none_cancer'))
data.exp$year <- substr(data.exp$ep_CKB0017_oinc_datedeveloped,1,4)
data.exp$year <- as.numeric(data.exp$year)
data.exp$suifang <- data.exp$year-data.exp$study_date_year

#####2.13 ABSI #####
data.exp$bmi_calc <- as.numeric(data.exp$bmi_calc)
data.exp$ABSI <-(data.exp$waist_mm/1000)/((data.exp$bmi_calc^(2/3))*(data.exp$height^0.5))
data.exp <- data.exp[!is.na(data.exp$ABSI),]
data.exp$ABSI_0.001 <- data.exp$ABSI/0.001

#####2.14 BRI index #####
data.exp$BRI <- 364.2-365.5*((1-(((data.exp$waist_mm/1000)/(2*pi))^2)/((0.5*data.exp$height)^2))^0.5)

#####2.15 waist #####
data.exp$Wasit_10unit <- data.exp$waist_mm/100
#####2.16 hip#####
data.exp$Hip_10unit <- data.exp$hip_mm/100

#####2.17 family history of cancer#####
data.exp$family_cancer <- ifelse(data.exp$father_cancer=='0'&
                                   data.exp$mother_cancer=='0'&
                                   data.exp$siblings_cancer=='0','0','1')
table(data.exp$family_cancer)

#####2.18 entry_date&exit_date#####
data.exp$entry_date <- substr(data.exp$study_date,1,10)
head(data.exp$entry_date )
data.exp$exit_date <- ifelse(data.exp$ep_CKB0017_oinc_ep == 1,
                             data.exp$ep_CKB0017_oinc_datedeveloped,
                             ifelse(data.exp$ep_CKB0018_oinc_ep == 1,
                                    data.exp$ep_CKB0018_oinc_datedeveloped,
                                    data.exp$ep_CKB0017_combined_datedeveloped))
sum(is.na(data.exp$exit_date))
data.exp$exit_date <- substr(data.exp$exit_date,1,10)
head(data.exp$exit_date)
data.exp$entry_date <- as.Date(data.exp$entry_date)
data.exp$exit_date <- as.Date(data.exp$exit_date)

####3. data_filter####
paper.data <- subset.data.frame(data.exp,(cancer %in% c("none_cancer"))&
                                  (!is.na(age)) & #年龄   无NA
                                  (!is.na(gender)) & #性别   无NA
                                  (!is.na(BMI25)) & #BMI   无NA
                                  (!is.na(bmi_calc)) & #BMI   无NA
                                  (!is.na(education)) & #教育程度 无NA
                                  (!is.na(hypertension))& #高血压 无NA
                                  (!is.na(income)) & # 收入  无NA
                                  (!is.na(diabetes)) & # 糖尿病  无NA 
                                  (!is.na(marital)) & #婚姻状态  无NA
                                  (!is.na(peptic_ulcer)) & #十二指肠溃疡
                                  (!is.na(fat_percent_x10))&
                                  (!is.na(family_cancer))&
                                  (!is.na(hip_mm))&
                                  (!is.na(waist_mm))&
                                  (!is.na(waist_hip_ratio))
)



paper.data <- paper.data[!(paper.data $ep_CKB0017_combined_ep==1&paper.data $ep_CKB0017_oinc_ep=='0')|(paper.data $ep_CKB0018_combined_ep==1&paper.data $ep_CKB0018_oinc_ep=='0'),]
paper.data$zzyear <- (paper.data$exit_date-paper.data$entry_date)/365.25+paper.data$age
paper.data$zzyear <- as.numeric(paper.data$zzyear)

#####3.1 define EOGICs age<50#####
data <- paper.data
data$dbp_mean <- as.numeric(data$dbp_mean)
data$sbp_mean <- as.numeric(data$sbp_mean)
data$waist_hip_ratio <- as.numeric(data$waist_hip_ratio)
data$outcome <- as.numeric(data$outcome)
data$zaofa <- ifelse((data$zzyear<50)&(data$outcome=='1'),'zaofa',
                     ifelse((data$zzyear>=50)&(data$outcome=='1'),'wanfa',"none_cancer"))
data <- data[!data$csid=='2022-00369-4376912',]

#####3.2 exclude outliners#####
#BMI
data <- data[(data$bmi_calc>=15),]
#WC&HC<height
data <- data[(data$waist_mm/1000)<data$height,]
data <- data[(data$hip_mm/1000)<data$height,]
#WHR
data <- data[(data$waist_hip_ratio>=0.5)&(data$waist_hip_ratio<=1.5),]
#hypertension
data <- data[(data$dbp_mean<=200&data$dbp_mean>=40&data$sbp_mean<=250&data$sbp_mean>=80),]
#WC&WC
data <- data[(data$waist_mm/10)>=50&(data$waist_mm/10)<=150,]
data <- data[(data$hip_mm/10)>=50&(data$hip_mm/10)<=150,]

rt1 <- smal
rt1 <- rt1[rt1$csid %in% data$csid,]
data <- merge(data,rt1)
table(data$smoking_category)
data$smoke <- ifelse(data$smoking_category == 1,"Never smoker",
                           ifelse(data$smoking_category == 2, "Occasional smoker",
                                  if_else(data$smoking_category == 3,"Ex regular smoker","Smoker")))
# data$smoke [data$smoke  %in% c("Occasional smoker","Ex regular smoker")] <- "Current smoker"
data$smoke [data$smoke  %in% c("Never smoker","Ex regular smoker")] <- "Abstainer/Former smoker"
table(data$smoke)

table(data$alcohol_category)
data$alco <- ifelse(data$alcohol_category == 1,"Never regular",
                          ifelse(data$alcohol_category == 2, "Ex-regular",
                                 if_else(data$alcohol_category == 3,"Occasional",
                                         ifelse(data$alcohol_category == 4,"Monthly",
                                                ifelse(data$alcohol_category == 5,"Reduced intake","Weekly")))))

data$alco[data$alco %in% c("Ex-regular","Reduced intake")] <- "Former drinker"
#data$alco[data$alco %in% c("Occasional","Monthly")] <- "Infrequent drinker"
data$alco[data$alco %in% c("Former drinker","Never regular")] <- "Abstainer/Former drinker"
table(data$alco)

#####3.3 filter age<50#####
data_zaofa <-data[data$zaofa %in% c('zaofa','none_cancer'),] 







case <- data_zaofa[data_zaofa$zaofa=='zaofa',]
case$str_id <-1:nrow(case)
control <- data_zaofa[data_zaofa$zaofa=='none_cancer',]

####4.risk-set sampling####
#####4.1 define function#####
match_riskset_fast_summary <- function(case_df, ctrl_df, ratio = 5,seed = 123L) {
  set.seed(seed)
    # 1. 先保留完整列，只做行号索引
  case_dt <- as.data.table(case_df)
  ctrl_dt <- as.data.table(ctrl_df)
  ctrl_dt[, idx := .I]                   # 行号
    # 2. 固定排序（无论列数多少，行号顺序一致）
  setorder(ctrl_dt, region_code, gender, age, entry_date, exit_date, idx)
    # 3. 建立二分索引
  setkey(ctrl_dt, region_code, gender)
  used <- logical(nrow(ctrl_dt))
  out  <- list()
  pool   <- list()          # 记录每例候选库大小
  # 4. 逐病例循环（顺序固定）
  for (i in seq_len(nrow(case_dt))) {
    c_row <- case_dt[i]
    # 4.1 二分找 region+gender 块，再向量过滤
    cand_idx <- ctrl_dt[.(c_row$region_code, c_row$gender),
                        on = .(region_code, gender),
                        which = TRUE]
    cand_idx <- cand_idx[
      abs(ctrl_dt$age[cand_idx] - c_row$age)       <= 0.5 &
        ctrl_dt$entry_date[cand_idx]                 <= c_row$entry_date &
        ctrl_dt$exit_date[cand_idx]                  >= c_row$exit_date  &
        !used[cand_idx]
    ]
    pool[[i]] <- data.table(csid      = c_row$csid,
                            ctrl_pool = length(cand_idx))
    if (length(cand_idx) < ratio) next
    pick_idx <- sample(cand_idx, ratio)
    used[pick_idx] <- TRUE
    # 4.2 用行号取子集，列数不影响顺序
    out[[i]] <- ctrl_dt[pick_idx][, str_id := c_row$str_id]
  }
  assign("pool_sz", rbindlist(pool), envir = .GlobalEnv)
  rbindlist(out)
}

#####4.2 construct nest case-control queue#####
matched_ctrl <- match_riskset_fast_summary(case, control, ratio = 5, seed = 423)
csid_id<- c(matched_ctrl$csid,case$csid)
zzdata <- data[data$csid %in% csid_id,]
case_str_id <- case[,c("csid","str_id")]
control_str_id <- matched_ctrl[,c("csid","str_id")]
data_str_id <- rbind(case_str_id ,control_str_id)
zzdata_str <- merge(zzdata,data_str_id)
zzdata_str$outcome <- as.numeric(zzdata$outcome)
ABSI_group<- quantile(zzdata_str$ABSI, probs=c(0,1/4,2/4,3/4,1))
zzdata_str$ABSI_group <- cut(zzdata_str$ABSI,
                             breaks = ABSI_group,
                             labels = c('Q1_ABSI', 'Q2_ABSI', 'Q3_ABSI','Q4_ABSI'))
table(zzdata_str$ABSI_group)
zzdata_str[which(zzdata_str$ABSI_group %in% NA),'ABSI_group'] <- 'Q1_ABSI'
table(zzdata_str$ABSI_group)
table(zzdata_str$group)



####Analysis####
####5 Baseline data####
#####5.1 define function-baseline#####
tab_common <- function(dat,group) {
  dat_name <- deparse(substitute(dat))
  dat %>% 
    dplyr::select(alco,smoke,bmi_calc,BMI25,Gastrointestinal,cancer_type,BRI,BMI_change,family_cancer,outcome,ABSI,ABSI_group,age, dbp_mean, sbp_mean, hypertension, diabetes, fat_percent_x10, education,
           hip_mm, income, impedance_ohms, gender,marital, peptic_ulcer, waist_hip_ratio, height, sitting_height_mm, region,waist_mm) %>%
    tbl_summary(
      by = group,
      type = list(#Gastrointestinal~"categorical",
        #cancer ~"categorical",
        diabetes ~"categorical",
        alco ~"categorical",
        smoke ~"categorical",
        hypertension ~"categorical",
        education ~"categorical",
        income ~"categorical",
        region ~"categorical",
        marital ~"categorical",
        peptic_ulcer ~"categorical",
        dbp_mean ~"continuous",
        waist_hip_ratio ~"continuous",
        BRI~"continuous",
        ABSI~"continuous",
        impedance_ohms~"continuous",
        fat_percent_x10~"continuous",
        waist_hip_ratio~"continuous",
        BMI_change~"continuous",
        waist_mm~"continuous",
        family_cancer~"categorical",
        sbp_mean ~"continuous",
        bmi_calc ~"continuous",
        BMI25 ~"continuous"),
      label = list(age ~ 'Age (years),mean(SD)', 
                   dbp_mean ~ 'dbp_mean (mmHg),mean(SD)',
                   sbp_mean ~ 'sbp_mean (mmHg),mean(SD)',
                   fat_percent_x10 ~ 'FAT (%)',
                   hip_mm ~ 'hip_mm (mm),mean(SD)',
                   impedance_ohms ~ 'impedance_ohms (ohms),mean(SD)',
                   waist_hip_ratio ~ 'waist_hip_ratio (ratio),mean(SD)',
                   waist_mm ~ 'waist (mm),mean(SD)',
                   gender ~ 'Gender,n(%)',
                   education ~'Education level,n(%)',
                   marital ~ 'Marital Status,n(%)',
                   hypertension ~ 'Hypertension,n(%)',
                   diabetes ~ 'Diabetes,n(%)',
                   #cancer ~ 'Cancer or malignant tumor,n(%)',
                   #Gastrointestinal ~ 'Gastrointestinal Tumors,n(%)', 
                   region ~ 'region,n(%)',
                   peptic_ulcer ~ 'peptic_ulcer,n(%)',
                   income ~ 'income,n(%)'),
      statistic = list(all_continuous()  ~ "{mean} ({sd})", 
                       all_categorical() ~ "{n} ({p}%)"),
      digits = all_continuous() ~2,
      sort = list(gender ~ "frequency",
                  education~"frequency",
                  marital~"frequency")) %>%
    add_n() %>%
    add_overall() %>%
    add_p() %>%
    as_flex_table() %>%
    flextable::save_as_docx(path = paste0('summary_',dat_name,group,'.docx'))
}

#####5.2 Result-Baseline data#####
table(zzdata$cancer_type)
tab_common(zzdata_str,"Gastrointestinal")
tab_common(zzdata_str,"cancer_type")

data_crc_id<- case[case$cancer_type =='colon cancer',]
data_crc <- zzdata_str[zzdata_str$str_id %in% data_crc_id$str_id,]
data_gc_id<- case[case$cancer_type =='gastric cancer',]
data_gc <- zzdata_str[zzdata_str$str_id %in% data_gc_id$str_id,]
tab_common(data_gc,"cancer_type")
tab_common(data_crc ,"cancer_type")

####6. Conditional logistic####
zzdata_str <- zzdata_str %>% 
  mutate(
    education = factor(education),
    marital   = factor(marital) ,
    income  =   factor(income),
    hypertension = factor(hypertension),
    diabetes = factor(diabetes)
  )

rt1 <- smal
rt1 <- rt1[rt1$csid %in% zzdata_str$csid,]
zzdata_str <- merge(zzdata_str,rt1)
table(zzdata_str$smoking_category)
zzdata_str$smoke <- ifelse(zzdata_str$smoking_category == 1,"Never smoker",
                             ifelse(zzdata_str$smoking_category == 2, "Occasional smoker",
                                    if_else(zzdata_str$smoking_category == 3,"Ex regular smoker","Smoker")))
# zzdata_str$smoke [zzdata_str$smoke  %in% c("Occasional smoker","Ex regular smoker")] <- "Current smoker"
zzdata_str$smoke [zzdata_str$smoke  %in% c("Never smoker","Ex regular smoker")] <- "Abstainer/Former smoker"
table(zzdata_str$smoke)

table(zzdata_str$alcohol_category)
zzdata_str$alco <- ifelse(zzdata_str$alcohol_category == 1,"Never regular",
                             ifelse(zzdata_str$alcohol_category == 2, "Ex-regular",
                                    if_else(zzdata_str$alcohol_category == 3,"Occasional",
                                            ifelse(zzdata_str$alcohol_category == 4,"Monthly",
                                                   ifelse(zzdata_str$alcohol_category == 5,"Reduced intake","Weekly")))))

zzdata_str$alco[zzdata_str$alco %in% c("Ex-regular","Reduced intake")] <- "Former drinker"
#zzdata_str$alco[zzdata_str$alco %in% c("Occasional","Monthly")] <- "Infrequent drinker"
zzdata_str$alco[zzdata_str$alco %in% c("Former drinker","Never regular")] <- "Abstainer/Former drinker"
table(zzdata_str$alco)





#####6.1 define function—Conditional logistic#####
logregression <- function(dat, form) {
  result <- c()
  #基础模型
  formula1 <- as.formula(paste("outcome ~ ", form,"+strata(str_id)"))
  fit1 <- clogit(formula1, data = dat)
  a <- summary(fit1)
  A <- as.data.frame(a$coefficients)
  OR1 <- round(A[,"exp(coef)"], 3)
  OR_CI1 <- round(as.data.frame(a$conf.int)[,c("lower .95", "upper .95")], 3)
  he1 <- paste(OR1, "(", OR_CI1[,1], ",", OR_CI1[,2], ")", sep = "")
  result$group <- rownames(A)
  result$OR1 <- he1
  result$P1 <- round(A[,"Pr(>|z|)"], 3)
  result <- as.data.frame(result)
  #调整模型1
  formula2 <- as.formula(paste("outcome ~ ", form,"+education+marital+income+strata(str_id)"))
  fit2 <- clogit(formula2, data = dat)
  a <- summary(fit2)
  A <- as.data.frame(a$coefficients)
  OR2 <- round(A[,"exp(coef)"], 3)
  OR_CI2 <- round(as.data.frame(a$conf.int)[,c("lower .95", "upper .95")], 3)
  he2 <- paste(OR2, "(", OR_CI2[,1], ",", OR_CI2[,2], ")", sep = "")
  result$OR2 <- he2[1:nrow(result)]
  P2 <- round(A[,"Pr(>|z|)"], 3)
  result$P2 <- P2[1:nrow(result)]
  #调整模型2
  formula3 <- as.formula(paste("outcome ~ ", form,"+smoke+alco+education+marital+income+hypertension+diabetes+peptic_ulcer+family_cancer+strata(str_id)"))
  fit3 <- clogit(formula3, data = dat)
  a <- summary(fit3)
  A <- as.data.frame(a$coefficients)
  OR3 <- round(A[,"exp(coef)"], 3)
  OR_CI3 <- round(as.data.frame(a$conf.int)[,c("lower .95", "upper .95")], 3)
  he3 <- paste(OR3, "(", OR_CI3[,1], ",", OR_CI3[,2], ")", sep = "")
  result$OR3 <- he3[1:nrow(result)]
  P3 <- round(A[,"Pr(>|z|)"], 3)
  result$P3 <- P3[1:nrow(result)]
  return(result)
}

####6.2 EOGIC#####
bl <-c('ABSI','ABSI_group','weight_change_ratio','BMI_change_ratio','BMI25','BMI25_5unit','bmi_calc','BMI_5unit','BMI_change','BMI_change_5unit','hip_mm','Hip_10unit','waist_mm','Wasit_10unit','waist_hip_ratio','fat_percent_x10','BRI')
result0 <- NULL
resultt <- c()
for (i in 1:length(bl)) {
  df <- bl[i]
  resultt <- logregression(dat = zzdata_str,form = df)
  if (is.null(result0)) {
    result0 <- resultt  # 如果result0为空，则直接赋值
  } else {
    result0 <- rbind(result0, resultt)  # 否则按列合并结果
  }
}
#####6.3 EOGC#####
data_gc_id<- case[case$cancer_type =='gastric cancer',]
data_gc <- zzdata_str[zzdata_str$str_id %in% data_gc_id$str_id,]
bl <-c('ABSI_0.001','ABSI','ABSI_group','weight_change_ratio','BMI_change_ratio','BMI25','BMI25_5unit','bmi_calc','BMI_5unit','BMI_change','BMI_change_5unit','hip_mm','Hip_10unit','waist_mm','Wasit_10unit','waist_hip_ratio','fat_percent_x10','BRI')
result0 <- NULL
resultt <- c()
for (i in 1:length(bl)) {
  df <- bl[i]
  resultt <- logregression(dat = data_gc,form = df)
  if (is.null(result0)) {
    result0 <- resultt  # 如果result0为空，则直接赋值
  } else {
    result0 <- rbind(result0, resultt)  # 否则按列合并结果
  }
}

#####6.4 EOCRC#####
data_crc_id<- case[case$cancer_type =='colon cancer',]
data_crc <- zzdata_str[zzdata_str$str_id %in% data_crc_id$str_id,]
bl <-c('ABSI_0.001','ABSI','ABSI_group','weight_change_ratio','BMI_change_ratio','BMI25','BMI25_5unit','bmi_calc','BMI_5unit','BMI_change','BMI_change_5unit','hip_mm','Hip_10unit','waist_mm','Wasit_10unit','waist_hip_ratio','fat_percent_x10','BRI')
result0 <- NULL
resultt <- c()
for (i in 1:length(bl)) {
  df <- bl[i]
  resultt <- logregression(dat = data_crc,form = df)
  if (is.null(result0)) {
    result0 <- resultt  # 如果result0为空，则直接赋值
  } else {
    result0 <- rbind(result0, resultt)  # 否则按列合并结果
  }
}

####7.RCS####
#####7.1 define function-RCS#####
fit_rms <- function(dat,indicates) {
  dd <- datadist(dat)
  options(datadist = "dd")
  ## 1. 条件 logistic
  fit.clogit <- clogit(
    as.formula(sprintf("outcome ~ rcs(%s, 4) + strata(str_id)", indicates)),
    data = dat, method = "exact")
  ## 2. 转 rms 对象
  fit<- cph(
    as.formula(sprintf("Surv(rep(1, nrow(dat)), outcome) ~ rcs(%s, 4) + strat(str_id)", indicates)),
    data = dat, init = fit.clogit$coef, method = "exact", x = TRUE, y = TRUE)
  return(fit)
}

RCS_plot_gc <- function(dat,indicates){
  ## 把离散 (BMI, OR-1) 变成连续函数
  sp  <- splines::interpSpline(OR[[indicates]], OR$yhat - 1)  # 平滑
  f   <- function(x) predict(sp, x)$y                            # 连续版本
  ## 在整个 BMI 范围里找所有根
  roots     <- uniroot.all(f, range(OR[[indicates]]))   # 返回 0、1 或 2 个根
  ## 4. 提取 P-overall 与 P-nonlinear
  an <- anova(fit.rms)
  p_overall  <- round(an[indicates, "P"], 3)
  p_nonlin   <- round(an[" Nonlinear", "P"], 3)
  p_lab <- sprintf("P-overall = %s\nP-non-linear = %s",
                   format.pval(p_overall, digits = 3, eps = 0.001),
                   format.pval(p_nonlin,  digits = 3, eps = 0.001))
  ggplot() +
    geom_line(data = OR,mapping = aes_string(x = indicates, y = "yhat"),
              size = 1, colour = "#0070b9") +
    geom_ribbon(data = OR,
                mapping = aes_string(x = indicates,ymin = "lower", ymax = "upper"),
                alpha = 0.1, fill = "#0070b9") +
    geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
    ## 多根：一次性画所有竖线
    geom_vline(xintercept = roots, size = 1, colour = "#d40e8c") +
    annotate("text", x = Inf, y = Inf,
             label = p_lab, hjust = 1.1, vjust = 1.1, size = 4) +
    labs(title = "EOGC risk",
         y = "Odds Ratio (95% CI)") +
    theme_classic(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5))+
    ## 把两根值写图上
    lapply(seq_along(roots), function(i)
      annotate("text", x = roots[i], y = 1,
               label = sprintf("OR = 1\n%.3f", roots[i]),
               colour = "black", size = 4, hjust = -0.5, vjust = -1))
}
RCS_plot_crc <- function(dat,indicates){
  
  ## 把离散 (BMI, OR-1) 变成连续函数
  sp  <- splines::interpSpline(OR[[indicates]], OR$yhat - 1)  # 平滑
  f   <- function(x) predict(sp, x)$y                            # 连续版本
  ## 在整个 BMI 范围里找所有根
  roots     <- uniroot.all(f, range(OR[[indicates]]))   # 返回 0、1 或 2 个根
  ## 4. 提取 P-overall 与 P-nonlinear
  an <- anova(fit.rms)
  p_overall  <- round(an[indicates, "P"], 3)
  p_nonlin   <- round(an[" Nonlinear", "P"], 3)
  p_lab <- sprintf("P-overall = %s\nP-non-linear = %s",
                   format.pval(p_overall, digits = 3, eps = 0.001),
                   format.pval(p_nonlin,  digits = 3, eps = 0.001))
  ggplot() +
    geom_line(data = OR,mapping = aes_string(x = indicates, y = "yhat"),
              size = 1, colour = "#0070b9") +
    geom_ribbon(data = OR,
                mapping = aes_string(x = indicates,ymin = "lower", ymax = "upper"),
                alpha = 0.1, fill = "#0070b9") +
    geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
    ## 多根：一次性画所有竖线
    geom_vline(xintercept = roots, size = 1, colour = "#d40e8c") +
    annotate("text", x = Inf, y = Inf,
             label = p_lab, hjust = 1.1, vjust = 1.1, size = 4) +
    labs(title = "EOCRC risk",
         y = "Odds Ratio (95% CI)") +
    theme_classic(base_size = 14) +
    theme(plot.title = element_text(hjust = 0.5))+
    ## 把两根值写图上
    lapply(seq_along(roots), function(i)
      annotate("text", x = roots[i], y = 1,
               label = sprintf("%.3f", roots[i]),
               colour = "black", size = 4, hjust = -0.5, vjust = -1))
}

#####7.2EOGC#####
data_gc_id<- case[case$cancer_type =='gastric cancer',]
data_gc <- zzdata_str[zzdata_str$str_id %in% data_gc_id$str_id,]
tt_gc <- data_gc[, c('BMI_change','BMI25_5unit','BMI_5unit','BMI_change_5unit','ABSI','BRI','BMI_change_ratio','Hip_10unit','Wasit_10unit','waist_hip_ratio','fat_percent_x10','outcome','str_id')]
dd <- datadist(tt_gc)
options(datadist = "dd")
{
  fit.rms <- fit_rms(tt_gc,'BMI25_5unit')
  OR <- rms::Predict(fit.rms, BMI25_5unit, fun = exp, ref.zero = TRUE)
  p1 <- RCS_plot_gc(tt_gc,'BMI25_5unit')
}
{
  fit.rms <- fit_rms(tt_gc,'BMI_5unit')
  OR <- rms::Predict(fit.rms, BMI_5unit, fun = exp, ref.zero = TRUE)
  p2 <- RCS_plot_gc(tt_gc,'BMI_5unit')
}
{
  fit.rms <- fit_rms(tt_gc,'BMI_change_5unit')
  OR <- rms::Predict(fit.rms,BMI_change_5unit, fun = exp, ref.zero = TRUE)
  p3 <- RCS_plot_gc(tt_gc,'BMI_change_5unit')
}
{
  fit.rms <- fit_rms(tt_gc,'ABSI')
  OR <- rms::Predict(fit.rms, ABSI, fun = exp, ref.zero = TRUE)
  p4 <- RCS_plot_gc(tt_gc,'ABSI')
}
{
  fit.rms <- fit_rms(tt_gc,'BRI')
  OR <- rms::Predict(fit.rms, BRI, fun = exp, ref.zero = TRUE)
  p5 <- RCS_plot_gc(tt_gc,'BRI')
}
{
  fit.rms <- fit_rms(tt_gc,'BMI_change_ratio')
  OR <- rms::Predict(fit.rms, BMI_change_ratio, fun = exp, ref.zero = TRUE)
  p6 <- RCS_plot_gc(tt_gc,'BMI_change_ratio')
}
{
  fit.rms <- fit_rms(tt_gc,'Hip_10unit')
  OR <- rms::Predict(fit.rms, Hip_10unit, fun = exp, ref.zero = TRUE)
  p7 <- RCS_plot_gc(tt_gc,'Hip_10unit')
}
{
  fit.rms <- fit_rms(tt_gc,'Wasit_10unit')
  OR <- rms::Predict(fit.rms, Wasit_10unit, fun = exp, ref.zero = TRUE)
  p8 <- RCS_plot_gc(tt_gc,'Wasit_10unit')
}
{
  fit.rms <- fit_rms(tt_gc,'fat_percent_x10')
  OR <- rms::Predict(fit.rms, fat_percent_x10, fun = exp, ref.zero = TRUE)
  p9 <- RCS_plot_gc(tt_gc,'fat_percent_x10')
}

{
  fit.rms <- fit_rms(tt_gc,'BMI_change')
  OR <- rms::Predict(fit.rms, BMI_change, fun = exp, ref.zero = TRUE)
  p10 <- RCS_plot_gc(tt_gc,'BMI_change')
}

library(patchwork)
p1 <- p1 + theme(aspect.ratio = 0.7)
p2 <- p2 + theme(aspect.ratio = 0.7)
p3 <- p3 + theme(aspect.ratio = 0.7)
p4 <- p4 + theme(aspect.ratio = 0.7)
p5 <- p5 + theme(aspect.ratio = 0.7)
p6 <- p6 + theme(aspect.ratio = 0.7)
p7 <- p7 + theme(aspect.ratio = 0.7)
p8 <- p8 + theme(aspect.ratio = 0.7)
p9 <- p9 + theme(aspect.ratio = 0.7)

fig_gc <- (p1 | p2 | p3) / (p4 | p5 | p6) / (p7 | p8 | p9) +
  plot_annotation(tag_levels = 'A')+
  theme(plot.tag = element_text(size = 18, face = 'bold'))

print(fig_gc)
ggsave("RCS_gc.pdf", fig_gc, width = 16, height = 12, device = "pdf")

#####7.3 EOCRC#####
data_crc <- zzdata_str[zzdata_str$str_id %in% data_crc_id$str_id,]
tt_crc <- data_crc[, c('BMI_change','BMI25','BMI25_5unit','BMI_5unit','BMI_change_5unit','ABSI','BRI','BMI_change_ratio','Hip_10unit','Wasit_10unit','waist_hip_ratio','fat_percent_x10','outcome','str_id')]
dd <- datadist(tt_crc)
options(datadist = "dd")
{
  fit.rms <- fit_rms(tt_crc,'BMI25_5unit')
  OR <- rms::Predict(fit.rms, BMI25_5unit, fun = exp, ref.zero = TRUE)
  pp1 <- RCS_plot_crc(tt_crc,'BMI25_5unit')
}
{
  fit.rms <- fit_rms(tt_crc,'BMI_5unit')
  OR <- rms::Predict(fit.rms, BMI_5unit, fun = exp, ref.zero = TRUE)
  pp2 <- RCS_plot_crc(tt_crc,'BMI_5unit')
}
{
  fit.rms <- fit_rms(tt_crc,'BMI_change_5unit')
  OR <- rms::Predict(fit.rms,BMI_change_5unit, fun = exp, ref.zero = TRUE)
  pp3 <- RCS_plot_crc(tt_crc,'BMI_change_5unit')
}
{
  fit.rms <- fit_rms(tt_crc,'ABSI')
  OR <- rms::Predict(fit.rms, ABSI, fun = exp, ref.zero = TRUE)
  pp4 <- RCS_plot_crc(tt_crc,'ABSI')
}
{
  fit.rms <- fit_rms(tt_crc,'BRI')
  OR <- rms::Predict(fit.rms, BRI, fun = exp, ref.zero = TRUE)
  pp5 <- RCS_plot_crc(tt_crc,'BRI')
}
{
  fit.rms <- fit_rms(tt_crc,'BMI_change_ratio')
  OR <- rms::Predict(fit.rms, BMI_change_ratio, fun = exp, ref.zero = TRUE)
  pp6 <- RCS_plot_crc(tt_crc,'BMI_change_ratio')
}
{
  fit.rms <- fit_rms(tt_crc,'Hip_10unit')
  OR <- rms::Predict(fit.rms, Hip_10unit, fun = exp, ref.zero = TRUE)
  pp7 <- RCS_plot_crc(tt_crc,'Hip_10unit')
}
{
  fit.rms <- fit_rms(tt_crc,'Wasit_10unit')
  OR <- rms::Predict(fit.rms, Wasit_10unit, fun = exp, ref.zero = TRUE)
  pp8 <- RCS_plot_crc(tt_crc,'Wasit_10unit')
}
{
  fit.rms <- fit_rms(tt_crc,'fat_percent_x10')
  OR <- rms::Predict(fit.rms, fat_percent_x10, fun = exp, ref.zero = TRUE)
  pp9 <- RCS_plot_crc(tt_crc,'fat_percent_x10')
}
{
  fit.rms <- fit_rms(tt_crc,'BMI_change')
  OR <- rms::Predict(fit.rms, BMI_change, fun = exp, ref.zero = TRUE)
  pp10 <- RCS_plot_crc(tt_crc,'BMI_change')
}
library(patchwork)
pp1 <- pp1 + theme(aspect.ratio = 0.7)
pp2 <- pp2 + theme(aspect.ratio = 0.7)
pp3 <- pp3 + theme(aspect.ratio = 0.7)
pp4 <- pp4 + theme(aspect.ratio = 0.7)
pp5 <- pp5 + theme(aspect.ratio = 0.7)
pp6 <- pp6 + theme(aspect.ratio = 0.7)
pp7 <- pp7 + theme(aspect.ratio = 0.7)
pp8 <- pp8 + theme(aspect.ratio = 0.7)
pp9 <- pp9 + theme(aspect.ratio = 0.7)

fig_crc <- (pp1 | pp2 | pp3) / (pp4 | pp5 | pp6) / (pp7 | pp8 | pp9) +
  plot_annotation(tag_levels = 'A')+
  theme(plot.tag = element_text(size = 18, face = 'bold'))

print(fig_crc)
ggsave("RCS_crc.pdf", fig_crc, width = 16, height = 12, device = "pdf")

#####7.4 Figure.2#####
fig_2 <- (p4 | p10) / (pp4 | pp10)+
  plot_annotation(tag_levels = 'A')+
  theme(plot.tag = element_text(size = 18, face = 'bold'))
print(fig_2)
ggsave("Fig.2.pdf", fig_2, width = 12, height = 9, device = "pdf")

####8. Figure.1####
#####8.1 Left#####
p1 <- ggplot(case, aes(BMI25, BMI_change,colour = cancer_type)) +
  # 1. 映射分组到颜色 / 大小 / 透明度
  geom_point(aes(size   = cancer_type)) +
  scale_colour_manual(values = c("colon cancer" = "#2C6EB6",
                                 "gastric cancer"  = "grey50")) +
  scale_size_manual(values   = c("colon cancer" = 2.5,
                                 "gastric cancer"  = 2.5)) +
  scale_alpha_manual(values  = c("colon cancer" = 1.0,
                                 "gastric cancer"  = 0.7)) +
  geom_smooth(aes(colour = cancer_type, group = cancer_type),
              method = "glm", se = TRUE, size = 1, alpha = 0.2) +
  stat_poly_eq(
    aes(label = paste(after_stat(eq.label),
                      after_stat(rr.label),
                      after_stat(p.value.label),
                      sep = "~~~"),
        colour = cancer_type),
    formula = y ~ x, parse = TRUE, size = 4.5,
    label.x.npc = 0.05, label.y.npc = c(0.95, 0.9),
    hjust = 0, vjust = 1
  ) +
  theme_gray(base_family = "sans") +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    panel.border     = element_rect(colour = "black", fill = NA, size = 1.2),
    panel.grid.major = element_line(colour = "grey", size = 0.4),
    panel.grid.minor = element_line(colour = "grey", size = 0.4, linetype = "dotted"),
    plot.title   = element_text(face = "bold", size = 22, hjust = 0.5, colour = "#0059B2"),
    axis.title.x = element_text(size = 18, colour = "#333333"),
    axis.title.y = element_text(size = 18, colour = "#333333"),
    axis.text    = element_text(size = 15, colour = "#333333"),
    legend.position      = "none"
  ) +
  labs(x = "BMI at Age 25 (kg/m²)",
       y = "BMI Change (kg/m²)")
p1

p2 <- ggplot(case, aes(BMI25, fill = cancer_type)) +
  geom_histogram(position = "identity", alpha = 0.7,
                 colour = "white", bins = 30) +
  scale_fill_manual(values = c("colon cancer"   = "#2C6EB6",
                               "gastric cancer" = "grey50")) +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.border     = element_blank(),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        legend.position  = "top",
        legend.title     = element_blank())

p3 <- ggplot(case, aes(BMI_change, fill = cancer_type)) +
  geom_histogram(position = "identity", alpha = 0.7,
                 colour = "white", bins = 30) +
  scale_fill_manual(values = c("colon cancer"   = "#2C6EB6",
                               "gastric cancer" = "grey50")) +
  coord_flip() +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.border     = element_blank(),
        panel.grid       = element_blank(),
        axis.title       = element_blank(),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        legend.position = "none",
        legend.title     = element_blank())

empty <- ggplot()+geom_point(aes(1,1), colour="white") +
  theme(                              
    plot.background = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.border = element_blank(), 
    panel.background = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank()
    ,plot.margin=unit(c(0.1, 0.1, 0, 0), "inches")
  )
pg1=ggpubr::ggarrange(p2,p1, ncol = 1, nrow = 2,heights = c(0.3,1),align = "v")
pg2=ggpubr::ggarrange(empty,p3, ncol = 1, nrow = 2,heights = c(0.2,1),align = "v")
final=ggpubr::ggarrange(pg1,pg2, ncol = 2, nrow = 1,widths = c(1,0.3),align = "h")
final
ggsave("Fig.1.Left.pdf", final, width = 11, height = 11, device = "pdf")

#####8.2 Right#####
data_crc_id<- case[case$cancer_type =='colon cancer',]
data_crc <- zzdata_str[zzdata_str$str_id %in% data_crc_id$str_id,]
bl <-c('BMI25_5unit','BMI_change_5unit','BMI25_5unit+BMI_change_5unit')
result0 <- NULL
resultt <- c()
for (i in 1:length(bl)) {
  df <- bl[i]
  resultt <- logregression(dat = data_crc,form = df)
  if (is.null(result0)) {
    result0 <- resultt  # 如果result0为空，则直接赋值
  } else {
    result0 <- rbind(result0, resultt)  # 否则按列合并结果
  }
}

dat <- result0 %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(cols = -c(group, id),
               names_to = c(".value", "model"),
               names_pattern = "(OR|P)(\\d)") %>% 
  # 1. 先把原始字符串留一份
  mutate(OR_raw = OR) %>% 
  # 2. 在字符串上提取
  mutate(
    OR   = str_extract(OR_raw, "^[\\d\\.]+") %>% as.numeric,
    low  = str_extract(OR_raw, "(?<=\\()[\\d\\.]+") %>% as.numeric,
    high = str_extract(OR_raw, "[\\d\\.]+(?=\\))") %>% as.numeric
  ) %>% 
  dplyr::select(group, model, OR, OR_raw,low, high, P)

dat <- dat %>% 
  add_row(group = "Single-exposure estimates", .before = 1)
# 原第 7 行现在变成了第 8 行，再在它前面插入
dat <- dat %>% 
  add_row(group = "Mutually adjusted estimates", .before = 8)
dat <- dat %>% 
  mutate(across(everything(), ~replace(., is.na(.), ""))) 
dat$OR <- as.numeric(dat$OR)
dat$high<- as.numeric(dat$high)
dat$low<- as.numeric(dat$low)
dat[c(2,4,5,7,9,11,12,14),1] <- ""
dat[c(3,10),1] <-'BMI25 (per 5kg/m²)'
dat[c(6,13),1] <-'BMI change (per 5kg/m²)'
dat[c(5,6),7] <-'<0.001'
dat$" "<-paste(rep(" ",40),collapse="")
colnames(dat)
names(dat)=c('Characteristics','Model','OR','OR(95%CI)','low','high','P','')
tm <- forest_theme(base_size = 10,  #文本的大小                   
                   # Confidence interval point shape, line type/color/width                   
                   ci_pch = 15,   #可信区间点的形状                  
                   ci_col = "#644a81",    #CI的颜色                  
                   ci_fill = "black",     #ci颜色填充            
                   ci_alpha = 0.8,        #ci透明度                  
                   ci_lty = 1,            #CI的线型                 
                   ci_lwd = 1.5,          #CI的线宽                  
                   ci_Theight = 0.2, # Set an T end at the end of CI  ci的高度，默认是NULL    
                   # Reference line width/type/color   参考线默认的参数，中间的竖的虚线  
                   refline_lwd = 1,       #中间的竖的虚线                 
                   refline_lty = "dashed",                
                   refline_col = "grey20",               
                   # Vertical line width/type/color  垂直线宽/类型/颜色   可以添加一条额外的垂直线，如果没有就不显示              
                   vertline_lwd = 1,              #可以添加一条额外的垂直线，如果没有就不显示                  
                   vertline_lty = "dashed",                   
                   vertline_col = "grey20",                   # Change summary color for filling and borders   更改填充和边框的摘要颜色                
                   summary_fill = "yellow",       #汇总部分大菱形的颜色                  
                   summary_col = "#4575b4",
                   strip_bg = NA)                  # Footnote font size/face/color  脚注字体大小/字体/颜色
log_breaks <- c(0.25, 0.5, 1, 2, 4)
p <- forest(dat[,c(1:2,8,4,7)],       
       est = dat$OR,       #效应值       
       lower = dat$low,     #可信区间下限       
       upper = dat$high,      #可信区间上限       
       ci_column =3,   #在那一列画森林图，要选空的那一列
       ref_line = 1,       
       arrow_lab =c("Decrease","Increase"),
       theme = tm,
       xlim = c(0.25, 4),    
       x_trans = "log" ,
       ticks_at = c(0.25, 0.5, 1, 2, 4))
pp <- add_border(p, part = "header", where = "bottom",row = 0)
pp <- add_border(pp, part = "header", where = "bottom")
pp <- edit_plot(pp, row = c(1,8),col = 1, gp = gpar(col = "red4", fontface ="bold.italic"))
pp <- edit_plot(pp, row = c(3,6,10,13),col = 1, gp = gpar(col = "black", fontface = "italic"))
pp
ggsave("Fig.1.right.pdf", pp, width = 10, height = 10, device = "pdf")

####9. Figure.3####
#####9.1 Left-EOCRC#####
data_crc_id<- case[case$cancer_type =='colon cancer',]
data_crc <- zzdata_str[zzdata_str$str_id %in% data_crc_id$str_id,]
bl <-c('BMI25_5unit','BMI_change_5unit','ABSI_0.001')
result0 <- NULL
resultt <- c()
for (i in 1:length(bl)) {
  df <- bl[i]
  resultt <- logregression(dat = data_crc,form = df)
  if (is.null(result0)) {
    result0 <- resultt  # 如果result0为空，则直接赋值
  } else {
    result0 <- rbind(result0, resultt)  # 否则按列合并结果
  }
}
result_EOCRC <- result0
dat <- result_EOCRC %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(cols = -c(group, id),
               names_to = c(".value", "model"),
               names_pattern = "(OR|P)(\\d)") %>% 
  mutate(OR_raw = OR) %>% 
  mutate(
    OR   = str_extract(OR_raw, "^[\\d\\.]+") %>% as.numeric,
    low  = str_extract(OR_raw, "(?<=\\()[\\d\\.]+") %>% as.numeric,
    high = str_extract(OR_raw, "[\\d\\.]+(?=\\))") %>% as.numeric
  ) %>% 
  dplyr::select(group, model, OR, OR_raw,low, high, P)
dat <- dat %>%
  mutate(across(everything(), ~replace(., is.na(.), "")))
dat$OR <- as.numeric(dat$OR)
dat$high<- as.numeric(dat$high)
dat$low<- as.numeric(dat$low)
dat[c(1,3,4,6,7,9),1] <- ""
dat[2,1] <-'BMI25 (per 5kg/m²)'
dat[5,1] <-'BMI change (per 5kg/m²)'
dat[8,1] <-'ABSI (per 0.001)'
dat$" "<-paste(rep(" ",40),collapse="")
colnames(dat)
names(dat)=c('Characteristics','Model','OR','OR(95%CI)','low','high','P','')
p2 <- forest(dat[,c(1:2,8,4,7)],       
             est = dat$OR,       #效应值       
             lower = dat$low,     #可信区间下限       
             upper = dat$high,      #可信区间上限       
             ci_column =3,   #在那一列画森林图，要选空的那一列
             ref_line = 1,       
             arrow_lab =c("Decrease","Increase"),
             theme = tm, xlim = c(0.25, 4),    
             x_trans = "log" ,
             ticks_at = c(0.25, 0.5, 1, 2,4))
pp2 <- add_border(p2, part = "header", where = "bottom",row = 0)
pp2 <- add_border(pp2, part = "header", where = "bottom")
pp2 <- edit_plot(pp2, row = c(2,5,8),col = 1, gp = gpar(col = "black", fontface = "italic"))
pp2
ggsave("Fig.3.Left.pdf", pp2, width = 10, height = 10, device = "pdf")

#####9.2 Right-all year CRC#####
dat_crc_all <- data[data$cancer_type %in% c('colon cancer','none_cancer'),]
case <- dat_crc_all[dat_crc_all$cancer_type=='colon cancer',]
case$str_id <-1:nrow(case)
control <- dat_crc_all[dat_crc_all$cancer_type=='none_cancer',]
matched_ctrl <- match_riskset_fast_summary(case, control, ratio = 5, seed = 81)
n_id <- pool_sz[pool_sz$ctrl_pool<5,1]
case <- case[!(case$csid %in% n_id$csid),]
matched_ctrl <- match_riskset_fast_summary(case, control, ratio = 5, seed = 81)
csid_id<- c(matched_ctrl$csid,case$csid)
zzdata <- data[data$csid %in% csid_id,]
case_str_id <- case[,c("csid","str_id")]
control_str_id <- matched_ctrl[,c("csid","str_id")]
data_str_id <- rbind(case_str_id ,control_str_id)
zzdata_str <- merge(zzdata,data_str_id)
zzdata_str$outcome <- as.numeric(zzdata$outcome)
ABSI_group<- quantile(zzdata_str$ABSI, probs=c(0,1/4,2/4,3/4,1))
zzdata_str$ABSI_group <- cut(zzdata_str$ABSI,
                             breaks = ABSI_group,
                             labels = c('Q1_ABSI', 'Q2_ABSI', 'Q3_ABSI','Q4_ABSI'))

rt1 <- smal
rt1 <- rt1[rt1$csid %in% zzdata_str$csid,]
zzdata_str <- merge(zzdata_str,rt1)
table(zzdata_str$smoking_category)
zzdata_str$smoke <- ifelse(zzdata_str$smoking_category == 1,"Never smoker",
                           ifelse(zzdata_str$smoking_category == 2, "Occasional smoker",
                                  if_else(zzdata_str$smoking_category == 3,"Ex regular smoker","Smoker")))
zzdata_str$smoke [zzdata_str$smoke  %in% c("Never smoker","Ex regular smoker")] <- "Abstainer/Former smoker"
table(zzdata_str$smoke)

table(zzdata_str$alcohol_category)
zzdata_str$alco <- ifelse(zzdata_str$alcohol_category == 1,"Never regular",
                          ifelse(zzdata_str$alcohol_category == 2, "Ex-regular",
                                 if_else(zzdata_str$alcohol_category == 3,"Occasional",
                                         ifelse(zzdata_str$alcohol_category == 4,"Monthly",
                                                ifelse(zzdata_str$alcohol_category == 5,"Reduced intake","Weekly")))))
zzdata_str$alco[zzdata_str$alco %in% c("Ex-regular","Reduced intake")] <- "Former drinker"
# zzdata_str$alco[zzdata_str$alco %in% c("Occasional","Monthly")] <- "Infrequent drinker"
zzdata_str$alco[zzdata_str$alco %in% c("Former drinker","Never regular")] <- "Abstainer/Former drinker"
table(zzdata_str$alco)



data_crc <- zzdata_str
bl <-c('BMI25_5unit','BMI_change_5unit','ABSI_0.001')
result0 <- NULL
resultt <- c()
for (i in 1:length(bl)) {
  df <- bl[i]
  resultt <- logregression(dat = data_crc,form = df)
  if (is.null(result0)) {
    result0 <- resultt  # 如果result0为空，则直接赋值
  } else {
    result0 <- rbind(result0, resultt)  # 否则按列合并结果
  }
}
result_allCRC <- result0
dat <- result_allCRC %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(cols = -c(group, id),
               names_to = c(".value", "model"),
               names_pattern = "(OR|P)(\\d)") %>% 
  mutate(OR_raw = OR) %>% 
  mutate(
    OR   = str_extract(OR_raw, "^[\\d\\.]+") %>% as.numeric,
    low  = str_extract(OR_raw, "(?<=\\()[\\d\\.]+") %>% as.numeric,
    high = str_extract(OR_raw, "[\\d\\.]+(?=\\))") %>% as.numeric
  ) %>% 
  dplyr::select(group, model, OR, OR_raw,low, high, P)
dat <- dat %>%
  mutate(across(everything(), ~replace(., is.na(.), "")))
dat$OR <- as.numeric(dat$OR)
dat$high<- as.numeric(dat$high)
dat$low<- as.numeric(dat$low)
dat[c(1,3,4,6,7,9),1] <- ""
dat[2,1] <-'BMI25 (per 5kg/m²)'
dat[5,1] <-'BMI change (per 5kg/m²)'
dat[8,1] <-'ABSI (per 0.001)'
dat$" "<-paste(rep(" ",40),collapse="")
colnames(dat)
names(dat)=c('Characteristics','Model','OR','OR(95%CI)','low','high','P','')
dat[c(4,7,8,9),7] <-'<0.001'
p3 <- forest(dat[,c(1:2,8,4,7)],       
             est = dat$OR,       #效应值       
             lower = dat$low,     #可信区间下限       
             upper = dat$high,      #可信区间上限       
             ci_column =3,   #在那一列画森林图，要选空的那一列
             ref_line = 1,       
             arrow_lab =c("Decrease","Increase"),
             theme = tm, xlim = c(0.5, 2),    
             x_trans = "log" ,
             ticks_at = c(0.5, 1, 2))
pp3 <- add_border(p3, part = "header", where = "bottom",row = 0)
pp3 <- add_border(pp3, part = "header", where = "bottom")
pp3 <- edit_plot(pp3, row = c(2,5,8),col = 1, gp = gpar(col = "black", fontface = "italic"))
pp3
ggsave("Fig.3.Right.pdf", pp3, width = 10, height = 10, device = "pdf")

#####9.3 baseline-all year CRC#####
tab_common(zzdata_str,'cancer_type')

#####9.4 logistic-all year CRC #####
bl <-c('ABSI_0.001','ABSI','ABSI_group','weight_change_ratio','BMI_change_ratio','BMI25','BMI25_5unit','bmi_calc','BMI_5unit','BMI_change','BMI_change_5unit','hip_mm','Hip_10unit','waist_mm','Wasit_10unit','waist_hip_ratio','fat_percent_x10','BRI')
result0 <- NULL
resultt <- c()
for (i in 1:length(bl)) {
  df <- bl[i]
  resultt <- logregression(dat = data_crc,form = df)
  if (is.null(result0)) {
    result0 <- resultt  # 如果result0为空，则直接赋值
  } else {
    result0 <- rbind(result0, resultt)  # 否则按列合并结果
  }
}

####9.5.bottom-sex####
#再跑一次，将所有变量换成早发性肿瘤队列
case <- data_zaofa[data_zaofa$zaofa=='zaofa',]
case$str_id <-1:nrow(case)
control <- data_zaofa[data_zaofa$zaofa=='none_cancer',]
matched_ctrl <- match_riskset_fast_summary(case, control, ratio = 5, seed = 423)
csid_id<- c(matched_ctrl$csid,case$csid)
zzdata <- data[data$csid %in% csid_id,]
case_str_id <- case[,c("csid","str_id")]
control_str_id <- matched_ctrl[,c("csid","str_id")]
data_str_id <- rbind(case_str_id ,control_str_id)
zzdata_str <- merge(zzdata,data_str_id)
zzdata_str$outcome <- as.numeric(zzdata$outcome)
ABSI_group<- quantile(zzdata_str$ABSI, probs=c(0,1/4,2/4,3/4,1))
zzdata_str$ABSI_group <- cut(zzdata_str$ABSI,
                             breaks = ABSI_group,
                             labels = c('Q1_ABSI', 'Q2_ABSI', 'Q3_ABSI','Q4_ABSI'))
table(zzdata_str$ABSI_group)
zzdata_str[which(zzdata_str$ABSI_group %in% NA),'ABSI_group'] <- 'Q1_ABSI'
table(zzdata_str$ABSI_group)

#CRC的男女分层结果
data_crc_id<- case[case$cancer_type =='colon cancer',]
data_crc <- zzdata_str[zzdata_str$str_id %in% data_crc_id$str_id,]
data_crc_Male <- data_crc[data_crc$gender=='Male',]
data_crc_Woman<- data_crc[data_crc$gender=='Female',]
bl <-c('ABSI_0.001','BMI_change_5unit')
result0 <- NULL
resultt <- c()
for (i in 1:length(bl)) {
  df <- bl[i]
  resultt <- logregression(dat = data_crc_Male,form = df)
  if (is.null(result0)) {
    result0 <- resultt  # 如果result0为空，则直接赋值
  } else {
    result0 <- rbind(result0, resultt)  # 否则按列合并结果
  }
}
result_crc_Male <- result0
result0 <- NULL
resultt <- c()
for (i in 1:length(bl)) {
  df <- bl[i]
  resultt <- logregression(dat = data_crc_Woman,form = df)
  if (is.null(result0)) {
    result0 <- resultt  # 如果result0为空，则直接赋值
  } else {
    result0 <- rbind(result0, resultt)  # 否则按列合并结果
  }
}
result_crc_Woman <- result0

#GC的男女分层结果
data_gc_id<- case[case$cancer_type =='gastric cancer',]
data_gc <- zzdata_str[zzdata_str$str_id %in% data_gc_id$str_id,]
data_gc_Male <- data_gc[data_gc$gender=='Male',]
data_gc_Woman<- data_gc[data_gc$gender=='Female',]
bl <-c('ABSI_0.001','BMI_change_5unit')
result0 <- NULL
resultt <- c()
for (i in 1:length(bl)) {
  df <- bl[i]
  resultt <- logregression(dat = data_gc_Male,form = df)
  if (is.null(result0)) {
    result0 <- resultt  # 如果result0为空，则直接赋值
  } else {
    result0 <- rbind(result0, resultt)  # 否则按列合并结果
  }
}
result_gc_Male <- result0
result0 <- NULL
resultt <- c()
for (i in 1:length(bl)) {
  df <- bl[i]
  resultt <- logregression(dat = data_gc_Woman,form = df)
  if (is.null(result0)) {
    result0 <- resultt  # 如果result0为空，则直接赋值
  } else {
    result0 <- rbind(result0, resultt)  # 否则按列合并结果
  }
}
result_gc_Woman <- result0

#####9.5.1 Fig.3 bottom#####
result_crc_Male <- result_crc_Male[,c(1,6:7)]
result_crc_Woman<- result_crc_Woman[,c(1,6:7)]
result_gc_Male <- result_gc_Male[,c(1,6:7)]
result_gc_Woman<- result_gc_Woman[,c(1,6:7)]
combined_crc_result <- bind_rows(
  result_crc_Male = result_crc_Male,
  result_crc_Woman = result_crc_Woman,
  .id = "subgroup"
) %>%
  relocate(subgroup, .after = group) %>%  # 把 subgroup 放在 group 后面
  arrange(group)

combined_gc_result <- bind_rows(
  result_gc_Male = result_gc_Male,
  result_gc_Woman = result_gc_Woman,
  .id = "subgroup"
) %>%
  relocate(subgroup, .after = group) %>%  # 把 subgroup 放在 group 后面
  arrange(group)

combined_result <- rbind(combined_crc_result,combined_gc_result)
print(combined_result)

combined_result <- combined_result %>% 
  add_row(group = "EOCRC", .before = 1) %>%
  add_row(group = "EOGC", .before = 6)

combined_result <- combined_result %>%
  mutate(across(everything(), ~replace(., is.na(.), "")))

names(combined_result) <- c("Cancer type & Metric","Subgroup","OR3","P(Model3)")
combined_result[c(2:3,7:8),1] <- "ABSI (per 0.001)"
combined_result[c(4:5,9:10),1] <- "BMI change (per 5kg/m²)"
combined_result[c(2,4,7,9),2] <- "Male"
combined_result[c(3,5,8,10),2] <- "Female"

dat <- combined_result %>% 
  mutate(
    OR   = str_extract(OR3, "^[\\d\\.]+") %>% as.numeric,
    low  = str_extract(OR3, "(?<=\\()[\\d\\.]+") %>% as.numeric,
    high = str_extract(OR3, "[\\d\\.]+(?=\\))") %>% as.numeric
  )
dat$" "<-paste(rep(" ",40),collapse="")
colnames(dat)
names(dat)=c("Cancer type & Metric","Subgroup","OR(95%CI)","P(Model3)","OR","low","high","")

p6 <- forest(dat[,c(1:2,8,3,4)],       
             est = dat$OR,       #效应值       
             lower = dat$low,     #可信区间下限       
             upper = dat$high,      #可信区间上限       
             ci_column =3,   #在那一列画森林图，要选空的那一列
             ref_line = 1,       
             arrow_lab =c("Decrease","Increase"),
             theme = tm, xlim = c(0.25, 4),    
             x_trans = "log" ,
             ticks_at = c(0.25, 0.5, 1, 2, 4))

pp6 <- add_border(p6, part = "header", where = "bottom",row = 0)
pp6 <- add_border(pp6, part = "header", where = "bottom")
pp6 <- edit_plot(pp6, row = c(1,6), gp = gpar(col = "red4", fontface ="bold.italic"))
pp6 <- edit_plot(pp6, row = c(2:5,7:10),col = 1, gp = gpar(col = "black", fontface = "italic"))
pp6
ggsave("Fig.3.bottom.pdf", pp6, width = 10, height = 10, device = "pdf")


#####9.5.2 Supplementary Figure#####
#补充图需要跑更多的变量条件logistic回归分析，因此需要重新跑遍gc和crc的男女分层结果
bl <-c('BMI25_5unit','BMI_5unit','BMI_change_5unit','ABSI_0.001','BRI','Hip_10unit','Wasit_10unit','BMI_change_ratio','weight_change_ratio','fat_percent_x10')
result0 <- NULL
resultt <- c()
for (i in 1:length(bl)) {
  df <- bl[i]
  resultt <- logregression(dat = data_crc_Male,form = df)
  if (is.null(result0)) {
    result0 <- resultt  # 如果result0为空，则直接赋值
  } else {
    result0 <- rbind(result0, resultt)  # 否则按列合并结果
  }
}
result_crc_Male <- result0
result0 <- NULL
resultt <- c()
for (i in 1:length(bl)) {
  df <- bl[i]
  resultt <- logregression(dat = data_crc_Woman,form = df)
  if (is.null(result0)) {
    result0 <- resultt  # 如果result0为空，则直接赋值
  } else {
    result0 <- rbind(result0, resultt)  # 否则按列合并结果
  }
}
result_crc_Woman <- result0

#GC的男女分层结果
bl <-c('ABSI_0.001','BMI_change_5unit')
result0 <- NULL
resultt <- c()
for (i in 1:length(bl)) {
  df <- bl[i]
  resultt <- logregression(dat = data_gc_Male,form = df)
  if (is.null(result0)) {
    result0 <- resultt  # 如果result0为空，则直接赋值
  } else {
    result0 <- rbind(result0, resultt)  # 否则按列合并结果
  }
}
result_gc_Male <- result0
result0 <- NULL
resultt <- c()
for (i in 1:length(bl)) {
  df <- bl[i]
  resultt <- logregression(dat = data_gc_Woman,form = df)
  if (is.null(result0)) {
    result0 <- resultt  # 如果result0为空，则直接赋值
  } else {
    result0 <- rbind(result0, resultt)  # 否则按列合并结果
  }
}
result_gc_Woman <- result0


dat <- result_crc_Male %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(cols = -c(group, id),
               names_to = c(".value", "model"),
               names_pattern = "(OR|P)(\\d)") %>% 
  # 1. 先把原始字符串留一份
  mutate(OR_raw = OR) %>% 
  # 2. 在字符串上提取
  mutate(
    OR   = str_extract(OR_raw, "^[\\d\\.]+") %>% as.numeric,
    low  = str_extract(OR_raw, "(?<=\\()[\\d\\.]+") %>% as.numeric,
    high = str_extract(OR_raw, "[\\d\\.]+(?=\\))") %>% as.numeric
  ) %>% 
  dplyr::select(group, model, OR, OR_raw,low, high, P)

dat <- dat %>%
  mutate(across(everything(), ~replace(., is.na(.), "")))
dat$OR <- as.numeric(dat$OR)
dat$high<- as.numeric(dat$high)
dat$low<- as.numeric(dat$low)
dat[c(1,3,4,6,7,9,10,12,13,15,16,18,19,21,22,24,25,27,28,30),1] <- ""
dat[2,1] <-'BMI25 (per 5kg/m²)'
dat[5,1] <-'Adult BMI (per 5kg/m²)'
dat[8,1] <-'BMI change (per 5kg/m²)'
dat[11,1] <-'ABSI (per 0.001)'
dat[17,1] <-'Hip (per 10cm)'
dat[20,1] <-'Waist (per 10cm)'
dat[23,1] <-'BMI change (Ratio)'
dat[26,1] <-'Weight change (Ratio)'
dat[29,1] <-'FAT (%)'
dat$" "<-paste(rep(" ",40),collapse="")
colnames(dat)
names(dat)=c('Characteristics','Model','OR','OR(95%CI)','low','high','P','')
p4 <- forest(dat[,c(1:2,8,4,7)],       
             est = dat$OR,       #效应值       
             lower = dat$low,     #可信区间下限       
             upper = dat$high,      #可信区间上限       
             ci_column =3,   #在那一列画森林图，要选空的那一列
             ref_line = 1,       
             arrow_lab =c("Decrease","Increase"),
             xlim      = c(0, 2.5),
             theme = tm)

pp4 <- add_border(p4, part = "header", where = "bottom",row = 0)
pp4 <- add_border(pp4, part = "header", where = "bottom")
pp4 <- edit_plot(pp4, row = c(2,5,8,11,14,17,20,23,26,29),col = 1, gp = gpar(col = "black", fontface = "italic"))
pp4
ggsave("forest_plot4.pdf", pp4, width = 10, height = 10, device = "pdf")

dat <- result_crc_Woman %>% 
  mutate(id = row_number()) %>% 
  pivot_longer(cols = -c(group, id),
               names_to = c(".value", "model"),
               names_pattern = "(OR|P)(\\d)") %>% 
  # 1. 先把原始字符串留一份
  mutate(OR_raw = OR) %>% 
  # 2. 在字符串上提取
  mutate(
    OR   = str_extract(OR_raw, "^[\\d\\.]+") %>% as.numeric,
    low  = str_extract(OR_raw, "(?<=\\()[\\d\\.]+") %>% as.numeric,
    high = str_extract(OR_raw, "[\\d\\.]+(?=\\))") %>% as.numeric
  ) %>% 
  dplyr::select(group, model, OR, OR_raw,low, high, P)

dat <- dat %>%
  mutate(across(everything(), ~replace(., is.na(.), "")))
dat$OR <- as.numeric(dat$OR)
dat$high<- as.numeric(dat$high)
dat$low<- as.numeric(dat$low)
dat[c(1,3,4,6,7,9,10,12,13,15,16,18,19,21,22,24,25,27,28,30),1] <- ""
dat[2,1] <-'BMI25 (per 5kg/m²)'
dat[5,1] <-'Adult BMI (per 5kg/m²)'
dat[8,1] <-'BMI change (per 5kg/m²)'
dat[11,1] <-'ABSI (per 0.001)'
dat[17,1] <-'Hip (per 10cm)'
dat[20,1] <-'Waist (per 10cm)'
dat[23,1] <-'BMI change (Ratio)'
dat[26,1] <-'Weight change (Ratio)'
dat[29,1] <-'FAT (%)'
dat$" "<-paste(rep(" ",40),collapse="")
colnames(dat)
names(dat)=c('Characteristics','Model','OR','OR(95%CI)','low','high','P','')
p5 <- forest(dat[,c(1:2,8,4,7)],       
             est = dat$OR,       #效应值       
             lower = dat$low,     #可信区间下限       
             upper = dat$high,      #可信区间上限       
             ci_column =3,   #在那一列画森林图，要选空的那一列
             ref_line = 1,       
             arrow_lab =c("Decrease","Increase"),
             xlim      = c(0, 3),
             theme = tm)
pp5 <- add_border(p5, part = "header", where = "bottom",row = 0)
pp5 <- add_border(pp5, part = "header", where = "bottom")
pp5 <- edit_plot(pp5, row = c(2,5,8,11,14,17,20,23,26,29),col = 1, gp = gpar(col = "black", fontface = "italic"))
pp5
ggsave("forest_plot5.pdf", pp5, width = 10, height = 10, device = "pdf")



####10.Sensitivity analysis-1####
case <- case %>%
  mutate(
    exit_date  = as.Date(exit_date),
    entry_date = as.Date(entry_date),
    suifang = as.numeric(exit_date - entry_date) / 365.25
  )
summary(case$suifang)
paichu <- case[case$suifang<2,]
table(paichu$cancer_type)
shengyu <- case[case$suifang>=2,]
table(shengyu$cancer_type)

#####10.1 EOGIC#####
data_mg <- zzdata_str[!(zzdata_str$str_id %in% paichu$str_id),]
ABSI_group<- quantile(data_mg$ABSI, probs=c(0,1/4,2/4,3/4,1))
data_mg$ABSI_group <- cut(data_mg$ABSI,
                             breaks = ABSI_group,
                             labels = c('Q1_ABSI', 'Q2_ABSI', 'Q3_ABSI','Q4_ABSI'))
data_mg[which(data_mg$ABSI_group %in% NA),'ABSI_group'] <- 'Q1_ABSI'
table(data_mg$ABSI_group)

bl <-c('ABSI_0.001','ABSI','ABSI_group','weight_change_ratio','BMI_change_ratio','BMI25','BMI25_5unit','bmi_calc','BMI_5unit','BMI_change','BMI_change_5unit','hip_mm','Hip_10unit','waist_mm','Wasit_10unit','waist_hip_ratio','fat_percent_x10','BRI')
result0 <- NULL
resultt <- c()
for (i in 1:length(bl)) {
  df <- bl[i]
  resultt <- logregression(dat = data_mg,form = df)
  if (is.null(result0)) {
    result0 <- resultt  # 如果result0为空，则直接赋值
  } else {
    result0 <- rbind(result0, resultt)  # 否则按列合并结果
  }
}

#####10.2 EOGC#####
data_gc_id<- case[case$cancer_type =='gastric cancer',]
data_gc <- data_mg[data_mg$str_id %in% data_gc_id$str_id,]
bl <-c('ABSI_0.001','ABSI','ABSI_group','weight_change_ratio','BMI_change_ratio','BMI25','BMI25_5unit','bmi_calc','BMI_5unit','BMI_change','BMI_change_5unit','hip_mm','Hip_10unit','waist_mm','Wasit_10unit','waist_hip_ratio','fat_percent_x10','BRI')
result0 <- NULL
resultt <- c()
for (i in 1:length(bl)) {
  df <- bl[i]
  resultt <- logregression(dat = data_gc,form = df)
  if (is.null(result0)) {
    result0 <- resultt  # 如果result0为空，则直接赋值
  } else {
    result0 <- rbind(result0, resultt)  # 否则按列合并结果
  }
}

#####10.3 EOCRC#####
data_crc_id<- case[case$cancer_type =='colon cancer',]
data_crc <- data_mg[data_mg$str_id %in% data_crc_id$str_id,]
bl <-c('ABSI_0.001','ABSI','ABSI_group','weight_change_ratio','BMI_change_ratio','BMI25','BMI25_5unit','bmi_calc','BMI_5unit','BMI_change','BMI_change_5unit','hip_mm','Hip_10unit','waist_mm','Wasit_10unit','waist_hip_ratio','fat_percent_x10','BRI')
result0 <- NULL
resultt <- c()
for (i in 1:length(bl)) {
  df <- bl[i]
  resultt <- logregression(dat = data_crc,form = df)
  if (is.null(result0)) {
    result0 <- resultt  # 如果result0为空，则直接赋值
  } else {
    result0 <- rbind(result0, resultt)  # 否则按列合并结果
  }
}

####11.Sensitivity analysis-2####
resultt <- c()
result0 <- NULL
result1 <- c()
result2 <- NULL
result3 <- c()
result4 <- NULL

bl <-c('BMI25_5unit','BMI_5unit','BMI_change_5unit','ABSI_0.001','BRI','Hip_10unit','Wasit_10unit','BMI_change_ratio','weight_change_ratio','fat_percent_x10')
system.time({
  for (i in 1:10000) {
    matched_ctrl <- match_riskset_fast_summary(case, control, ratio = 5, seed = i)
    csid_id<- c(matched_ctrl$csid,case$csid)
    zzdata <- data[data$csid %in% csid_id,]
    case_str_id <- case[,c("csid","str_id")]
    control_str_id <- matched_ctrl[,c("csid","str_id")]
    data_str_id <- rbind(case_str_id ,control_str_id)
    zzdata_str <- merge(zzdata,data_str_id)
    zzdata_str$outcome <- as.numeric(zzdata_str$outcome )
      for (a in 1:length(bl)) {
      df <- bl[a]
      resultt <- logregression(dat = zzdata_str,form = df)
      if (is.null(result0)) {
        result0 <- resultt  # 如果result0为空，则直接赋值
      } else {
        result0 <- rbind(result0, resultt)  # 否则按列合并结果
      }
      }
    
    data_crc_id<- case[case$cancer_type =='colon cancer',]
    data_crc <- zzdata_str[zzdata_str$str_id %in% data_crc_id$str_id,]
    for (a in 1:length(bl)) {
      df <- bl[a]
      result1 <- logregression(dat = data_crc,form = df)
      if (is.null(result2)) {
        result2 <- result1  # 如果result0为空，则直接赋值
      } else {
        result2 <- rbind(result2, result1)  # 否则按列合并结果
      }
    }
    
    data_gc_id<- case[case$cancer_type =='gastric cancer',]
    data_gc <- zzdata_str[zzdata_str$str_id %in% data_gc_id$str_id,]
    for (a in 1:length(bl)) {
      df <- bl[a]
      result3 <- logregression(dat = data_gc,form = df)
      if (is.null(result4)) {
        result4 <- result3  # 如果result0为空，则直接赋值
      } else {
        result4 <- rbind(result4, result3)  # 否则按列合并结果
      }
    }
    print(i)
    
  }
})

write.table(result0,"10000_EOGIC_zz.txt",sep = '\t',row.names = F)
write.table(result2,"10000_EOCRC_zz.txt",sep = '\t',row.names = F)
write.table(result4,"10000_EOGC_zz.txt",sep = '\t',row.names = F)
save(result0,result2,result4,file = '10000_zz.Rdata')

#####11.1 Fig-EOGIC#####
group_levels <- unique(result0$group)   # 分类变量及其出现顺序
n_per_cycle  <- length(group_levels)    # 每轮多少个
n_cycles     <- nrow(result0) / n_per_cycle  # 总轮数

result0 <- result0 %>%                  # 原位加列
  mutate(cycle_id = rep(1:n_cycles, each = n_per_cycle))

plot_data <- result0 %>%
  dplyr::select(cycle_id, group, P1, P2, P3) %>%
  pivot_longer(cols = c(P1, P2, P3), names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from = group, values_from = Value)

plot_data_long <- plot_data %>%
  dplyr::select(cycle_id, Metric, all_of(group_levels)) %>%
  pivot_longer(cols = all_of(group_levels), names_to = "Group", values_to = "Value")

plot_data_long$Value <- -log10(plot_data_long$Value+1E-5)
unique(plot_data_long$Group)

p1 <- ggplot(plot_data_long, aes(x = factor(Group, levels = group_levels), y = factor(cycle_id), fill = Value)) +
  geom_tile(width = 1, height = 10) +
  scale_fill_gradient2(low = "#1084a6", mid = "white", high = "#ff8453", midpoint =-log10(0.05+1E-5)) +
  facet_wrap(~Metric, ncol = 3) +
  labs(x = "Group", y = "Cycle Index", fill = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 10),
        strip.background = element_rect(fill = "grey95", colour = "grey95"),  # 背景色 + 边框
        strip.text       = element_text(colour = "black", size = 12),
        panel.border = element_rect(colour = "#3f8998", fill = NA, linetype = "dashed", size = 0.8)) +
  scale_y_discrete(breaks = factor(c(0,1,2000, 4000, 6000, 8000, 10000)), 
                   labels = c("0", "0", "2000", "4000", "6000", "8000","10000"))
ggsave("myplot_cancer_zz.pdf", plot = p1, width = 12, height = 6, units = "in")

#####11.2 Fig-EOGC#####
group_levels <- unique(result2$group)  
n_per_cycle  <- length(group_levels)
n_cycles     <- nrow(result2) / n_per_cycle
result2 <- result2 %>%                  # 原位加列
  mutate(cycle_id = rep(1:n_cycles, each = n_per_cycle))
plot_data <- result2 %>%
  dplyr::select(cycle_id, group, P1, P2, P3) %>%
  pivot_longer(cols = c(P1, P2, P3), names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from = group, values_from = Value)
plot_data_long <- plot_data %>%
  dplyr::select(cycle_id, Metric, all_of(group_levels)) %>%
  pivot_longer(cols = all_of(group_levels), names_to = "Group", values_to = "Value")
plot_data_long$Value <- -log10(plot_data_long$Value+1E-5)
unique(plot_data_long$Group)
p2 <- ggplot(plot_data_long, aes(x = factor(Group, levels = group_levels), y = factor(cycle_id), fill = Value)) +
  geom_tile(width = 1, height = 10) +
  scale_fill_gradient2(low = "#1084a6", mid = "white", high = "#ff8453", midpoint =-log10(0.05+1E-5)) +
  facet_wrap(~Metric, ncol = 3) +
  labs(x = "Group", y = "Cycle Index", fill = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 10),
        strip.background = element_rect(fill = "grey95", colour = "grey95"),  # 背景色 + 边框
        strip.text       = element_text(colour = "black", size = 12),
        panel.border = element_rect(colour = "#3f8998", fill = NA, linetype = "dashed", size = 0.8)) +
  scale_y_discrete(breaks = factor(c(0,1,2000, 4000, 6000, 8000, 10000)), 
                   labels = c("0", "0", "2000", "4000", "6000", "8000","10000"))
ggsave("myplot_crc_zz.pdf", plot = p2, width = 12, height = 6, units = "in")

#####11.3 Fig-EOCRC#####
group_levels <- unique(result4$group)   # 分类变量及其出现顺序
n_per_cycle  <- length(group_levels)    # 每轮多少个
n_cycles     <- nrow(result4) / n_per_cycle  # 总轮数
result4 <- result4 %>%                  # 原位加列
  mutate(cycle_id = rep(1:n_cycles, each = n_per_cycle))
plot_data <- result4 %>%
  dplyr::select(cycle_id, group, P1, P2, P3) %>%
  pivot_longer(cols = c(P1, P2, P3), names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from = group, values_from = Value)
plot_data_long <- plot_data %>%
  dplyr::select(cycle_id, Metric, all_of(group_levels)) %>%
  pivot_longer(cols = all_of(group_levels), names_to = "Group", values_to = "Value")
plot_data_long$Value <- -log10(plot_data_long$Value+1E-5)
unique(plot_data_long$Group)
p3 <- ggplot(plot_data_long, aes(x = factor(Group, levels = group_levels), y = factor(cycle_id), fill = Value)) +
  geom_tile(width = 1, height = 10) +
  scale_fill_gradient2(low = "#1084a6", mid = "white", high = "#ff8453", midpoint =-log10(0.05+1E-5)) +
  facet_wrap(~Metric, ncol = 3) +
  labs(x = "Group", y = "Cycle Index", fill = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,size = 10),
        strip.background = element_rect(fill = "grey95", colour = "grey95"),  # 背景色 + 边框
        strip.text       = element_text(colour = "black", size = 12),
        panel.border = element_rect(colour = "#3f8998", fill = NA, linetype = "dashed", size = 0.8)) +
  scale_y_discrete(breaks = factor(c(0,1,2000, 4000, 6000, 8000, 10000)), 
                   labels = c("0", "0", "2000", "4000", "6000", "8000","10000"))
ggsave("myplot_gc_zz.pdf", plot = p3, width = 12, height = 6, units = "in")

