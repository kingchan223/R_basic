install.packages("foreign")
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
#sex = h10_g3
#birth = h10_g4
#marriage = h10_g10
#religion = h10_g11
#income = p1002_8aq1(월급)
#code_job = h10_eco9(직업코드)
#code_region = h10_reg7(지역코드)
raw_welfare <- read.spss(file = "Koweps_hpc10_2015_beta1.sav", to.data.frame = T )
welfare <- raw_welfare # init용
View(welfare)

#우리가 다룰 변수들의 이름 바꾸기
welfare <- rename(welfare, sex = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  code_job = h10_eco9,
                  code_region = h10_reg7
)
# 데이터 분석 절차
# 1. 변수 검토 및 전처리
#  - 먼저, 분석에 사용할 변수를 전처리. 변수의 특성을 파악하고 이상치를 정제 후 파생변수 만듬
#  - 전처리는 <분석에 활용할 변수> 각각에 대해 실시 (e.g., 성별에 따른 월급 차이)
# 2. 변수 간 관계 분석
#  - 전처리 완료 후 본격적으로 변수 간 관계를 파악하는 분석을 함.
#  - 데이터를 요약한 표를 만든 후 분석 결과를 쉽게 이해하게 하는 그래프를 그리는 등의 작업

# 데이터 분석 예
# - 성별에 따른 월급 차이
# - 나이와 월급의 관계
# - 연령대에 따른 월급 차이 
# - 연령대 및 성별 월급 차이 
# - 직업별 월급 차이
# - 성별 직업 빈도
# - 종교 유무에 따른 이혼율 
# - 지역별 연령대 비율

###### 1.성별에 따른 월급차이 ######
class(welfare$sex)
class(welfare$income)
class(welfare$sex)
table(welfare$sex) #빈도 테이블 이것의 결과는 모두 1 또는 2였음 --> 이상치는 없다.

# 1.1 Sex. 먼저 성별에 대한 전처리를 한다. (이상치 확인, chart로 보기)
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex) # 이상치는 NA로 바꾼다.
table(is.na(welfare$sex))#요 결과로 이상치가 없는 것을 확인하였다.

welfare$sex <- ifelse(welfare$sex == 1, "male", "female")
table(welfare$sex)
#이제 barchart를 그려보자.
qplot(welfare$sex)#female이 male보다 많다는 것을 알 수 있다.
ggplot(data = welfare$sex, aes(x=sex)) + geom_bar() # !!! geom_bar()는 빈도를 나타낼 때

# 1.2 income. 이제 수입에 대한 전처리를 한다.
class(welfare$income)
summary(welfare$income)
qplot(welfare$income) + xlim(0, 1500) # x축이 income, y축이 사람 수
# ---> 근데 summry의 min이 0.0인 이상치가 발견된다. (xcel 파일을 보면 값이 1~9998사이임)
# 해서 이것을 처리해줘야 한다. (결측치화하자.)
welfare$income <- ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)
summary(welfare$income)
# 1.3
# 이제 성별, 수입을 모두 전처리하였으므로 성별에 따른 임금차이를 평균으로 알아보자. 
sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(sex) %>%
  summarise(avg_income = mean(income))
sex_income
ggplot(data = sex_income, aes(x = sex, y = avg_income)) + geom_col() # !!! geom_col()는 계산한 결과를 나타낼 때 사용



#### 2. 나이와 월급의 관계 ####

# 2.1 나이에 대한 전처리
class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)
table(is.na(welfare$birth)) # 결츨치가 있는지 확인

welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth) # 혹시 모를 이상치 처리
welfare$age <- 2015 - welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)

age_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income = mean(income))
age_income
ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()

#### 3. 연령대에 따른 월급 차이 ####

#### 4. 연령대 및 성별 월급 차이 ####
#### 5. 직업별 월급 차이 ####
#### 6. 성별 직업 빈도 ####
#### 7. 종교 유무에 따른 이혼율 ####
#### 8. 지역별 연령대 비율####
##