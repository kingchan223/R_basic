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

# 우리가 이전에 만든 age 파생변수를 사용한다.
summary(welfare$age)
# age 그룹(young, middle, old)을 만든다.
welfare <- welfare %>%
  mutate(ageg = ifelse(age < 30, "young", ifelse(age < 59, "middle", "old")))
# ageg의 빈도 테이블로 만들어보자.
qplot(welfare$ageg)

# income에 대한 전처리도 이미 이전에 한 바가 있다.
class(welfare$income)
summary(welfare$income)

ageg_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg) %>%
  summarise(mean_income = mean(income))
ageg_income

ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) + geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old")) # "young", "middle", "old"으로 바로잡기
# 또는 아래와 같이 income을 기준으로 순서를 잡을 수도 있다.
ggplot(data = ageg_income, aes(x = reorder(ageg, mean_income), y = mean_income)) + geom_col()

#### 4. 연령대 및 성별 월급 차이 ####

sex_income = welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg, sex) %>%
  summarise(mean_income = mean(income))
sex_income

ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +geom_col() +
  scale_x_discrete(limits = c("young", "middle", "old"))

#근데 위의 표는 헷갈리니까 position = "dodge"를 추가해준다.
ggplot(data = sex_income, aes(x = ageg, y = mean_income, fill = sex)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = c("young", "middle", "old"))
# 근제 조금 더 세밀하게 연령과 성별에 따른 차이를 보자.(위에서는 연령그룹과 성별했음)
sex_age <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(age, sex) %>%
  summarise(mean_income = mean(income))
sex_age
# 여기서는 geom_line을 사용하는 것이 합리적임
ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) + geom_line()



#### 5. 직업별 월급 차이 ####
class(welfare$code_job)
table(welfare$code_job)
# codebook의 직업 코드 데이터 읽어오기
library(readxl)
list_job = read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
list_job
# welfare의 job과 join
welfare <- left_join(welfare, list_job, id = "code_job")

welfare %>%
  filter(!is.na(code_job)) %>%
  select(code_job, job) %>%
  head(6)

job_income <- welfare %>%
  filter(!is.na(code_job) & !is.na(job)) %>%
  group_by(job) %>%
  summarise(mean_income = mean(income))
View(job_income)

# 월급을 내림차순 정렬, 상위 10개 추출
top10 <- job_income %>%
  arrange(desc(mean_income)) %>%
  head(10)
top10

ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income)) + geom_col() +
  coord_flip()# 얘를 추가하면 글씨가 쭉 보이게 뒤집힌다.

# 월급을 내림차순 정렬, 하위 10개 추출
bottom10 <- job_income %>%
  arrange(mean_income) %>%
  head(10)
bottom10

ggplot(data = bottom10, aes(x = reorder(job, -mean_income), y = mean_income)) + geom_col() +
  coord_flip() + ylim(0, 850)


#### 6. 성별 직업 빈도 ####
job_male <- welfare %>%
  filter(!is.na(job) & sex == "male") %>%
  group_by(job) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)
job_male

job_female <- welfare %>%
  filter(!is.na(job) & sex == "female") %>%
  group_by(job) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  head(10)
job_female

ggplot(data = job_male, aes(x = reorder(job, n), y = n)) + geom_col() + coord_flip()

#### 7. 종교 유무에 따른 이혼율 ####
class(welfare$religion)
class(welfare$marriage)
table(welfare$marriage)
welfare$religion <- ifelse(welfare$religion == 1, "YES", "NO")
qplot(welfare$religion)

table(welfare$marriage)
welfare$group_marraige <- ifelse(welfare$marriage == 1, "marriage",
                                 ifelse(welfare$marriage == 3, "divorce", NA))
table(welfare$group_marraige)
table(is.na(welfare$group_marraige))
qplot(welfare$group_marraige)

#이제 종교 유무에 따른 이혼율을 구해보자.
religion_marriage <- welfare %>%
  filter(!is.na(group_marraige)) %>%
  group_by(religion, group_marraige) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100, 1))
religion_marriage

divorce <- religion_marriage %>%
  filter(group_marriage == "divorce") %>%
  select(religion, pct)

## 연령대별 이혼률
ageg_marriage <- welfare %>%
  filter(!is.na(group_marraige)) %>%
  group_by(ageg, group_marraige) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100, 1))
ageg_marriage

ageg_divorce <- ageg_marriage %>%
  filter(ageg != "young" & group_marraige == "divorce") %>%
  select(ageg, pct)
ageg_divorce
ggplot(data = ageg_divorce, aes(x=ageg, y = pct)) + geom_col()

## 연령대 및 종교에 따른 이혼률
ageg_religion_magrriage <- welfare %>%
  filter(!is.na(group_marraige) & ageg != "young") %>%
  group_by(ageg, religion, group_marraige) %>%
  summarize(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100, 1))
ageg_religion_magrriage

df_divorce <- ageg_religion_magrriage %>%
  filter(group_marraige == "divorce") %>%
  select(ageg, religion, pct)
df_divorce

ggplot(data = df_divorce, aes(x = ageg, y = pct, fill = religion)) + geom_col(position = "dodge")
#### 8. 지역별 연령대 비율 ####

class(welfare$code_region)
table(welfare$code_region)
list_rigion <- data.frame(code_region = c(1:7), region = c("서울", 
                                                           "수도권(인천/경기)", 
                                                           "부산/경남/울산",
                                                           "대구/경북",
                                                           "대전/충남",
                                                           "강원/충북",
                                                           "광주/전남/전북/제주도"))

welfare <- left_join(welfare, list_rigion, id = "code_region")

data <- welfare %>%
  filter(!is.na(region) & !is.na(age))
group_by(region, age) %>%
  summarise(n = n())

welfare %>%
  select(code_region, region) %>%
  head

region_ageg <- welfare %>%
  group_by(region, ageg) %>%
  summarise(n = n()) %>%
  mutate(tot_group = sum(n)) %>%
  mutate(pct = round(n/tot_group*100, 2))
region_ageg
ggplot(data = region_ageg, aes(x=region, y = pct, fill = ageg)) + geom_col() + coord_flip()
# 위의 표를 노년이 많은 순으로 정렬
list_order_old <- region_ageg %>%
  filter(ageg == "old") %>%
  arrange(pct)
list_order_old

order <- list_order_old$region

ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) + geom_col() + coord_flip() +
  scale_x_discrete(limits = order)

region_ageg$ageg <- factor(region_ageg$ageg, level = c("old", "middle", "young"))
class(region_ageg$ageg)
levels(region_ageg$ageg)
ggplot(data = region_ageg, aes(x = region, y = pct, fill = ageg)) + geom_col() +
  coord_flip() +scale_x_discrete(limits=order)
