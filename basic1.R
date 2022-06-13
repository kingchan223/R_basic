# 변수 만들기는 '<-'를 사용한다.
a <- 1

#여러 값을 지닌 벡터 변수를 만들기 위해서는 c() , seq()를 사용한다.
vector1 <- c(1, 2, 3, 4, 5) # [1, 2, 3, 4, 5]
vector2 <- seq(1, 5)        # [1, 2, 3, 4, 5]
vector3 <- seq(1,10,by=2)   # [1, 3, 5, 7, 9]

#변수는 연속변수(continuous variable)와 범주변수(categorical variable)로 나뉜다.
#연속변수(continuous variable):
#연속적이며 크기를 가짐.(ex. 키, 몸무게, 매출) 양적 변수

# 범주변수(categorical variable):
# 대상을 분류(ex. 성별:남(1)/여(2) )
# 설령 숫자의 형태이더라도 크기를 의미하지 않으므로 산술연산이 불가능
# 또 범주변수는 명목변수와 순서 변수가 있다.
# 그 중 명목변수(Nominal variable)는 R에서 factor로 나타낸다.

x1 <- c(1, 2, 3, 3, 4)         # numeric 변수
x2 <- factor(c(1, 2, 3, 2, 2)) # factor 변수
x3 <- factor(c("A", "B", "C", "A", "B")) # factor 변수는 문자도 사용가능
#변수의 타입확인은 class()함수 사용
class(x1) #numeric
class(x2) #factor

#factor변수의 구성 범주를 확인할 수 있는 levels()함수
levels(x2) # [1] "1" "2" "3"
levels(x3) # [1] "A" "B" "C"

# 변수의 타입변환이 필요할 경우 as.numeric, as.factor, as.character, as.Date, as.data.frame  사용가능
x2 <- as.numeric(x2) # x2 = [1, 2, 3, 2, 2]
x2 <- mean(x2)       # factor변수를 numeric으로 바꿔서 mean함수 사용가능
x2 <- class(x2)      # "numeric"

# 변수 타입에는 아래와 같은 타입들이 있다.
# numeric   실수
# integer   정수
# complex   복소수
# character 문자
# Logical   논리 TRUE, FALSE, T, F
# factor    범주
# Date      날짜

## Data Frame ##
# 데이터 프레임은 엑셀처럼 숫자, 문자 등 다양한 데이터를 하나의 테이블에 담을 수 있는 자료구조

example.df <- data.frame(  
  컬럼1=c('a', 'b', 'c', 'd', 'e' ),
  컬럼2=c( 2,   4,    5,   7,   8 )
)
example.df
#   컬럼1  컬럼2
# 1     a     2
# 2     b     4
# 3     c     5
# 4     d     7
# 5     e     8

#아래는 dataframe을 살펴볼 수 있는 다양한 함수
View(example.df) #표로 보기
str(example.df) # data.frame의 structure
head(example.df) # 위에서 10개 데이터 보기
tail(example.df) # 아래에서 10개 데이터 보기
summary(example.df) #간략히 살펴보기
dim(example.df) #열, 행 개수 알아보기


#data.frame의 column과 row에 접근
example.df$컬럼1 # 컬럼1의 모든 데이터에 접근하기 ('칼럼1'의 모든 칼럼)
example.df[1,]   # 첫번째 row의 모든 칼럼 (1번째 row전부)
example.df[,2]   # 두번째 컬럼의 모든 row
example.df[1,2]  # 첫번째 로우의 두번째 칼럼