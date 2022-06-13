#basic1에서 학습한 것들을 사용해서 ggplt2의 mpg 데이터 다뤄보기

# ggplot2의 mpg 데이터의 drv변수는 자동차의 구동방식 (f, r, 4)을 나타냄. 
# 이 데이터를 이용하여 다음을 답해 보시오.
install.packages("ggplot2")
library(ggplot2)
# 1. drv의 변수타입은?
mpg <- as.data.frame(ggplot2::mpg)
View(mpg)
drv <- mpg$drv
class(drv)
# 2. drv변수를 적절한 함수를 이용해 factor타입으로 변환 후 다시 타입을 확인하시오.
drv <- as.factor(drv)
class(drv)
# 3. drv가 어떤 범주로 구성되었는지 알아보시오.
levels(drv)