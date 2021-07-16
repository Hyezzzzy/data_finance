#패키지 설치 및 라이브러리 추가
install.packages("dplyr")
library(dplyr)
install.packages("readxl")
library(readxl)

#데이터 불러오기
s <- readxl::read_excel(path="C:/data_fi/금융대회_신한은행.xlsx",
                        sheet = "finance_data",
                        col_names=TRUE)
summary(s)

#중복값확인
duplicates <- s %>% duplicated() %>% table()
duplicates

#결측값확인
table(is.na(s$가맹점매출입금))
table(is.na(s$지역구))
table(is.na(s$기준년월))
table(is.na(s$신용대출금액))
table(is.na(s$담보대출금액))
table(is.na(s$주택대출금액))

#필요한 데이터 추출
card <-s %>% filter(가맹점매출입금!=0) %>% select(기준년월, 지역구, 가맹점매출입금, 신용대출금액, 담보대출금액, 주택대출금액)

#대출총합변수 추가 및 미리보기
card<-card %>%
  mutate(대출총합 = 신용대출금액+담보대출금액+주택대출금액) %>%
  head
summary(card) ###오류가 계속 생기는 것 같은,,,,,,,

#데이터 요약 및 이상치 제거 전 상자그림
summary(card$대출총합)
summary(card$가맹점매출입금)
boxplot(card$대출총합)
boxplot(card$가맹점매출입금)

#결측값개수 확인
which(card$대출총합>boxplot(card$대출총합)$stats[5,1] + 1.5*IQR(card$대출총합)) ##없음
which(card$가맹점매출입금>boxplot(card$가맹점매출입금)$stats[5,1] + 1.5*IQR(card$가맹점매출입금)) ##없음
which(card$대출총합<boxplot(card$대출총합)$stats[1,1] - 1.5*IQR(card$대출총합)) ##없음
which(card$가맹점매출입금<boxplot(card$가맹점매출입금)$stats[1,1] - 1.5*IQR(card$가맹점매출입금))

#결측값제거 및 상자그림
card <-card[-which(card$가맹점매출입금<boxplot(card$가맹점매출입금)$stats[1,1] - 1.5*IQR(card$가맹점매출입금)),]
boxplot(card$가맹점매출입금)
boxplot(card$가맹점매출입금)$stats

#지역구, 기준년월로 묶기
card %>%
  group_by(지역구, 기준년월) %>%
  summarize(mean_대출 = mean(대출총합))

card %>%
  group_by(지역구, 기준년월) %>%
  summarize(min_대출 = min(대출총합))

card %>%
  group_by(지역구, 기준년월) %>%
  summarize(max_대출 = max(대출총합))

card %>%
  group_by(지역구, 기준년월) %>%
  summarize(med_대출 = median(대출총합))