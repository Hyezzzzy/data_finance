install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("UsingR")
library(UsingR)
install.packages("readxl")
library(readxl)


shin <- readxl::read_excel(path="C:/data_fi/금융대회_신한은행.xlsx",
                        sheet = "finance_data",
                        col_names=TRUE)

dim(shin)
dim(shin[complete.cases(shin),])
head(shin)


shin <- shin %>%
  mutate(대출총합 = 신용대출금액+담보대출금액)

shin <- shin %>% filter(가맹점매출입금!=0&대출총합!=0) %>% dplyr::select(기준년월, 지역구, 가맹점매출입금, 대출총합)
head(shin)
summary(shin)

card <- aggregate(대출총합~기준년월+가맹점매출입금, shin, mean)


![image](https://user-images.githubusercontent.com/87364080/126675888-8a610ab1-0778-4c17-b37d-4cab15050dd2.png)



card_201903 <- card %>% filter(기준년월==201903)

![image](https://user-images.githubusercontent.com/87364080/126675969-0ece06b8-772e-4bc3-acf7-580cfd16b197.png)

card_201909 <- card %>% filter(기준년월==201909)
card_202003 <- card %>% filter(기준년월==202003)
card_202009 <- card %>% filter(기준년월==202009)
card_202103 <- card %>% filter(기준년월==202103)

