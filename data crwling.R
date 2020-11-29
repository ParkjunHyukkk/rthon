#install.packages("stringr")
#install.packages("dplyr")
#install.packages('tidyverse')
#install.packages("")
#install.packages("shiny")


library(tqk)
library(tidyverse) # 8개의 package가 있음
library(httr)
library(rvest)
library(jsonlite)
library(stringr)
library(dplyr)




results <- data.frame() #매출액 저장하기 위한 데이터 프레임
results_2 <- data.frame() # 부채비율 저장하기 위한 데이터 프레임

i<-1
codes<-code_get() #tqk에서 이용한 codes 저장
n<-nrow(x=codes)


for(i in 1:n){ #for 문을 이용해 codes을 이용해 수집된 기업 코드 만큼 반복

  cat(str_glue('현재 {i}번째 회사 정보 수집중 !'),'\n') # cat을 이용해 몇번 째 회사 탐색 인지 확인
  res<-GET(url='https://finance.naver.com/item/main.nhn',
           query = list('code'=codes$code[i]))
  # res에 GET함수를 통해 네이버 증권 url에  쿼리를 날려 정보 요청 후 저장
  tryCatch(expr={ # tryCatch를 통해 오류처리
    tbl <- res %>%
      read_html(encoding = 'EUC-KR')%>% #window윈도우 인코딩 방식오류 예방위해 인코딩 방식 지정
      html_node(css ='#content > div.section.cop_analysis > div.sub_section >
            table')%>% # chome->개발자도구->network->영역을 찍어 볼 수 있는 기능 이용해 해당 css를 가져옴.
      html_table(fill = TRUE)  # html_table만 사용시 fill = TRUE를 입력하라는 오류 발생, 따라서 fill = True를 사용


    colnames(x=tbl)[1:1] <- str_c('total') #toteal 부분 컬럼
    colnames(x=tbl)[2:5] <- str_c('실적',2017:2020) # 해당 부분에 '실적'을 넣은 이유는 데이터 시각화를 하는 과정에서 발생하는 col명에 대한 문제 때문에 사용


    df2<-
      tbl %>%
      select(1:5)%>% #슬라이싱하여
      slice(c(3))%>% #3번 째 항목인 매출 총이익 부분 슬라이싱
      mutate(회사 = codes$name[i])

    results <-rbind(results,df2) # rbind로 저장

    Sys.sleep(time = 0.5L) # # sleep을 통해 데이터 요청에 대한 시간을 두었다. 수집이제한 될 수 있기 때문이다.
  }, error = function(e) cat('---> 에러 발생!\n'))  # 에러 발생시 에러 발생이라는 문구
}

results$실적2017 <- str_remove(string = results$실적2017,pattern = ',')# , 지우기
results$실적2018 <- str_remove(string = results$실적2018,pattern = ',')# , 지우기
results$실적2019 <- str_remove(string = results$실적2019,pattern = ',')# , 지우기
results$실적2020 <- str_remove(string = results$실적2020,pattern = ',')# , 지우기


results$실적2017 <- as.integer(x = results$실적2017)# 정수로 바꾸기
results$실적2018 <- as.integer(x = results$실적2018)# 정수로 바꾸기
results$실적2019 <- as.integer(x = results$실적2019)# 정수로 바꾸기
results$실적2020 <- as.integer(x = results$실적2020)# 정수로 바꾸기

#dplyr
results <- results %>% select(-total) # 쉬프트+컨트롤+m = 파이프라인 단축키

#
long <-  results %>% # wide로 되어있는 데이터 구조를
  gather(key = year, value = sales, -회사) %>% #gather를 사용하여 long데이터로 변환

  arrange(회사) # 순서별로 정렬


#rm(wide) # wide 데이터 파일 지우기



#slice(9) 부채비율 찾기
i<-1
for(i in 1:n){

  cat(str_glue('현재 {i}번째 회사 정보 수집중 !'),'\n')
  res<-GET(url='https://finance.naver.com/item/main.nhn',
           query = list('code'=codes$code[i]))

  tryCatch(expr={
    tbl <- res %>%
      read_html(encoding = 'EUC-KR')%>%
      html_node(css ='#content > div.section.cop_analysis > div.sub_section >
            table')%>%
      html_table(fill = TRUE)

    colnames(x=tbl)[1:1] <- str_c('total')
    colnames(x=tbl)[2:5] <- str_c('실적',2017:2020)

    df<-
      tbl %>%
      select(1:5)%>%
      slice(c(9))%>% #부채 비율 debt
      mutate(회사 = codes$name[i])

    results_2 <-rbind(results_2,df)

    Sys.sleep(time = 0.5L)
  }, error = function(e) cat('---> 에러 발생!\n'))
}


results_2$실적2017 <- str_remove(string = results_2$실적2017,pattern = ',')# , 지우기
results_2$실적2018 <- str_remove(string = results_2$실적2018,pattern = ',')# , 지우기
results_2$실적2019 <- str_remove(string = results_2$실적2019,pattern = ',')# , 지우기
results_2$실적2020 <- str_remove(string = results_2$실적2020,pattern = ',')# , 지우기


results_2$실적2017 <- as.integer(x = results_2$실적2017)# 정수로 바꾸기
results_2$실적2018 <- as.integer(x = results_2$실적2018)# 정수로 바꾸기
results_2$실적2019 <- as.integer(x = results_2$실적2019)# 정수로 바꾸기
results_2$실적2020 <- as.integer(x = results_2$실적2020)# 정수로 바꾸기

#dplyr
results_2 <- results_2 %>% select(-total) # 쉬프트+컨트롤+m = 파이프라인 단축키

#
long_2 <-  results_2 %>%
  gather(key = year, value = debt, -회사) %>%
  arrange(회사)


# 데이터 시각화 표현 과정

long %>%
  filter(회사 == '고바이오랩') %>%
  ggplot(aes(x = year, y = sales , fill = year)) +
  geom_col() +
  ggtitle("[매출액]")+
  theme_bw()

long_2 %>%
  filter(회사 == '고바이오랩') %>%
  ggplot(aes(x = year, y = debt , fill = year)) +
  geom_col() +
  ggtitle("[부채]")+
  theme_bw()

saveRDS(object = results, file = 'results_1128_1.RDS')#데이터 파일 저장하기
saveRDS(object = results_2, file = 'results_1128_2.RDS')#데이터 파일 저장하기
saveRDS(object = long, file = 'long_1128_1.RDS')#데이터 파일 저장하기
saveRDS(object = long_2, file = 'long_1128_2.RDS')#데이터 파일 저장하기
