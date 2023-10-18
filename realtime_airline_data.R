# 실시간 운항 정보 스크래핑
# easy course
library(RSelenium)
library(rvest)
library(tidyverse)
library(httpuv)

#library(googledrive)

#shell('docker ps')

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  # 호스트의 IP 주소 또는 호스트 이름
  port = 4446,
  # 호스트와 매핑된 포트
  browserName = "chrome"           # 사용할 브라우저 (여기서는 Chrome)
)

remDr$close()
remDr$open()
remDr$closeServer()
############################### Departure 예시
sample_url <-
  'https://www.airportal.go.kr/life/airinfo/RbHanList.jsp?gubun=c_getList&depArr=D&current_date=20230921&airport=RKSI&fp_id='
sample_url <- 'https://www.airportal.go.kr/life/airinfo/RbHanList.jsp?gubun=c_getList&depArr=D&current_date=20071109&al_icao=&fp_id='

remDr$navigate(sample_url)

page_source <- remDr$getPageSource()[[1]]
page <- read_html(page_source)

#뽑고자 하는 데이터의 xpath 관찰
road <- '/html/body/form/table/tbody/tr/td/table/tbody/tr[1]'

tr <- page %>% html_node('body') %>%
  html_node('form') %>%
  html_node('table') %>%
  html_node('tbody') %>%
  html_node('td') %>%
  html_node('tbody')

tr %>% html_node('td') %>% html_text()

td <- tr %>% html_nodes('td') %>% html_text(trim = T)
td <- td[-length(td)]
td <- td[c(T, F)]


real_time_df <- matrix(td, ncol = 8, byrow = T) %>%
  data.frame()
df_col <- c('항공사', '편명', '목적지', '계획', '예상', '출발', '구분', '현황')
colnames(real_time_df) <- df_col

real_time_df %>% View()

############################### Arrival 예시
#remDr$closeServer()
remDr$open()

sample_url2 <-
  'https://www.airportal.go.kr/life/airinfo/RbHanList.jsp?gubun=c_getList&depArr=A&current_date=20230309&airport=RKSI&fp_id='

sample_url2 <- 'https://www.airportal.go.kr/life/airinfo/RbHanList.jsp?gubun=c_getList&depArr=D&current_date=20231017&airport=RKNY&al_icao=&fp_id='

remDr$navigate(sample_url)
page_source <- remDr$getPageSource()[[1]]

page <- read_html(page_source)
#경로를 한번 살펴보자

road2 <- '/html/body/form/table/tbody/tr/td/table/tbody/tr[13]'

trs <- page %>% html_node('body') %>%
  html_node('form') %>%
  html_node('table') %>%
  html_node('tbody') %>%
  html_node('tr') %>%
  html_node('td') %>%
  html_node('table') %>%
  html_node('tbody') %>%
  html_nodes('tr')

trs %>% html_node('td')# %>% html_text() %>% '['(1)

#벡터 자체에서 마지막 원소를 빼버리고 싶긴 한데..
tds <- trs %>% html_nodes('td') %>% html_text(trim = T)
tds <- tds[-length(tds)]
tds <- tds[c(T, F)]

df_col2 <- c('항공사', '편명', '출발지', '계획', '예상', '도착', '구분', '현황')

real_time_df2 <- matrix(tds, ncol = 8, byrow = T) %>%
  data.frame()
colnames(real_time_df2) <- df_col2

real_time_df2 %>% View()
############################### 전범위 스크래핑

real_time <- function(Arr_Dep, date, icaos){
  all_dataframes <- list()
  
  for(icao in icaos){
    
    
    base_url <- "https://www.airportal.go.kr/life/airinfo/RbHanList.jsp?gubun=c_getList&"
    
    url = paste0(base_url,'depArr=',Arr_Dep,'&current_date=',date,'&airport=',icao,'&fp_id=')
    
    remDr$navigate(url)
    page_source <- remDr$getPageSource()[[1]]
    page <- read_html(page_source)
    
    
    if(Arr_Dep == "D"){
      tr <- page %>% html_node('body') %>%
        html_node('form') %>%
        html_node('table') %>%
        html_node('tbody') %>%
        html_node('td') %>% 
        html_node('tbody')
      
      td <- tr %>% html_nodes('td') %>% html_text(trim = T)
      td <- td[-length(td)]
      td <- td[c(T,F)]
      
      df_col <- c('항공사','편명','목적지','계획','예상','출발','구분','현황')
      
      real_time_df <- matrix(td, ncol = 8, byrow = T) %>%
        data.frame()
      colnames(real_time_df) <- df_col
      
      nrows <- nrow(real_time_df)
      
      
      real_time_df$날짜 = rep(date, nrows) 
      real_time_df$공항 = rep(kr_airport$공항명[kr_airport$ICAO == icao],nrows)
      
      all_dataframes[[icao]] <- real_time_df
      
    }else{
      trs <- page %>% html_node('body') %>%
        html_node('form') %>%
        html_node('table') %>%
        html_node('tbody') %>%
        html_node('tr') %>% 
        html_node('td') %>%
        html_node('table') %>%
        html_node('tbody') %>%
        html_nodes('tr')
      
      #벡터 자체에서 마지막 원소를 빼버리고 싶긴 한데..
      tds <- trs %>% html_nodes('td') %>% html_text(trim= T)
      tds <- tds[-length(tds)]
      tds <- tds[c(T,F)]
      
      df_col2 <- c('항공사','편명','출발지','계획','예상','도착','구분','현황')
      
      real_time_df <- matrix(tds, ncol = 8, byrow = T) %>%
        data.frame()
      colnames(real_time_df) <- df_col2
      
      nrows <- nrow(real_time_df)
      
      real_time_df$날짜 = rep(date, nrows) 
      real_time_df$공항 = rep(kr_airport$공항명[kr_airport$ICAO == icao],nrows)
      
      
      all_dataframes[[icao]] <- real_time_df
    }
  }
  combined_df <- bind_rows(all_dataframes, .id = "icao")
  combined_df <- combined_df[-which(combined_df$편명=="검색된 결과가 없습니다"),]
  return(combined_df)
}


a <- real_time('A','20230921',kor_code) #굿!

kr_airport

kor_code

remDr$closeServer()
remDr$close()

setwd("/home/tjdbswkd4321@naver.com/realtime_data/")

test_day <- gsub('-','',seq(as.Date("2007-11-09"), as.Date("2007-11-20"), by="day"))

for (date in test_day) {
  result <- real_time('D', date, icaos = kor_code)
  csv_file_name <- paste0(date, "_",'D')
  write.csv(result, file = csv_file_name, row.names = FALSE)
}

real_time('D','20071109',kor_code) #?

test_day <- seq(as.Date("2007-11-09"), as.Date("2007-11-20"), by="day")
