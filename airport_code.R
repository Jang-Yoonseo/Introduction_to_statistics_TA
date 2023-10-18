library(RSelenium)
library(rvest)
library(tidyverse)
library(httpuv)

#library(googledrive)

#shell('docker ps')

remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  # 호스트의 IP 주소 또는 호스트 이름
  port = 4445,
  # 호스트와 매핑된 포트
  browserName = "chrome"           # 사용할 브라우저 (여기서는 Chrome)
)

remDr$open()

code_url <-
  'https://www.airportal.go.kr/knowledge/airports/KbAirport01.jsp?PAGENO=1&PAGEROWS=7666&START=&keyword1=&keyword2=&gubun=&sortvalue=name&order=1&target=&search='

remDr$navigate(code_url)
page_source <- remDr$getPageSource()[[1]]

page_html <- page_source %>% read_html()

trs <- page_html %>%
  html_node('body') %>%
  html_node('table') %>%
  html_nodes('tbody') %>%
  html_nodes('tr')

#노드 구조를 다시 한번 봐야 할듯..?
#이게 구조가 육안이랑 다른가 싶은데
#다시 살펴봐야할듯..?
trs <- trs[39:7704]

code_col <- c('번호', '공항명', 'IATA', 'ICAO', '국가명', '도시명')

all_elements <- trs %>% html_nodes('td') %>% html_text()
all_elements2 <- #두 번째 원소마다 빈칸임
  all_elements[c(T, F, T, T, T, T, T)]

airport_code <-
  data.frame(matrix(all_elements2, ncol = 6 , byrow = T))# %>% data_frame()
colnames(airport_code) <- code_col

airport_code <- airport_code %>% tibble()

kor_airport <- airport_code %>% 
  filter(국가명 == 'Republic of Korea') %>% 
  unique()#공항, 코드만 보고싶다.


kor_airport #한국의 공항 코드들

kr_airport <- kor_airport[-c(9,13),] #제주 정석비행장, 서울공항은 기밀시설이라 정보 x

kor_code <- kr_airport$ICAO
airport_name <- kr_airport$공항명 
