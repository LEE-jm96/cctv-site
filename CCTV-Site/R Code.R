install.packages("ggmap") #지도 그래프 패키지
install.packages("readxl") # excel 패키지
setwd("C:/Users/ljm51/Desktop/20155151이정민/20155151이정민") #작업 디렉토리

library(ggmap)
library(data.table)
library(tidyverse)

#구글맵 API 획득
register_google(key = "AIzaSyBymIq_W1R4vkYA0J0GS0YLAilXVgzXdIk") #텍스트 복사
has_google_key() #api 인증
library(readxl) # excel load를 위한 패키지
cctv_list <- list() # empty 리스트 선언
excel_sheets(path = "경상남도_합천군_어린이보호구역_20180806.xls") #excel의 sheet 확인

#excel이 sheet 수 만큼 반복 진행, list의 요소로 각 sheet를 데이터프레임으로 저장
for(index in 1:length(excel_sheets(path = "경상남도_합천군_어린이보호구역
                                   _20180806.xls"))){
  cctv_list[[index]] <- read_xls(path = "경상남도_합천군_어린이보호구역_20180806.xls")
  }
cctv_list_df <- cctv_list%>%bind_rows()%>%as_tibble() #bind rows list의 데이터프레임의 row를 하나의 데이터프레임으로 변환
str(cctv_list_df)

#필요한 열만 추출
cctv_list_df_lat_lon <- cctv_list_df%>%select(., 소재지도로명주소,위도,경도 )%>%
  mutate(., 위도 = as.numeric(위도), 경도 = as.numeric(경도))
str(cctv_list_df_lat_lon) #mutate를 이용하여 열의 type을 변경

#x11, 데이터의 크기를 20,000까지 인덱싱하여 사용
qmap(title = "20155151", location = "경남", zoom = 6, maptype = 'satellite', source = 'google') +
  geom_point(data = cctv_list_df_lat_lon[1:20000,], aes(x = 경도, y = 위도), color = 'red', size = 2, alpha = 0.5) + ggtitle("경상남도_합천군_어린이보호구역_cctv_20155151")


#ggtitle을 이용한 타이틀 설정, theme를 이용한 타이틀 및 legend 설정
qmap(location = center, zoom = 15, maptype = 'hybrid', source = 'google') +
  geom_point(data = cctv_list_chunchen, aes(x = 경도, y = 위도, color = CCTV설치여부), size = 4, alpha = 0.5) +
  ggtitle("경상남도_합천군_어린이보호구역_cctv_20155151") +
  theme(legend.text = element_text(face = "bold",size = 15,family = "BM HANNA Pro"),
        legend.title = element_text(size = 15, face = "bold", family = "BM HANNA Pro"),
        legend.position = "top",
        plot.title = element_text(family = "BM HANNA
        Pro", face = "bold", hjust = 0.5, size = 25)) +
        facet_wrap(~CCTV설치여부)


library(leaflet)

#addTiles() api호출, setView() 지정
m <- leaflet() %>% addTiles() %>%
  setView(lng = 128.315457, lat = 35.557367, zoom = 16)
m

#leaflect옵션 추가
leaflet(cctv_list_hapcheon) %>% addTiles() %>%
  setView(lng = 128.315457, lat = 35.557367, zoom = 12)%>%
  addRectangles(lng1 =128.281212,lat1=35.545991,
                lng2 =128.315457,lat2=35.557367,
                fillColor ="transparent") %>% #위도 경도 설정 후 직사각형을 그린다.
  addCircleMarkers(lat = ~위도, lng = ~경도, color = ~pal(시설종류), popup = ~시설종
                   류) %>%
  addLegend(pal = pal, values = cctv_list_hapcheon$CCTV설치여부, opacity = 0.7,
            title = "CCTV설치여부_20155151", position = "bottomright") %>%
  addProviderTiles(providers$OpenStreetMap) %>% addMiniMap()
