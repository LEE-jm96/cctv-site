install.packages("ggmap") #���� �׷��� ��Ű��
install.packages("readxl") # excel ��Ű��
setwd("C:/Users/ljm51/Desktop/20155151������/20155151������") #�۾� ���丮

library(ggmap)
library(data.table)
library(tidyverse)

#���۸� API ȹ��
register_google(key = "AIzaSyBymIq_W1R4vkYA0J0GS0YLAilXVgzXdIk") #�ؽ�Ʈ ����
has_google_key() #api ����
library(readxl) # excel load�� ���� ��Ű��
cctv_list <- list() # empty ����Ʈ ����
excel_sheets(path = "��󳲵�_��õ��_��̺�ȣ����_20180806.xls") #excel�� sheet Ȯ��

#excel�� sheet �� ��ŭ �ݺ� ����, list�� ��ҷ� �� sheet�� ���������������� ����
for(index in 1:length(excel_sheets(path = "��󳲵�_��õ��_��̺�ȣ����
                                   _20180806.xls"))){
  cctv_list[[index]] <- read_xls(path = "��󳲵�_��õ��_��̺�ȣ����_20180806.xls")
  }
cctv_list_df <- cctv_list%>%bind_rows()%>%as_tibble() #bind rows list�� �������������� row�� �ϳ��� ���������������� ��ȯ
str(cctv_list_df)

#�ʿ��� ���� ����
cctv_list_df_lat_lon <- cctv_list_df%>%select(., ���������θ��ּ�,����,�浵 )%>%
  mutate(., ���� = as.numeric(����), �浵 = as.numeric(�浵))
str(cctv_list_df_lat_lon) #mutate�� �̿��Ͽ� ���� type�� ����

#x11, �������� ũ�⸦ 20,000���� �ε����Ͽ� ���
qmap(title = "20155151", location = "�泲", zoom = 6, maptype = 'satellite', source = 'google') +
  geom_point(data = cctv_list_df_lat_lon[1:20000,], aes(x = �浵, y = ����), color = 'red', size = 2, alpha = 0.5) + ggtitle("��󳲵�_��õ��_��̺�ȣ����_cctv_20155151")


#ggtitle�� �̿��� Ÿ��Ʋ ����, theme�� �̿��� Ÿ��Ʋ �� legend ����
qmap(location = center, zoom = 15, maptype = 'hybrid', source = 'google') +
  geom_point(data = cctv_list_chunchen, aes(x = �浵, y = ����, color = CCTV��ġ����), size = 4, alpha = 0.5) +
  ggtitle("��󳲵�_��õ��_��̺�ȣ����_cctv_20155151") +
  theme(legend.text = element_text(face = "bold",size = 15,family = "BM HANNA Pro"),
        legend.title = element_text(size = 15, face = "bold", family = "BM HANNA Pro"),
        legend.position = "top",
        plot.title = element_text(family = "BM HANNA
        Pro", face = "bold", hjust = 0.5, size = 25)) +
        facet_wrap(~CCTV��ġ����)


library(leaflet)

#addTiles() apiȣ��, setView() ����
m <- leaflet() %>% addTiles() %>%
  setView(lng = 128.315457, lat = 35.557367, zoom = 16)
m

#leaflect�ɼ� �߰�
leaflet(cctv_list_chunchen) %>% addTiles() %>%
  setView(lng = 128.315457, lat = 35.557367, zoom = 12)%>%
  addRectangles(lng1 =128.281212,lat1=35.545991,
                lng2 =128.315457,lat2=35.557367,
                fillColor ="transparent") %>% #���� �浵 ���� �� ���簢���� �׸���.
  addCircleMarkers(lat = ~����, lng = ~�浵, color = ~pal(�ü�����), popup = ~�ü���
                   ��) %>%
  addLegend(pal = pal, values = cctv_list_chunchen$CCTV��ġ����, opacity = 0.7,
            title = "CCTV��ġ����_20155151", position = "bottomright") %>%
  addProviderTiles(providers$OpenStreetMap) %>% addMiniMap()