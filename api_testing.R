mylocation = "C:/Users/Byeongjun Cho/Desktop/2020-1/데이터사이언스입문/data/open_api"
setwd(mylocation)
library(tidyverse)
library(httr)
library(XML)
library(xml2)

url = "http://openapi2.e-gen.or.kr/openapi/service/rest/ErmctInfoInqireService/"  
operator = "getEmrrmRltmUsefulSckbdInfoInqire"
Servicekey = "your_service_key"
STAGE1 = "부산광역시"
STAGE2 = "동래구"
pageNo = "1"
numOfRows = "10"
queryParams = str_c("?serviceKey=", Servicekey, "&STAGE1=", STAGE1, "&STAGE2=", STAGE2, "&pageNo=", pageNo, "&numOfRows=", numOfRows)

# url_tmp = paste0(
#  url_1, paste0("?ServiceKey=", Servicekey), paste0("&STAGE1=", STAGE1), paste0("&STAGE2=", STAGE2), paste0("&pageNo=", pageNo),
#  paste0("&numOfRows=", numOfRows))

doc = xmlInternalTreeParse(str_c(url, operator, queryParams))
rootNode = xmlRoot(doc)

names = rootNode[[2]][['items']][['item']] %>%
  names()
tmp_tbl = xmlToDataFrame(nodes = getNodeSet(rootNode, '//item')) %>%
  set_names(iconv(names, "UTF-8", "CP949") %>% unname()) %>%
  as_tibble()
result_table = tibble()
result_table = result_table %>% bind_rows(.,tmp_tbl)