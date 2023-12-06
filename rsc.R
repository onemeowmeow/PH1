URL = "https://reg.ntuh.gov.tw/EmgInfoBoard/NTUHEmgInfo.aspx"

txt = scan(URL, what = "character", encoding = "UTF-8", quiet = TRUE)

head(txt, 15) #擷取前15行資訊
txt_new = paste(txt, sep = "", collapse = " ")

TITLE.pos = gregexpr("<title>.*</title>", txt_new)  
start.TITLE.pos = TITLE.pos[[1]][1] #title 開始位置
end.TITLE.pos = start.TITLE.pos + attr(TITLE.pos[[1]], "match.length")[1] - 1 #title 結束位置

TITLE.word = substr(txt_new, start.TITLE.pos, end.TITLE.pos)   #擷取title 開始位置到結束位置中間的內容


TITLE.word = gsub("<title>", "", TITLE.word) #把印出來的文字去掉<title>標籤
TITLE.word = gsub("</title>", "", TITLE.word)
TITLE.word 


NTU_info = function () {
  
  result = data.frame(item = c('等候掛號人數', '等候看診人數', '等候住院人數', '等候ICU人數', '等候推床人數'),
                      info = NA,
                      stringsAsFactors = FALSE)
  
  URL = "https://reg.ntuh.gov.tw/EmgInfoBoard/NTUHEmgInfo.aspx"
  
  txt = scan(URL, what = "character", encoding = "UTF-8", quiet = TRUE)
  txt_new = paste(txt, sep = "", collapse = " ")
  
  start.pos = gregexpr("<tr>", txt_new)    #抓<tr>開始位置
  end.pos = gregexpr("</tr>", txt_new)     #抓</tr>位置
  
  for (i in 1:5) {
    
    sub.start.pos = start.pos[[1]][i]
    sub.end.pos = end.pos[[1]][i] + attr(end.pos[[1]], "match.length")[i] - 1
    
    sub_txt = substr(txt_new, sub.start.pos, sub.end.pos)
    sub_txt = gsub('等.*：', '', sub_txt)
    sub_txt = gsub('</?tr>', '', sub_txt)
    sub_txt = gsub('</?td>', '', sub_txt)
    result[i,'info'] = gsub(' ', '', sub_txt)
    
  }
  
  result
  
}

NTU_info()

library(rvest)

URL = "https://reg.ntuh.gov.tw/EmgInfoBoard/NTUHEmgInfo.aspx"

website = read_html(URL)

needed_txt = website %>% html_nodes("tr") %>% html_text()
needed_txt


#Practice1: 擷取PTT勘版文章
URL = "https://www.ptt.cc/bbs/Baseball/index.html"
website = read_html(URL)

needed_html = website %>% html_nodes("a")
needed_html
needed_txt = needed_html %>% html_text()
needed_txt
intrested_pos = grep("[新聞]", needed_txt, fixed = TRUE)
needed_txt[intrested_pos]
needed_link = needed_html[intrested_pos] %>% html_attr("href")  #擷取文章連結
i = 1
sub_link = paste("https://www.ptt.cc", needed_link[i], sep = "")
sub_website = read_html(sub_link) 

article_info = sub_website %>% html_nodes(".article-meta-value")
article_info


#調閱最近的10篇籃球版上的新聞文章

my_table = matrix("", nrow = 10, ncol = 4)
colnames(my_table) = c("Title", "url", "ID", "time")

URL = "https://www.ptt.cc/bbs/Baseball/index.html"
current_id = 1

for (i in 1:10) {
  
  website = read_html(URL)
  needed_html = website %>% html_nodes("a")
  needed_txt = needed_html %>% html_text()
  intrested_pos = grep("[新聞]", needed_txt, fixed = TRUE)
  
  if (length(intrested_pos) > 0) {
    
    for (j in intrested_pos) {
      
      if (current_id <= 10) {
        my_table[current_id, 1] = needed_txt[j]
        my_table[current_id, 2] = needed_html[j] %>% html_attr("href")
      }
      
      current_id = current_id + 1
      
    }
    
  }
  
  if (current_id > 10) {
    break
  }
  
  next_page = website %>% html_nodes("a") %>% .[8] %>% html_attr("href")
  URL = paste0("https://www.ptt.cc", next_page, sep = "")
  
}

for (i in 1:nrow(my_table)) {
  
  sub_URL = paste("https://www.ptt.cc", my_table[i, 2], sep = "")
  sub_website = read_html(sub_URL)
  article_info = sub_website %>% html_nodes(".article-meta-value") %>% html_text()
  my_table[i, 3] = article_info[1]
  my_table[i, 4] = article_info[4]
  
}

my_table

