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

