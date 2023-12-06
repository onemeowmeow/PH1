URL = "https://reg.ntuh.gov.tw/EmgInfoBoard/NTUHEmgInfo.aspx"

txt = scan(URL, what = "character", encoding = "UTF-8", quiet = TRUE)

head(txt, 15) #擷取前15行資訊
txt_new = paste(txt, sep = "", collapse = " ")

TITLE.pos = gregexpr("<title>.*</title>", txt_new)  #title 開始位置
start.TITLE.pos = TITLE.pos[[1]][1]
end.TITLE.pos = start.TITLE.pos + attr(TITLE.pos[[1]], "match.length")[1] - 1

TITLE.word = substr(txt_new, start.TITLE.pos, end.TITLE.pos)   #擷取title 開始位置到結束位置中間的內容

TITLE.word