


line_up = read_html(url) %>%
   html_nodes(xpath = '//*[@id="tteamlist"]')%>%
   html_nodes("div")%>%
   html_nodes("ul")%>%
   html_text() %>%
   str_replace_all("\n","") %>%
   str_replace_all("\r","") %>%
   str_trim() %>%
   as.data.frame()%>%
   rename("Teams" = ".")%>%
   mutate(Teams = as.character(Teams))%>%
   filter(str_detect(Teams, "Buy Tickets") != T)
