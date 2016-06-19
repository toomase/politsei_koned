# Politsei Facebooki postituste laadimine
library(Rfacebook)

# ajutine token
token <- "1546706838966767|N-8DPfW0q9uRUQmsh0X3AbmvexQ"

politsei_postitused_2016_raw <- getPage("politseijapiirivalveamet", token, n = 1000,
                                        since = '2016/03/03', until = '2016/03/06')

politsei_postitused_2015_raw <- getPage("politseijapiirivalveamet", token, n = 1000,
                                        since = '2015/06/11', until = '2015/06/14')

politsei_postitused_2014_raw <- getPage("politseijapiirivalveamet", token, n = 1000,
                                        since = '2014/06/22', until = '2014/06/25')

# erinevate aastate teated kokku ja kellaaegade korrigeerimine
politsei_postitused_raw <- politsei_postitused_2016_raw %>%
    mutate(aeg = str_replace_all(created_time, "T|\\+0000", ""),
           aeg = parse_date_time(aeg, "ymdHMS"),
           aeg = aeg + hours(1)) %>%
    bind_rows(politsei_postitused_2015_raw %>%
                  bind_rows(politsei_postitused_2014_raw) %>%
                  mutate(aeg = str_replace_all(created_time, "T|\\+0000", ""),
                         aeg = parse_date_time(aeg, "ymdHMS"),
                         aeg = aeg + hours(2)))

save(politsei_postitused_raw, file = "data/politsei_postitused.Rdata")
