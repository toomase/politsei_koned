# Politsei Facebooki postituste laadimine
library(Rfacebook)

# ajutine token
token <- "EAACEdEose0cBAC6QBA6bvAIQbdt3xAZA6PHcm0hFYnrff8O5lZBvRp9lBDsFwCchn9Fb1IF5emrPpKEyYRenUg4YUzjTkoDhdjxaR1aRY8O8byJErusEyInIX0lQMo6ZARyUQhZCN7L3vEMKse9lGIja3ZAoEcYUT61Px9m8nnAZDZD"

politsei_postitused_raw <- getPage("politseijapiirivalveamet", token, n = 1000,
                since = '2016/03/03', until = '2016/03/06')

save(politsei_postitused_raw, file = "data/politsei_postitused.Rdata")