library(travelpaths)
set.seed(2025)
df <- sim_travel_paths(2,3, format = "data.frame")
df2 <- df[sample(6),]


order(df2$id, df2$time)
df2[order(df2$id, df2$time),]


df <- sim_travel_paths(2,3, format = "trackframe")
df2 <- df[sample(6),]


order(df2$id, df2$time)
df2[order(df2$id, df2$time),]


data <- df2

time(data)
