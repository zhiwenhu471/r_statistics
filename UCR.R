UCR <- data.frame(
  age = c(13, 11, 9, 6, 8, 10, 12, 7, 10, 9, 11, 12, 15, 16, 8, 7, 10, 15), 
  ucr = c(3.54, 3.01, 3.09, 2.48, 2.56, 3.36, 3.18, 2.65, 3.01, 2.83, 2.92, 
          3.09, 3.98, 3.89, 2.21, 2.39, 2.74, 3.36), 
  group = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
)
UCR

# 给每列添加标签
library(Hmisc)
label(UCR$age) <- "Age in years"
label(UCR$ucr) <- "Urine creatine (mmol)"
label(UCR$group) <- "Type of chilren"
describe(UCR)

# 把UCR数据框存为R数据文件
save(UCR, file = "UCR.rdata")


