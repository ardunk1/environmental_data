scores <- c(0, 3, 5, 8, 10)
names(scores) <- c("low", "mod-low", "mod", "mod-high", "high")
scores            


p <- permutations(n = 5, r = 4, scores, repeats.allowed = TRUE)

dim(p)


library(magrittr)

p2 <- t(p)
dim(p2)                        

class(p)
p3 <- as.data.frame(p)
sc <- p3 %>% dplyr::summarise(score = )

names(p3) <- c("D1", "D2", "D3", "D4")
head(p3)
m <- p3 %>% rowMeans(na.rm = T)

p4 <- data.frame(p3, m)
head(p4)
hist(p4$m)


write.csv(p3, "C:/Users\adrienne.dunk\Documents\Personal\Grad_School\Course Work" row.names = FALSE)

getwd()
