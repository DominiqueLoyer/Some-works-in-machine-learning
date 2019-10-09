2044 master and doctorate plus grand que 50k 1181 = 3,87%
23265 plus petit ou égal 50k


missing data
workclass 1769
occupation Status 1774
native country 531	

Census <- read_excel("~/Desktop/PwC/Census.xlsx")
View(Census)
summary(Census)
attach(Census)
str(Census)
head(Census)
tail(Census)
getwd()
pwc <- read_excel("Census.xlsx")
view(pwc)
pwc
View(pwc)
str(pwc)
head(pwc)
subset (pwc, Education %in% "Masters")
subset (pwc, Education %in% "Masters" AND `Income Group` %in% "50k" OR `Income Group` %in% "50k.")
subset (pwc, Education %in% "Masters" AND Income Group %in% "50k" OR Income Group %in% "50k.")
subset (pwc, Education %in% "Masters" OR "Doctorates")
subset (pwc, Education %in% "Masters" | "Doctorates")
subset (pwc, Education %in% "Masters"|"Doctorates")
subset (pwc, Education %in% "Masters")
master <- subset (pwc, Education %in% "Masters")
filter(master)
master50kplus <- subset (pwc, `Income Group` %in% "50k")
master50kplus
master50kplusDot <- subset (master50kplus, `Income Group` %in% "50k.")
master50kplusDot
master
master50kplus <- subset (master, `Income Group` %in% "50k")
master50kplus
master50kplus <- subset (master, `Income Group` %in% ">50k")
master50kplus
master50kplus <- subset (master, `Income Group` %in% >50k)
master50kplus <- subset (master, `Income Group` >50k)
master50kplus <- subset (master, `Income Group` >50k)
master
master$`Income Group`
master50kplus <- subset (master, `Income Group` %in% ">50K")
master50kplus
master50kplusDot <- subset (master50kplus, `Income Group` %in% ">50K.")
master50kplusDot
master50kplus
master50kplusDot <- subset (master, `Income Group` %in% ">50K.")
master50kplus+master50kplusDot
master50kplusDot
dim(master50kplus)
dim(master50kplusDot)
426+500
totalMaster50kplus <- 500+426
totalMaster50kplus
doctorate <- subset (pwc, Education %in% "Doctorate")
doctorate
doctorate50kplus <- subset (doctorate, `Income Group` %in% ">50K")
doctorate50kplus
doctorate50kplusDot <- subset (doctorate, `Income Group` %in% ">50K.")
doctorate50kplusDot
dim(doctorate50kplus)
dim(doctorate50kplusDot)
totalDoctorate50plus <- 130+125
totalDoctorate50plus
totalDoctorate50plus+totalMaster50kplus
TotalDocPlusMasterEarn50kplus <- totalDoctorate50plus+totalMaster50kplus
TotalDocPlusMasterEarn50kplus / 30511
people50less <- subset (pwc, `Income Group` %in% "<=50K")
people50less
people50lessDot <- subset (pwc, `Income Group` %in% "<=50K.")
people50lessDot
dim(people50less)
dim(people50lessDot)
peopleLess50K <- 10835+12430
peopleLess50K
plot(peopleLess50K)
hist(peopleLess50K)
hist(pwc)
subset (pwc, Age <=18)
subset (pwc, Age <=15)
subset (pwc, Age <=16)
subset (pwc, Age <=17)