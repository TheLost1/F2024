dfload <- read.csv("C:\\Users\\Bruger\\Desktop\\F2024\\CallApple.csv")

K_list <- list("K100", "K105", "K110", "K115", "K120", "K125", "K126", "K127", "K128", "K129", "K130", "K131", "K132", "K133", "K134", "K135", "K136", "K137", "K138", "K139", "K140", "K141", "K142", "K143", "K144", "K145", "K146", "K147", "K148", "K149", "K150", "K155", "K160", "K165", "K170", "K175", "K180", "K185", "K190", "K195", "K200")


for (i in 1:length(K_list)) {
  colnames(dfload)[i+2] <- K_list[i]
}
dfload
colnames(dfload)[5] <- K_list[3]

write.csv(dfload, "CallApple.csv")
