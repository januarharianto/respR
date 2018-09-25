devtools::install_github('colin-olito/LoLinR')
library(LoLinR)


LoL_sard <- rankLocReg(xall=UrchinData$time, yall=UrchinData$D, alpha=0.2,
                       method="eq", verbose=TRUE)

sard_df <- data.frame(respR::sardine.rd)

sard_thin <- thinData(sard_df, by = 10)
sard_thin <- sard_thin[[1]]

sard_thin_2 <- thinData(sard_thin, by = 2)
sard_thin_2 <- sard_thin_2[[1]]


start.time <- Sys.time()
LoL_sard <- rankLocReg(xall=sard_thin_2$Time, yall=sard_thin_2$Oxygen, alpha=0.2,
                       method="eq", verbose=TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

sard_thin <- LoLinR::thinData(sard_df, by = 10)
sard_thin <- sard_thin[[1]]

# 100
data <- sard_thin[1:100,]
start.time <- Sys.time()
LoL_sard <- rankLocReg(xall=data$Time, yall=data$Oxygen, alpha=0.2,
                       method="eq", verbose=TRUE)
end.time <- Sys.time()
t_100 <-  end.time - start.time
t_100

# 200
data <- sard_thin[1:200,]
start.time <- Sys.time()
LoL_sard <- rankLocReg(xall=data$Time, yall=data$Oxygen, alpha=0.2,
                       method="eq", verbose=TRUE)
end.time <- Sys.time()
t_200 <-  end.time - start.time
t_200

# 300
data <- sard_thin[1:300,]
start.time <- Sys.time()
LoL_sard <- rankLocReg(xall=data$Time, yall=data$Oxygen, alpha=0.2,
                       method="eq", verbose=TRUE)
end.time <- Sys.time()
t_300 <-  end.time - start.time
t_300

# 400
data <- sard_thin[1:400,]
start.time <- Sys.time()
LoL_sard <- rankLocReg(xall=data$Time, yall=data$Oxygen, alpha=0.2,
                       method="eq", verbose=TRUE)
end.time <- Sys.time()
t_400 <-  end.time - start.time
t_400


# 500
data <- sard_thin[1:500,]
start.time <- Sys.time()
LoL_sard <- rankLocReg(xall=data$Time, yall=data$Oxygen, alpha=0.2,
                       method="eq", verbose=TRUE)
end.time <- Sys.time()
t_500 <-  end.time - start.time
t_500


# 600
data <- sard_thin[1:600,]
start.time <- Sys.time()
LoL_sard <- rankLocReg(xall=data$Time, yall=data$Oxygen, alpha=0.2,
                       method="eq", verbose=TRUE)
end.time <- Sys.time()
t_600 <-  end.time - start.time
t_600



for(i in 1:9){
  ## random number 100:500
  #rn <- round(runif(1, 100, 500))

  sequ <- seq(100,500,50)
  rn <- sequ[i]

  start.time <- Sys.time()
  rankLocReg(xall=sard_thin$Time[1:rn], yall=sard_thin$Oxygen[1:rn], alpha=0.2,
                                    method="eq", verbose=TRUE)

  end.time <- Sys.time()
  total.time <- difftime(end.time, start.time, units="secs")


  if(i == 1){
    LoL_times <- data.frame(Length = rn, Time = total.time)
  } else {
      LoL_times[i,1] <- rn
      LoL_times[i,2] <- total.time
    }
}

LoL_times$Time <- as.numeric(LoL_times$Time)

plot(log(LoL_times$Time)~log(LoL_times$Length))

fit <- lm(log(LoL_times$Time) ~ log(LoL_times$Length))

fit$coefficients

plot(LoL_times)

lines(LoL_times$Length, LoL_times$Length ^ fit$coefficients[2], col = "red")



fit <- lm(LoL_times_log$Time ~ LoL_times_log$Length)
int <- fit$coefficients[1]
slp <- fit$coefficients[2]

plot(log(LoL_times$Time)~log(LoL_times$Length))


lines(LoL_times_log$Length, LoL_times_log$Length * slp + int, col = "red")

LoL_times_log$Length ^ fit$coefficients[2]

write.csv(LoL_times, file = "loltimes.csv")
