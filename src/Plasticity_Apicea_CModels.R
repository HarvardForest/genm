temperature <- c(10,15,20,25)
TraitHMP_P1 <- c(0.4, 0.6, 0.8, 1.0)
TraitHMP_P2 <- c(0.1, 0.3, 0.5, 0.7)
TraitHMP_P3 <- c(0.38, 0.52, 0.66, 0.8)
plot(temperature, TraitHMP_P1, cex=0.01, ylim=c(0,1.0), xlim=c(10,25), main="High Margin Plasticity", xlab="Temperature(C)", ylab="Trait")
plot(temperature, TraitHMP_P2, cex=0.01, ylim=c(0,1.0), xlim=c(10,25), main="High Margin Plasticity", xlab="Temperature(C)", ylab="Trait")
plot(temperature, TraitHMP_P3, cex=0.01, ylim=c(0,1.0), xlim=c(10,25), main="High Margin Plasticity", xlab="Temperature(C)", ylab="Trait")
lines(temperature, TraitHMP_P2, col="blue")
lines(temperature, TraitHMP_P1, col="red")
lines(temperature, TraitHMP_P3, col="yellowgreen")

P1_fit <- c(.72, .92)
P1_temp <- c(18,23)
lines(P1_temp, P1_fit, col="red", lwd=3)


P2_fit <- c(.18, .42)
P2_temp <- c(12,18)
lines(P2_temp, P2_fit, col="blue", lwd=3)

P3_try <-c(15,18) 
P3_fail <-c(.52, .604)
lines(P3_try, P3_fail, col="yellowgreen", lwd=3)

legend(22.5, 0.2, legend=c("P1", "P2", "P3"),col = c("red", "blue", "yellowgreen"), lty=1, cex=0.8)

ddg.run("Plasticity_Apicea_CModels.R")
