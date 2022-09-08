get.jags.sel <- function(Data)
{
	# get initial values for censored WTA
  get.WTA.inits <- function(WTA, CENS) {
      Inits.WTA <- rep(NA, length(WTA))
      Inits.WTA[which(CENS == 0)] <- runif(length(which(CENS == 0)), -2.5, 0)
      Inits.WTA[which(CENS == 2)] <- runif(length(which(CENS == 2)), 2.5, 5)

      return(Inits.WTA)
  }

	# get initial values for remaining parameters

	inits1 <- list(WTA = get.WTA.inits(Data$WTA, Data$CENS), sig = runif(1, 0, 2), phi = runif(1, 0, 2), b_x = runif(Data$NCOV, -2, 2), b_y = runif(Data$NCOV, -2, 2), b_z = runif(Data$NCOV, -2, 2), bsa_x = c(NA, runif(Data$NCOVSA - 1, -2, 2)), bsa_y = c(NA, runif(Data$NCOVSA - 1, -2, 2)), bsa_z = c(NA, runif(Data$NCOVSA - 1, -2, 2)))

	inits2 <- list(WTA = get.WTA.inits(Data$WTA, Data$CENS), sig = runif(1, 0, 2), phi = runif(1, 0, 2), b_x = runif(Data$NCOV, -2, 2), b_y = runif(Data$NCOV, -2, 2), b_z = runif(Data$NCOV, -2, 2), bsa_x = c(NA, runif(Data$NCOVSA - 1, -2, 2)), bsa_y = c(NA, runif(Data$NCOVSA - 1, -2, 2)), bsa_z = c(NA, runif(Data$NCOVSA - 1, -2, 2)))

	inits3 <- list(WTA = get.WTA.inits(Data$WTA, Data$CENS), sig = runif(1, 0, 2), phi = runif(1, 0, 2), b_x = runif(Data$NCOV, -2, 2), b_y = runif(Data$NCOV, -2, 2), b_z = runif(Data$NCOV, -2, 2), bsa_x = c(NA, runif(Data$NCOVSA - 1, -2, 2)), bsa_y = c(NA, runif(Data$NCOVSA - 1, -2, 2)), bsa_z = c(NA, runif(Data$NCOVSA - 1, -2, 2)))

  cl <- makeCluster(3) # comment out if not using parallel processing

  # edit methods depending on whether using parallel processing or not
	fit <- run.jags(model="wta-jags-model-sel.txt", monitor = c("eta_g_x", "eta_g_y", "eta_g_z", "gamma_gl_x", "gamma_gl_y", "gamma_gl_z", "gammasa_gl_x", "gammasa_gl_y", "gammasa_gl_z", "beta_x", "betasa_x", "beta_y", "betasa_y",
	 	"beta_z", "betasa_z", "sig", "phi"), data = Data, n.chains = 3, inits = list(inits1,inits2,inits3), burnin = 10000, adapt = 1000, sample = 10000,
		jags = "C:/Program Files/JAGS/JAGS-4.3.0/x64/bin/jags-terminal.exe", method = "rjparallel", cl = cl) # method = "rjags")

	stopCluster(cl) # comment out if not using parallel processing

	return(fit)
}

get.predxz <- function(Data, Coefs)
{

	# get predictions
	Preds <- Data %*% t(Coefs)
	Preds <- exp(Preds) / (1 + exp(Preds))

	# expected value
	Mean <- mean(Preds, na.rm = TRUE)
	# standard deviation
	SD <- sd(Preds, na.rm = TRUE)
	# highest density interval
	HDI <- hdi(as.vector(Preds))

	return(tibble(Mean = Mean, SD = SD, Lower = HDI[1], Upper = HDI[2]))
}

get.predy <- function(Data, Coefs)
{

	# get predictions
	Preds <- Data %*% t(Coefs)

	# expected value
	Mean <- mean(Preds, na.rm = TRUE)
	# standard deviation
	SD <- sd(Preds, na.rm = TRUE)
	# highest density interval
	HDI <- hdi(as.vector(Preds))

	return(tibble(Mean = Mean, SD = SD, Lower = HDI[1], Upper = HDI[2]))
}

get.predxz.exp <- function(Data, Coefs)
{

	# get predictions
	Preds <- Data %*% as.matrix(Coefs)
	Preds <- exp(Preds) / (1 + exp(Preds))

	return(tibble(Mean = Preds))
}

get.predy.exp <- function(Data, Coefs)
{

	# get predictions
	Preds <- Data %*% as.matrix(Coefs)

	return(tibble(Mean = Preds))
}
