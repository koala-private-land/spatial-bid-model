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

get.predxz <- function(Row, Data, Coefs)
{
	for (i in 1:length(Data)) {
		# get predictions
		PredsTemp <- Data[[i]][Row, ] %*% t(Coefs[[i]])
		PredsTemp <- exp(PredsTemp) / (1 + exp(PredsTemp))
		if (i == 1) {
				Preds <- PredsTemp
		} else {
			Preds <- c(Preds, PredsTemp)
		}
	}

	# expected value
	Mean <- mean(Preds, na.rm = TRUE)
	# standard deviation
	SD <- sd(Preds, na.rm = TRUE)
	# highest density interval
	HDI <- hdi(as.vector(Preds))

	return(tibble(Mean = Mean, SD = SD, Lower = HDI[1], Upper = HDI[2]))
}

get.predy <- function(Row, Data, Coefs)
{
	for (i in 1:length(Data)) {
		# get predictions
		PredsTemp <- Data[[i]][Row, ] %*% t(Coefs[[i]])
		if (i == 1) {
				Preds <- PredsTemp
		} else {
			Preds <- c(Preds, PredsTemp)
		}
	}

	# expected value
	Mean <- mean(Preds, na.rm = TRUE)
	# standard deviation
	SD <- sd(Preds, na.rm = TRUE)
	# highest density interval
	HDI <- hdi(as.vector(Preds))

	return(tibble(Mean = Mean, SD = SD, Lower = HDI[1], Upper = HDI[2]))
}

get.predxz.list <- function(DataList, CoefsList, NumCores)
{
	cl <- makeCluster(NumCores)
	clusterExport(cl, varlist = c("DataList", "CoefsList", "get.predxz"), envir = environment())
	clusterEvalQ(cl, library("HDInterval"))
	clusterEvalQ(cl, library("tidyverse"))
	Predictions <- parApply(cl, as.matrix(1:nrow(DataList[[1]])), MARGIN = 1, FUN = get.predxz, Data = DataList, Coefs = CoefsList)
	stopCluster(cl)
	Predictions <- do.call("rbind", Predictions)

	return(as_tibble(Predictions))
}

get.predy.list <- function(DataList, CoefsList, NumCores)
{
	cl <- makeCluster(NumCores)
	clusterExport(cl, varlist = c("DataList", "CoefsList", "get.predy"), envir = environment())
	clusterEvalQ(cl, library("HDInterval"))
	clusterEvalQ(cl, library("tidyverse"))
	Predictions <- parApply(cl, as.matrix(1:nrow(DataList[[1]])), MARGIN = 1, FUN = get.predy, Data = DataList, Coefs = CoefsList)
	stopCluster(cl)
	Predictions <- do.call("rbind", Predictions)

	return(as_tibble(Predictions))
}
