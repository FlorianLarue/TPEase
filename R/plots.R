# User-friendly ggplot functions to plot Crop Growth Model results

#' @import ggplot2


#TODO

### Plot functions ###
boxplot_stics <- function(data = all_data, trait = "masec_kg_ha", plant = "Sorghum", divideby = "Var") {
  tmp_data <- data[which(data$Trait == trait & data$Plant == plant),]

  ## Construct dataframe for boxplot with respect to function arguments ##
  bp_data_obs <- data.frame()
  bp_data_res <- data.frame()
  for(i in unique(tmp_data[,divideby])) { #Add colomn to dataframe for each Var
    bp_data_obs <- rbind(bp_data_obs,tmp_data[which(tmp_data[,divideby] == i),c(divideby,"Obs")])
    bp_data_res <- rbind(bp_data_res,tmp_data[which(tmp_data[,divideby] == i),c(divideby,"Sim")])
  }
  colnames(bp_data_obs) <- c(divideby,"Value")
  colnames(bp_data_res) <- c(divideby,"Value")

  ## Combine observations and simulations data ##
  bp_data_obs$Type <- "Observations"
  bp_data_res$Type <- "Simulations"
  tmp_data_boxplot <- rbind(bp_data_obs,bp_data_res)

  ## Draw boxplot ##
  p <- ggplot(tmp_data_boxplot) + geom_boxplot(aes(x=tmp_data_boxplot[,divideby],y=Value, fill=tmp_data_boxplot[,divideby])) + facet_wrap(. ~ Type) + theme(legend.position = "bottom", legend.title=element_blank()) + xlab("") + ylab(t) + ggtitle(paste0(paste0(plant, collapse=" "), " ", trait))
  print(p)
}

simuobs_stics <- function(data = all_data, trait = "masec_kg_ha", plant = "Sorghum") {
  tmp_data <- data[which(data$Trait == trait & data$Plant == plant),]
  p <- ggplot(tmp_data,aes(x=Obs,y=Sim)) + geom_point(aes(col=Syst, shape=Var)) + ggtitle(paste0(paste0(plant, collapse=" "), " ", trait)) + geom_abline(intercept = 0, slope= 1) + facet_grid(. ~ Eval) + coord_fixed(ratio=1, xlim=c(0,1.1*max(c(tmp_data$Obs,tmp_data$Sim),na.rm=T)), ylim=c(0,1.1*max(c(tmp_data$Obs,tmp_data$Sim),na.rm=T)))
  print(p)
}

dynamics_stics <- function(data_obs = all_data, data_res = dynamic_data, trait = "masec_kg_ha", plant = "Sorghum") {
  tmp_data_obs <- data_obs[which(data_obs$Trait == trait & data_obs$Plant == plant),]
  tmp_data_res <- data_res[which(data_res$Trait == trait & data_res$Plant == plant),]
  p <- ggplot(tmp_data_res) + geom_line(aes(x=Date, y=Sim, col=Syst)) + geom_point(data=tmp_data_obs, aes(x=Date, y=Obs, col=Syst)) + facet_wrap(. ~ Var + Eval + Site, scales="free", ncol=4) + ylab("Value") + ggtitle(paste0(paste0(plant, collapse=" "), " ", trait)) + geom_rect(data = subset(tmp_data_res, Eval == "Calib"), fill = NA, colour = "red", xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf)
  print(p)
}

boxplot_stics_eval <- function(data = all_data, trait = "masec_kg_ha", plant = "Sorghum") {
  tmp_data <- data[which(data$Trait == trait & data$Plant == plant),]
  p <- ggplot(data=tmp_data) + geom_boxplot(aes(x=N, y=Sim, fill=N)) + coord_flip() + facet_grid(Syst + Var ~ Eval) + ggtitle(paste0(paste0(plant, collapse=" "), " ", trait))
  print(p)
}

boxplot_stics_eval_value <- function(data = all_data, trait = "masec_kg_ha", plant = "Sorghum", scenario = "T") {
  tmp_data <- data[which(data$Trait == trait & data$Plant == plant),]
  if(scenario == "PP") {
    tmp_data[which(tmp_data$Eval == "Actual"),"EvalValue"] <- "100"
    tmp_data$EvalValue <- factor(tmp_data$EvalValue, levels=c("50","75","100","125","150"))
  } else if(scenario == "CO") {
    tmp_data[which(tmp_data$Eval == "Actual"),"EvalValue"] <- "407"
  }
  p <- ggplot(data=tmp_data, aes(x=EvalValue, y=Sim, fill=Syst)) + geom_boxplot(aes(x=EvalValue, y=Sim, fill=Syst)) + ylab(trait) + xlab(i) + facet_grid(N ~ Var) + ggtitle(plant) + stat_summary(
    fun.y = median,
    geom = 'line',
    aes(group = Syst, colour = Syst),
    position = position_dodge(width = 0.9) #this has to be added
  )
  print(p)
}

boxplot_stics_eval_syst <- function(data = all_data, trait = "masec_kg_ha", plant = "Sorghum", scenario = "T") {
  tmp_data_pur <- data[which(data$Trait == trait & data$Plant == plant & data$Syst == "pur"),]
  tmp_data_assoc <- data[which(data$Trait == trait & data$Plant == plant & data$Syst != "pur"),]
  p1 <- ggplot(data=tmp_data_pur) + geom_boxplot(aes(x=N, y=Sim, fill=N)) + coord_flip() + facet_grid(Syst + Var ~ Eval) + ggtitle(paste0(paste0(plant, collapse=" "), " ", trait))
  print(p1)
  p2 <- ggplot(data=tmp_data_assoc) + geom_boxplot(aes(x=N, y=Sim, fill=N)) + coord_flip() + facet_grid(Syst + Var ~ Eval) + ggtitle(paste0(paste0(plant, collapse=" "), " ", trait))
  print(p2)
}

boxplot_stics_eval_value_syst <- function(data = all_data, trait = "masec_kg_ha", plant = "Sorghum", scenario = "T") {
  tmp_data <- data[which(data$Trait == trait & data$Plant == plant),]
  if(scenario == "PP") {
    tmp_data[which(tmp_data$Eval == "Actual"),"EvalValue"] <- "100"
    tmp_data$EvalValue <- factor(tmp_data$EvalValue, levels=c("50","75","100","125","150"))
  } else if(scenario == "CO") {
    tmp_data[which(tmp_data$Eval == "Actual"),"EvalValue"] <- "407"
  }

  tmp_data_pur <- tmp_data[which(tmp_data$Syst == "pur"),]
  tmp_data_assoc <- tmp_data[which(tmp_data$Syst != "pur"),]

  p1 <- ggplot(data=tmp_data_pur, aes(x=EvalValue, y=Sim, fill=Syst)) + geom_boxplot(aes(x=EvalValue, y=Sim, fill=Syst)) + ylab(trait) + xlab(i) + facet_grid(N ~ Var) + ggtitle(plant) + stat_summary(
    fun.y = median,
    geom = 'line',
    aes(group = Syst, colour = Syst),
    position = position_dodge(width = 0.9) #this has to be added
  )
  print(p1)

  p2 <- ggplot(data=tmp_data_assoc, aes(x=EvalValue, y=Sim, fill=Syst)) + geom_boxplot(aes(x=EvalValue, y=Sim, fill=Syst)) + ylab(trait) + xlab(i) + facet_grid(N ~ Var) + ggtitle(plant) + stat_summary(
    fun.y = median,
    geom = 'line',
    aes(group = Syst, colour = Syst),
    position = position_dodge(width = 0.9) #this has to be added
  )
  print(p2)
}


### Old functions ###
boxplot_stics_old <- function(data = all_data, trait = "masec_kg_ha", plant = "Sorghum", divideby = "Var") {
  tmp_data <- data[which(data$Trait == trait & data$Plant == plant),]

  ## Construct dataframe for boxplot with respect to function arguments ##
  bp_data_obs <- data.frame()
  bp_data_res <- data.frame()
  for(i in unique(tmp_data[,divideby])) { #Add colomn to dataframe for each Var
    bp_data_obs <- cbind.all(bp_data_obs,as.numeric(tmp_data[which(tmp_data[,divideby] == i),"Obs"]))
    bp_data_res <- cbind.all(bp_data_res,as.numeric(tmp_data[which(tmp_data[,divideby] == i),"Sim"]))
  }
  colnames(bp_data_obs) <- unique(tmp_data[,divideby])
  colnames(bp_data_res) <- unique(tmp_data[,divideby])

  ## Draw boxplot ##
  par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
  boxplot(bp_data_obs, main = "Observations", ylim=c(0,max(c(unlist(bp_data_obs),unlist(bp_data_res)),na.rm=T)))
  boxplot(bp_data_res, main = "Simulations", ylim=c(0,max(c(unlist(bp_data_obs),unlist(bp_data_res)),na.rm=T)))
  mtext(paste0(paste0(plant, collapse=" "), " ", trait), outer = TRUE, cex = 1.5)
}
boxplot_stics_eval_old <- function(data = all_data, trait = "masec_kg_ha", plant = "Sorghum", divideby = "Var") {
  tmp_data <- data[which(data$Trait == trait & data$Plant == plant),]

  ## Construct dataframe for boxplot with respect to function arguments ##
  bp_data_obs <- tmp_data[which(tmp_data[,divideby] == "calib"),"Obs"]
  bp_data_res <- data.frame()
  for(i in unique(tmp_data[,divideby])) { #Add colomn to dataframe for each Var
    bp_data_res <- cbind.all(bp_data_res,as.numeric(tmp_data[which(tmp_data[,divideby] == i),"Sim"]))
  }
  names(bp_data_obs) <- "calib"
  colnames(bp_data_res) <- unique(tmp_data[,divideby])

  ## Draw boxplot ##
  par(mfrow=c(1,2), oma = c(0, 0, 2, 0))
  boxplot(bp_data_obs, main = "Observations", ylim=c(0,max(c(unlist(bp_data_obs),unlist(bp_data_res)),na.rm=T)))
  boxplot(bp_data_res, main = "Simulations", ylim=c(0,max(c(unlist(bp_data_obs),unlist(bp_data_res)),na.rm=T)))
  mtext(paste0(paste0(plant, collapse=" "), " ", trait), outer = TRUE, cex = 1.5)
}
boxplot_stics_eval_2 <- function(data = all_data, trait = "masec_kg_ha", plant = "Sorghum") {
  tmp_data <- data[which(data$Trait == trait & data$Plant == plant),]

  ## Temporary fix to add obs to boxlot ##
  colnames(tmp_data)[which(colnames(tmp_data) == "Sim")] <- "Value"
  obs <- tmp_data[which(tmp_data$Eval == "calib"),]
  obs$Eval <- "A.obs" # very temporary to put it at the beginning of the plot
  obs$Value <- obs$Obs
  tmp_data <- rbind(tmp_data,obs)
  tmp_data[which(tmp_data$Eval == "calib"),"Eval"] <- "B.sim" # very temporary to put it at the beginning of the plot

  p <- ggplot(data=tmp_data) + geom_boxplot(aes(x=Eval, y=Value, fill=Syst)) + facet_grid(Var ~ .) + ggtitle(paste0(paste0(plant, collapse=" "), " ", trait, " ", i))
  print(p)
}
boxplot_stics_eval_3 <- function(data = all_data, trait = "masec_kg_ha", plant = "Sorghum") {
  tmp_data <- data[which(data$Trait == trait & data$Plant == plant),]

  ## Temporary fix to add obs to boxlot ##
  colnames(tmp_data)[which(colnames(tmp_data) == "Sim")] <- "Value"
  obs <- tmp_data[which(tmp_data$Eval == "calib"),]
  obs$Eval <- "A.obs" # very temporary to put it at the beginning of the plot
  obs$Value <- obs$Obs
  tmp_data <- rbind(tmp_data,obs)
  tmp_data[which(tmp_data$Eval == "calib"),"Eval"] <- "B.sim" # very temporary to put it at the beginning of the plot

  p <- ggplot(data=tmp_data) + geom_boxplot(aes(x=Var, y=Value, fill=Eval)) + facet_grid(Syst ~ .) + ggtitle(paste0(paste0(plant, collapse=" "), " ", trait, " ", i))
  print(p)
}
