###################################################################################
pkgload::load_all()
data(SEAex)
dai<-set_pbox(SEAex[,.(Malaysia,Thailand,Vietnam,avgRegion)])
print(dai)

#allParms<-dai@copula@paramMargins
#names(allParms)<-names(dai@data)

#+ assume that every paramter D of the distribution comes itself from a Normal distribution
#+  with (mu=D, sigma=0.05). We are interest  to query the probabilistic space under
#+  different scenarios with different combinations of parameters for a single query. To do so we shift every parameter
#+  by 1 2 and 3 standard deviation above and below the observed values generating several hypothetical copula and query
#+  the grid a all possible combinations of parameters. The only unchanged paramter is the copula parmeter.
#+  We then calculate the difference

# Compute values for mean ± 1, 2, 3 standard deviations
parameter_deviations<-function(param, sigma=0.05, range=seq(-3,3,1)){
param + range * sigma
}

##########################################################

modify_pbox <- function(all_parms, parameters_to_modify, sigma=sigma, range=range) {
  # Deep copy the original list to preserve non-modified elements
  modified_parms <- rapply(all_parms, identity, how = "replace")

  # Iterate over each country and its parameters to modify
  for (colvar in names(parameters_to_modify)) {
    param_names <- parameters_to_modify[[colvar]]
    # Ensure param_names is a list for uniform processing
    if (is.character(param_names)) {
      param_names <- as.list(param_names)
    }

    # Apply deviations to specified parameters
    for (param_name in param_names) {
      if (!is.null(modified_parms[[colvar]][[param_name]])) {
        # Calculate the deviation and update the parameter in the copied list
        modified_parms[[colvar]][[param_name]] <- parameter_deviations(modified_parms[[colvar]][[param_name]], sigma=sigma, range=range)
      }
    }
  }

  # Return the modified list which includes unchanged parts of the original list
  return(modified_parms)
}


###############################################################################


# Function to generate scenarios
gen_scenario<- function(params) {
  # Determine the maximum length from the parameters
  max_len <- max(unlist(map_depth(params,2,length)))

  # Create a list to hold all scenarios
  scenarios <- vector("list", max_len)

  # Initialize each scenario with deep copies of the original structure
  for (i in seq_len(max_len)) {
    scenarios[[i]] <- lapply(params, function(varname) {
      lapply(varname, function(param) {
        # Repeat the value max_len times if it's singular, otherwise pick the i-th value
        #if (length(param) == 1) return(param)
        #param[i %% length(param)+1]
        ifelse(length(param) == 1,param,param[i])

      })
    })
  }

  return(scenarios)
}


scenario_pbox<-function(pbx,param_list="list",sigma=0.05, range=seq(-3,3,1), ...){
  allParms<-pbx@copula@paramMargins
  names(allParms)<-names(pbx@data)
  deviation_results<-modify_pbox(all_parms =allParms,parameters_to_modify = param_list,sigma, range)
  scenarios<-gen_scenario(deviation_results)


  scenario_res<-lapply(scenarios,function(x){

    perCop<-copy(pbx)
    perCop@copula@paramMargins<-unname(x)
    qpbox(perCop,...)
  })

  return(scenario_res)
}


#lapply(resEx, function (x) x-resEx[[4]])

######

scenario_pbox(dai,mj = "Vietnam:31 & avgRegion:26", param_list = list(Vietnam="mu"))




