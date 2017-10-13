setClass(
  Class ="simulation",
  representation = representation(parameters = "list",       #the list of parameters for quantinemo
                                  sim.name="character",          # the name of the simulation is used for the input file name and the output file folder if not redefine
                                  sim.directory = "character",   #where is the executable and will the file be written
                                  exe.directory = "character",      #where is the executable
                                  exe.name = "character",     #what is the name of the executable
                                  params.file="list"     #A list containing all the parameters to be written in external file. Key is the name of the file, as defined by QN, and the value is a dataframe
                                  )
)

#' Build a minimal object for a QuantiNemo simulation
#' 
#' @param parameters The list of parameters that you want to pass to quantiNemo .
#' @param sim.directory The directory where you want to execute the simulation
#' @param sim.name Name of the simulation (input file and output folder)
#' @param exe.dir Where is the executable
#' @param exe.name Name of the executable
#' @return A simulation object ready to be run.
#' @examples
#' my_simulation <- new("simulation", parameters = (list(generations = 1000, patch_capacity =100)))
#' run(my_simulation)
#' 
setMethod(
  f ="initialize",
  signature ="simulation",
  definition = function(.Object,sim.name = "my_simulation",
                         parameters = list("generations" = 1000, "patch_capacity" = 100),
                         sim.dir="./",
                         exe.dir="",
                         exe.name="quantiNemo2"
                        ){
     .Object@sim.name <- sim.name
     .Object@sim.directory <- sim.dir
     .Object@parameters <- parameters
     .Object@exe.name <- exe.name
     if(exe.dir == ""){
       exe.dir <- path.package(package="RQuantiNemo")
     }
     .Object@exe.directory <- exe.dir
    return(.Object)
  }
)


setGeneric(name="run",
           def = function(object, verbose=TRUE){standardGeneric("run")}
)


#' Run a QuantiNemo simulation
#' @examples
#' my_simulation <- new("simulation")
#' run(my_simulation)
setMethod(f = "run",
          signature = "simulation",
          definition = function(object, verbose = TRUE){
            writeInput(object)
            comand <-  getCommand(object)
            message("excuting ", comand , "\n")
            if(verbose){
              result <- system2(comand[1],comand[2])
            }
            else{
              result <- system2(comand[1],comand[2],stdout = "output.log")
            }
            if(result & verbose){
              warning("The simulation failed to run \n")
            }
            return(result)
        }
)    



setGeneric(name="setParameter",
           def = function(object,name,value){standardGeneric("setParameter")}
)

#' Run a QuantiNemo simulation
#' @param name The name of the parameter that you want to set. It has to be a known parameter for QuantiNemo.
#' @param value The value of the parameter that you want to set. It can be a number if it's a simple parameter or a string for more complex parameter
#' @examples
#' my_sim <- new("simulation")
#' my_sim  <- setParameter(my_sim,"quanti_all",5)
#' run(my_simulation)
setMethod(f = "setParameter",
          signature = "simulation",
          definition= function(object, name, value){
            #object@parameters <- c(object@parameters, as.character(name)=value)
            object@parameters[[name]] = value
            return(object)
          }
)



setGeneric(name="addFile",
           def = function(object,file.name,file.content){standardGeneric("addFile")}
)

#' Run a QuantiNemo simulation
#' @param file.name The name of the file that you want to add, as defined by QuantiNemo
#' @param file.content A variable of dataframe type containing the needed information
#' @examples
#' my_sim <- new("simulation")
#' my_sim <- setParameter(my_sim,"quanti_all",5)
#' alleles <- data.frame(locus = rep(1,5),allele = seq(1,5),value = seq(-2,2))
#' my_sim <-  addFile(my_sim, "quanti_allelic_file",alleles)
#' run(my_sim)
setMethod(f = "addFile",
          signature = "simulation",
          definition= function(object,file.name,file.content){
            object@params.file[[file.name]] = file.content
            return(object)
          }
)


#' Run a QuantiNemo simulation
#' @param file.name The name of the file that you want to add, as defined by QuantiNemo
#' @param file.content A variable of dataframe type containing the needed information
#' @examples
#' my_sim <- new("simulation")
#' my_sim <- setParameter(my_sim,"quanti_all",5)
#' print(my_sim)


print.simulation <- function(object,...){
  cat(rep("-",40),"\n")
  cat("Parameters", "\n") 
  cat(rep("-",40),"\n")
  for (parameter in names(object@parameters)){
    cat(parameter, rep(" ",abs(28-nchar(parameter))), object@parameters[[parameter]],"\n", sep="")
  }    
  cat(rep("-",40),"\n")
  cat("Output folder & files", "\n") 
  cat(rep("-",40),"\n")
  cat("\"",object@sim.directory, object@sim.name, "\"","\n", sep="")
}
  

