#' @export
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
#' my_simulation <- new("simulation", parameters = (list(generations = 1000, patch_capacity =100)), sim.name = "my_new_sim")
#' @export
setMethod(
  f ="initialize",
  signature ="simulation",
  definition = function(.Object,sim.name = "my_simulation",
                         parameters = list("generations" = 1000, "patch_capacity" = 100),
                         sim.dir="./",
                         exe.dir="",
                         exe.name=NULL
                        ){
    if(is.null(exe.name))
    {
      os <- Sys.info()["sysname"]
      if(os == "Linux"){
        exe.name = "quantinemo.linux"
      }
      else if(os == "Darwin"){
        exe.name = "quantinemo.mac"
      }
      else if(os == "Windows"){
        exe.name = "quantiNemo.exe"
      }
      else{
        message("sorry, I was not able to find which executable to use on your system. Please use exe.name to help me.")
      }
      
    }
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


#' Run a QuantiNemo simulation
#' @param verbose If we want to display information about the simulation and the standard output of QuantiNemo. 
#' @examples
#' my_simulation <- new("simulation")
#' run(my_simulation)
#' @export
setGeneric(name="run",
           def = function(object, verbose=TRUE){standardGeneric("run")}
)


setMethod(f = "run",
          signature = "simulation",
          definition = function(object, verbose = TRUE){
            writeInput(object, verbose = verbose)
            comand <-  getCommand(object)
            if (verbose){message("excuting ", comand , "\n")}
            if(verbose){
              result <- system2(comand[1],comand[2])
            }
            else{
              result <- system2(comand[1],comand[2],stdout = "output.log")
            }
            if(result){
              warning("The simulation failed to run \n")
            }
            return(result)
        }
)    



#' Add a new parameter to a quantiNemo simulation. 
#' @param name The name of the parameter that you want to set. It has to be a known parameter for QuantiNemo.
#' @param value The value of the parameter that you want to set. It can be a number if it's a simple parameter or a string for more complex parameter
#' @examples
#' my_sim <- new("simulation")
#' my_sim  <- setParameter(my_sim,"patch_number",5)
#' my_sim  <- setParameter(my_sim,"patch_ini_size","{10 100 10 100 67}")
#' @export
setGeneric(name="setParameter",
           def = function(object,name,value){standardGeneric("setParameter")}
)

setMethod(f = "setParameter",
          signature = "simulation",
          definition= function(object, name, value){
            object@parameters[[name]] = value
            return(object)
          }
)


#' Add an input file  to quantiNemo. 
#' @param file.name The name of the file that you want to add, as defined by QuantiNemo
#' @param file.content A dataframe type containing the needed information. The name of the Column should be the one given by Quantinemo
#' @examples
#' my_sim <- new("simulation")
#' my_sim <- setParameter(my_sim,"quanti_all",5)
#' alleles <- data.frame(locus = rep(1,5),allele = seq(1,5),value = seq(-2,2))
#' my_sim <-  addFile(my_sim, "quanti_allelic_file",alleles)
#' run(my_sim)
#' @export
setGeneric(name="addFile",
           def = function(object,file.name,file.content){standardGeneric("addFile")}
)


setMethod(f = "addFile",
          signature = "simulation",
          definition= function(object,file.name,file.content){
            object@params.file[[file.name]] = file.content
            return(object)
          }
)


#' Display basic information about a quantiNemo simulation
#' @param file.name The name of the file that you want to add, as defined by QuantiNemo
#' @param file.content A variable of dataframe type containing the needed information
#' @examples
#' my_sim <- new("simulation", sim.name = "test72")
#' my_sim <- setParameter(my_sim,"quanti_all",5)
#' print(my_sim)
#' @export

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
  

