#' get the command to be run
#' @examples
#' my_simulation <- new("simulation")
#' system(getCommand(my_simulation))

setGeneric(name="getCommand",
           def = function(object){
             standardGeneric("getCommand")
           }
)

setMethod(f = "getCommand",
          signature = "simulation",
          definition= function(object){
          file.name = paste(" ",object@sim.directory,object@sim.name,".ini",sep="")
          exe = paste(object@exe.directory,object@exe.name,sep="/")
          return(c(exe, file.name))
          }
)


#' Print all the input file for a QuantiNemo simulation
#' @examples
#' my_simulation <- new("simulation")
#' writeInput(my_simulation)
#' @export
setGeneric(name="writeInput",
           def = function(object, verbose = TRUE){
             standardGeneric("writeInput")
           }
)
setMethod(f = "writeInput",
          signature = "simulation",
          definition= function(object, verbose = TRUE){
          if(!dir.exists(object@sim.directory)){
             dir.create(object@sim.directory)
          }
           writeMainFile(object, verbose = verbose)
           writeGenoFile(object)
           writeDataframeFiles(object)
          }
)

#' Print the main input file for the QuantiNemo simulation
#' @examples
#' my_simulation <- new("simulation")
#' writeMainFile(my_simulation)
setGeneric(name="writeMainFile",
           def = function(object, verbose = TRUE){
             standardGeneric("writeMainFile")
           }
)

setMethod(f = "writeMainFile",
          signature = "simulation",
          definition= function(object, verbose = TRUE){
            file.name = paste(object@sim.directory,object@sim.name,".ini",sep="")
            if(verbose) {message(c("printing into :",file.name, sep = ""))}
            sink(file.name)
            #printing output folder
            cat(paste("folder",paste(object@sim.directory, object@sim.name,sep=""),"\n",sep = "\t \t"))
            #printing common parameters
            for(param in names(object@parameters))
            {
              cat(paste(param,object@parameters[[param]],"\n",sep = "\t \t"))
            }
            for(file in names(object@params.file))
            {
              cat(paste(file,paste('"',paste(file,".ini",sep=""),'"',sep=""),"\n",sep = "\t \t"))
            }
            sink()
          
          }
)

#' Print all the input files which come from a data frame
#' @examples
#' my_simulation <- new("simulation")
#' writeDataframeFile(my_simulation)
setGeneric(name="writeDataframeFiles",
           def = function(object){
             standardGeneric("writeDataframeFiles")
           }
)
setMethod(f = "writeDataframeFiles",
          signature = "simulation",
          definition= function(object){
            for(file in names(object@params.file)){
              if(file=="quanti_ini_genotypes" | file=="ntrl_ini_genotypes"){
                next
              }
              df = object@params.file[[file]]
              sink(paste(object@sim.directory,file,".ini",sep=""))
              cat("[FILE_INFO]{\n")
              params.name = colnames(df)
              nb.params = length(params.name)
              for(col in 1:nb.params){
                cat(paste(params.name[col]),col,"\n",sep= "\t \t")
              }
              cat("} \n")
              for(i in 1:nrow(df)){
                cat(as.matrix(df[i, ]))
                cat("\n")
              }
              sink()
            }
          }
)


#' Print the genotype file
#' @examples
#' my_simulation <- new("simulation")
#' writeMainFile(my_simulation)
setGeneric(name="writeGenoFile",
           def = function(object){
             standardGeneric("writeGenoFile")
           }
)

setMethod(f = "writeGenoFile",
          signature = "simulation",
          definition= function(object){
            #printing genotype file
            for(file in names(object@params.file))
            {
              if(file=="quanti_ini_genotypes" | file=="ntrl_ini_genotypes")
              {
                pre = substr(file, 1, nchar(file)-14) #either quanti or ntrl
                file.name = paste(object@sim.directory,file,".ini",sep="")
                sink(file.name)
                ini.size <- getParameter(object, "ini_size", default = 0)
                if(ini.size ==0) {ini.size <- getParameter(object, "patch_capacity", default = 1)}
                cat(getParameter(object, "patch_number", default = 1),
                    getParameter(object, paste(pre, "_loci", sep = ""), default = 1),
                    ini.size * getParameter(object, "patch_number", default = 1),
                    floor(log10(getParameter(object, paste(pre, "_all", sep = ""), default = 255))) + 1,
                    "\n")
                #cat("1 2 2 1 \n")
                for(trait in 1:getParameter(object, paste(pre, "_nb_trait", collapse = ""), default = 1)){
                  for(allele in 1:getParameter(object, paste(pre, "_loci", collapse = ""), default = 1)){
                    cat("n",as.character(trait),"_l",as.character(allele), "\n", sep = "")
                  }
                }
                df = object@params.file[[file]]
                for(i in 1:nrow(df)){
                  cat(as.matrix(df[i, ]))
                  cat("\n")
                }
                sink()             
              }
            }
          }
)


#' Get the last part of a filename that changes through generation and replicate
#' @param generation The generation from which we want to load the data. A Negative number mean starting from the end. 
#' @param replicate The replicate from which we want to load the data. A value of 0 mean that there is only one replicate. 
#' @examples
#' my_sim <- new("simulation")
#' getPostInfo(my_sim, generation = 10, replicate = 1)
setGeneric(name="getPostInfo",
           def = function(object, generation = -1, replicate = 0){
             standardGeneric("getPostInfo")
           }
)

setMethod(f = "getPostInfo",
          signature = "simulation",
          definition= function(object, generation = -1, replicate = 0 ){
            width.gen <- floor(log10(object@parameters$generations))+1
            if(generation < 1){
              generation <-  object@parameters$generations + generation + 1
            }
            value.g = paste("_g",formatC(generation, width = width.gen,  flag = "0"),sep="")
            
            value.r <- ""
            if(replicate > 0){
             width.rep <- floor(log10(object@parameters$replicates))+1
             value.r = paste("_r",formatC(replicate, width = width.rep,  flag = "0"),sep="")
            }
            return(paste(value.g, value.r,sep=""))
          }
)


#' Get the value of a parameter if it exist or the default value otherwise
#' @param parameter the parameter which we want to extract
#' @param default The default value which will be return in case the parameter does not exist
#' @examples
#' my_sim <- new("simulation", parameters = list("generations" = 100)
#' get_parameter(my_sim, "generations") # return 100
#' get_parameter(my_sim, "patch_number") # return 0
#' get_parameter(my_sim, "patch_number", default = 1) # return 1
setGeneric(name="getParameter",
           def = function(object, parameter, default = 0){
             standardGeneric("getParameter")
           }
)

setMethod(f = "getParameter",
          signature = "simulation",
          definition= function(object, parameter, default = 0 ){
            if (parameter %in% names(object@parameters)){
              value <- object@parameters[[parameter]]
            }
            else{
              value <- default
            }
            return(value)
          }
)

