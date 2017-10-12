setGeneric(name="getCommand",
           def = function(object){
             standardGeneric("getCommand")
           }
)


#' get the command to be run
#' @examples
#' my_simulation <- new("simulation")
#' system(getCommand(my_simulation))
setMethod(f = "getCommand",
          signature = "simulation",
          definition= function(object){
          file.name = paste(" ",object@sim.directory,object@sim.name,".ini",sep="")
          exe = paste(object@exe.directory,object@exe.name,sep="/")
          return(c(exe, file.name))
          }
)

setGeneric(name="writeInput",
           def = function(object){
             standardGeneric("writeInput")
           }
)

#' Print all the input file for a QuantiNemo simulation
#' @examples
#' my_simulation <- new("simulation")
#' writeInput(my_simulation)
setMethod(f = "writeInput",
          signature = "simulation",
          definition= function(object){
          if(!dir.exists(object@sim.directory)){
             dir.create(object@sim.directory)
          }
           writeMainFile(object)
           writeGenoFile(object)
           writeDataframeFiles(object)
          }
)

setGeneric(name="writeMainFile",
           def = function(object){
             standardGeneric("writeMainFile")
           }
)

#' Print the main input file for the QuantiNemo simulation
#' @examples
#' my_simulation <- new("simulation")
#' writeMainFile(my_simulation)
setMethod(f = "writeMainFile",
          signature = "simulation",
          definition= function(object){
            file.name = paste(object@sim.directory,object@sim.name,".ini",sep="")
            message(c("printing into :",file.name, sep = ""))
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
              cat(paste(file,paste('"',file,'"',sep=""),"\n",sep = "\t \t"))
            }
            sink()
          
          }
)


setGeneric(name="writeDataframeFiles",
           def = function(object){
             standardGeneric("writeDataframeFiles")
           }
)

#' Print all the input files which come from a data frame
#' @examples
#' my_simulation <- new("simulation")
#' writeDataframeFile(my_simulation)
setMethod(f = "writeDataframeFiles",
          signature = "simulation",
          definition= function(object){
            for(file in names(object@params.file))
            {
              if(file=="quanti_ini_genotypes"){
                next
              }
              df = object@params.file[[file]]
              sink(file)
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


setGeneric(name="writeGenoFile",
           def = function(object){
             standardGeneric("writeGenoFile")
           }
)

#' Print the genotype file
#' @examples
#' my_simulation <- new("simulation")
#' writeMainFile(my_simulation)
setMethod(f = "writeGenoFile",
          signature = "simulation",
          definition= function(object){
            #printing genotype file
            for(file in names(object@params.file))
            {
              if(file=="quanti_ini_genotypes")
              {
                print(paste(c("printing into :",file), sep = ""))
                sink(file)
                cat ("1 1 2 1 \n")
                cat("n1_l1 \n")
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



setGeneric(name="getPostInfo",
           def = function(object, generation = -1, replicate = 0){
             standardGeneric("getPostInfo")
           }
)
#' Get the last part of a filename that changes through generation and replicate
#' @param generation The generation from which we want to load the data. A Negative number mean starting from the end. 
#' @param replicate The replicate from which we want to load the data. A value of 0 mean that there is only one replicate. 
#' @examples
#' my_sim <- new("simulation")
#' getPostInfo(my_sim, generation = 10, replicate = 1)
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