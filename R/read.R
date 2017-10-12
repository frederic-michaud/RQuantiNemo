setGeneric(name="loadStat",
           def = function(object){standardGeneric("loadStat")}
)

#' Load the statistic of a QuantiNemo simulation
#' @examples
#' my_sim <- new("simulation")
#' my_sim <- setParameter(my_sim,"stat","adlt.ind")
#' run(my_sim)
#' my_sim <- loadStat(my_sim)
#' plot(my_sim@stat$adlt.ind)
setMethod(f = "loadStat",
          signature = "simulation",
          definition= function(object){
            object@stat <- read.table(paste(object@sim.directory, object@sim.name,"/simulation_mean.txt",sep=""),header = TRUE)
            return(object)
          }
)


setGeneric(name="loadPheno",
           def = function(object,generation = -1,replicate= 0){standardGeneric("loadPheno")}
)

#' Load the Phenotype of a QuantiNemo simulation
#' @examples
#' my_sim <- new("simulation")
#' my_sim <- setParameter(my_sim,"quanti_loci",1)
#' my_sim <- setParameter(my_sim,"quanti_save_phenotype",1)
#' run(my_sim)
#' my_sim <- loadPheno(my_sim)
#' table(my_sim@Pheno$V2)
setMethod(f = "loadPheno",
          signature = "simulation",
            definition= function(object,generation, replicate){
            post.info <- getPostInfo(object, generation, replicate)
            return(read.table(paste(object@sim.directory, object@sim.name,"/simulation",post.info,".phe",sep=""),skip=2))
          }
)