#' Load the statistic of a QuantiNemo simulation
#' @examples
#' my_sim <- new("simulation")
#' my_sim <- setParameter(my_sim,"stat","adlt.ind")
#' run(my_sim)
#' stat <- loadStat(my_sim)
#' plot(stat$adlt.ind)
#' @export
setGeneric(name="loadStat",
           def = function(object){standardGeneric("loadStat")}
)

setMethod(f = "loadStat",
          signature = "simulation",
          definition= function(object){
            return(read.table(paste(object@sim.directory, object@sim.name,"/simulation_mean.txt",sep=""),header = TRUE))
          }
)


#' Load the Phenotype of a QuantiNemo simulation
#' @param generation The generation from which we want to load the data. A Negative number mean starting from the end. 
#' @param replicate The replicate from which we want to load the data. A value of 0 mean that there is only one replicate. 
#' @examples
#' my_sim <- new("simulation")
#' my_sim <- setParameter(my_sim,"quanti_loci",1)
#' my_sim <- setParameter(my_sim,"quanti_save_phenotype",1)
#' run(my_sim)
#' pheno <- loadPheno(my_sim)
#' table(pheno$V2)
#' @export
setGeneric(name="loadPheno",
           def = function(object,generation = -1,replicate= 0){standardGeneric("loadPheno")}
)

setMethod(f = "loadPheno",
          signature = "simulation",
            definition= function(object,generation, replicate){
            post.info <- getPostInfo(object, generation, replicate)
            nb.trait <- getParameter(object, "quanti_nb_trait", default = 1 )
            return(read.table(paste(object@sim.directory, object@sim.name,"/simulation",post.info,".phe",sep=""),skip=1 + nb.trait))
          }
)

#' Load the Genotype of a QuantiNemo simulation
#' @param generation The generation from which we want to load the data. A Negative number mean starting from the end. 
#' @param replicate The replicate from which we want to load the data. A value of 0 mean that there is only one replicate. 
#' @examples
#' my_sim <- new("simulation")
#' my_sim <- setParameter(my_sim,"quanti_loci",1)
#' my_sim <- setParameter(my_sim,"quanti_save_genotype",1)
#' run(my_sim)
#' geno <- loadGeno(my_sim)
#' table(geno$V2)
#' @export

setGeneric(name="loadGeno",
           def = function(object,generation = -1,replicate= 0){standardGeneric("loadGeno")}
)

setMethod(f = "loadGeno",
          signature = "simulation",
          definition= function(object,generation, replicate){
            post.info <- getPostInfo(object, generation, replicate)
            nb.trait = getParameter(object, "quanti_nb_trait", default = 1) + getParameter(object, "ntrl_nb_trait", default = 0)
            nb.loci =  getParameter(object, "quanti_loci", default = 0) + getParameter(object, "ntrl_loci", default = 0)
           
            return(read.table(paste(object@sim.directory, object@sim.name, "/simulation",post.info,".dat",sep=""),
                              skip=1 + nb.trait*nb.loci,
                              ,colClasses=c("character")
                              ))
          }
)



#' Load the statistic for various replicate of a QuantiNemo simulation
#' @examples
#' my_sim <- new("simulation")
#' my_sim <- setParameter(my_sim,"stat","adlt.ind")
#' run(my_sim)
#' stat.r <- loadStatRep(my_sim,"stat","adlt.ind")
#' for (i in 1:10){
#'   plot(stat.r$adlt.ind[stat.r$replicate==i])
#'  }
#'  @export
setGeneric(name="loadStatRep",
           def = function(object){standardGeneric("loadStatRep")}
)

setMethod(f = "loadStatRep",
          signature = "simulation",
          definition= function(object){
            return(read.table(paste(object@sim.directory, object@sim.name,"/simulation_stats.txt",sep=""),header = TRUE))
          }
)


#' Load the statistic (mean) of a QuantiNemo simulation for each patch and return it as a matrix
#' @param stat.name String representing the name of the statisitic to load
#' @examples
#' parameters = list("generations" = 5,
#'                   "patch_capacity" = 100,
#'                   "patch_number" = 1000,
#'                   "stat" = "{adlt.nbInd_p}",
#'                   "patch_ini_size", "{seq(1,1000,1000)}")
#' my_sim.base = new("simulation", parameters = parameters)
#' run(my_sim, verbose =FALSE)
#' plot(loadStatPatch(my_sim, "adlt.nbInd_p")[1, ])
#' @export
setGeneric(name="loadStatPatch",
           def = function(object, stat.name){standardGeneric("loadStatPatch")}
)


setMethod(f = "loadStatPatch",
          signature = "simulation",
          definition= function(object, stat.name){
            stat <- loadStat(object)
            if(!("patch_number" %in% names(object@parameters))){
              stop("This simulation has only one patch")
            }
            n.patch = object@parameters$patch_number
            width = floor(log10(n.patch))+1
            n.generation = max(stat$generation)
            stat.patch = matrix(0, nrow = n.patch, ncol = n.generation)
            for (patch in 1:n.patch){
              patch.name = paste(stat.name,formatC(patch, width = width,  flag = "0"),sep="")
              stat.patch[patch, ] <- stat[[patch.name]]
            }
            return(t(stat.patch))
          }
)
