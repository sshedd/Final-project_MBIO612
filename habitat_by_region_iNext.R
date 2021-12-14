habitat_by_region_iNext <-
function(){
  subregion_subset <- function(x,sub.region){
    subreg <- subset(All_HI_2, Sub.Region==x, drop = F)
  }
  All_HI_2[,"Habitat2"] <- as.numeric(as.factor(All_HI_2[,"Habitat2"]))
  subregion_subset("Northwestern Hawaiian Islands", "NWHI")
  subregion_subset("Main Hawaiian Islands","MHI")
  subregion_subset("Exclusive Economic Zone around Hawaiian Islands", "EEZ")
  
  NWHI_iNext_Habitat_data <- function(){
    NWHI[,c(1,8,29)] %>%
      pivot_wider(names_from =Habitat2, 
                  values_from ="IndividualCount", 
                  values_fn = length,
                  values_fill = 0
      ) %>%
      data.frame() 
  }
  NWHI_iNext_Habitat <- NWHI_iNext_Habitat_data()
  
  #organize data into matrix to put into iNext
  NWHI_iNext_Habitat_2 <- as.matrix(apply(NWHI_iNext_Habitat[,-1],2,as.integer))
  
  #must add species name for row names 
  row.names(NWHI_iNext_Habitat_2) <- NWHI_iNext_Habitat[,1]
  
  NWHI_habitat_rare <- iNEXT(NWHI_iNext_Habitat_2, q=0, datatype="abundance", size=NULL, endpoint=NULL, knots=40,
                             se=TRUE, conf=0.95, nboot=50)
  
  NWHI_habitat_rare <- ggiNEXT(NWHI_habitat_rare)+
    theme_bw(base_size = 7)+
    ggtitle("NWHI")
  
  
  #iNext the MHI by feature 
  MHI_iNext_Habitat_data <- function(){
    MHI[,c(1,8,29)] %>%
      pivot_wider(names_from =Habitat2, 
                  values_from ="IndividualCount", 
                  values_fn = length,
                  values_fill = 0
      ) %>%
      data.frame() 
  }
  MHI_iNext_Habitat <- MHI_iNext_Habitat_data()
  
  #organize data into matrix to put into iNext
  MHI_iNext_Habitat_2 <- as.matrix(apply(MHI_iNext_Habitat[,-1],2,as.integer))
  
  #must add species name for row names 
  row.names(MHI_iNext_Habitat_2) <- MHI_iNext_Habitat[,1]
  
  MHI_habitat_rare <- iNEXT(MHI_iNext_Habitat_2, q=0, datatype="abundance", size=NULL, endpoint=NULL, knots=40,
                            se=TRUE, conf=0.95, nboot=50)
  
  MHI_habitat_rare <- ggiNEXT(MHI_habitat_rare)+
    theme_bw(base_size = 7)+
    ggtitle("MHI")
  
  #EEZ iNext by feature
  #iNext the NWHI by feature 
  EEZ_iNext_Habitat_data <- function(){
    EEZ[,c(1,8,29)] %>%
      pivot_wider(names_from =Habitat2, 
                  values_from ="IndividualCount", 
                  values_fn = length,
                  values_fill = 0
      ) %>%
      data.frame() 
  }
  EEZ_iNext_Habitat <- EEZ_iNext_Habitat_data()
  
  #organize data into matrix to put into iNext
  EEZ_iNext_Habitat_2 <- as.matrix(apply(EEZ_iNext_Habitat[,-1],2,as.integer))
  
  #must add species name for row names 
  row.names(EEZ_iNext_Habitat_2) <- EEZ_iNext_Habitat[,1]
  
  EEZ_habitat_rare <- iNEXT(EEZ_iNext_Habitat_2, q=0, datatype="abundance", size=NULL, endpoint=NULL, knots=40,
                            se=TRUE, conf=0.95, nboot=50)
  
  EEZ_habitat_rare<- ggiNEXT(EEZ_habitat_rare)+
    theme_bw(base_size = 7)+
    ggtitle("EEZ")
  
  grid.arrange(NWHI_habitat_rare, MHI_habitat_rare, EEZ_habitat_rare, nrow=3)
}
