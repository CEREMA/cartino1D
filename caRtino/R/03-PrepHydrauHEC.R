#' Write hec-ras project
#' @noRd 
HEC_RAS_Project = function(dir_prj, name)                                       
  # LPo : création du fichier de projet HECRAS (.prj) + écriture
{  
  template = file.path(dir_tmp_hecras, "template.prj")
  x = readLines(template)
  y = gsub("Proj Title=", paste0("Proj Title=", name), x )
  cat(y, file=file.path(dir_prj, paste0(name, ".prj")), sep="\n")
}

#' Write hec-ras plan                                                           
#' # LPo : Création du fichier plan HECRAS (.p01) + écriture
#' @noRd 
HEC_RAS_Plan = function(dir_prj, name)
{
  template = file.path(dir_tmp_hecras, "template.p01")
  x = readLines(template)
  y = gsub("Plan Title=", paste0("Plan Title=", name), x )
  cat(y, file=file.path(dir_prj, paste0(name, ".p01")), sep="\n")
}

#' Write hec-ras flow                                                           
#' # LPo : Création du fichier flow HECRAS (.f01) + écriture
#' @noRd 
HEC_RAS_Flow = function(Bief_Q, slope_val, name, Profil_name,
                        Reach_name, dir_prj)
{
  Flow_name = file.path(dir_prj, paste0(name, ".f01"))
  River_name = name
  
  write(paste("Flow Title=", name, "\n",
              paste0("Program Version=", version_hecras), "\n",                  # LPo : version d'HECRAS utilisée
              "Number of Profiles= ", 1, "\n",
              "Profile Names=", Profil_name, sep=""),
        file=Flow_name)
  nQ = length(Bief_Q)
  
  for(k in 1:length(Bief_Q))
  {
    debu=paste0("River Rch & RM=", substr(name,1,16), ",", Reach_name)
    for (ib in (nchar(debu):47)){debu=paste0(debu," ")}
    write(paste0(debu,
                 ",", nQ-k+1, "\n",
                 formatC(Bief_Q[k], digits=3, width=8, format="f")),
          file=Flow_name, append=T)
  }
  
  write(paste("Boundary for River Rch & Prof#=",                                
              substr(name,1,16), ",", Reach_name,
              "               , ", 1, "\n",
              "Up Type= 3\n",
              "Up Slope=", slope_val, "\n",
              "Dn Type= 3\n", 
              "Dn Slope=", slope_val, "\n",
              "DSS Import StartDate=\n",
              "DSS Import StartTime=\n",
              "DSS Import EndDate=\n",
              "DSS Import EndTime=\n",
              "DSS Import GetInterval= 0\n" ,
              "DSS Import Interval=\n",
              "DSS Import GetPeak= 0\n",
              "DSS Import FillOption= 0\n",sep=""),
        file=Flow_name, append=T)      
  
}

#' Write hec-ras geometry - part 1                                              
#' # LPo : création du fichier géométrie (partie 1) (.g01) + écriture
#' @noRd 
Hec_RAS_Geom_Part1 = function(df_bief, dir_prj, name_run)
{
  
  geom_name = file.path(dir_prj, paste0(name_run,".g01"))
  write(paste("Geom Title=", name_run, "\n",
              paste0("Program Version=", version_hecras), sep=""),
        file=geom_name)
  Reach_name = "Channel"
  extent_test = df_bief[c(1, nrow(df_bief)), 1:2]
  write(paste("Viewing Rectangle=     ",
              (extent_test$X[1])," ,     ",
              (extent_test$Y[1])," ,     ",
              (extent_test$X[2])," ,     ",
              (extent_test$Y[2]), sep=""),
        file=geom_name, append=T)
  
  ## reach geometry
  limits = df_bief[, 1:2]
  names(limits) = c("x", "y")
  pt_riv = nrow(limits)
  write(paste("\nRiver Reach=", name_run, ",", Reach_name,
              "\nReach XY= ", pt_riv, sep=""),
        file=geom_name, append=T)
  
  ## four columns table
  tmp = rep(limits$y, each=2)
  tmp[seq(1, length.out=length(limits$x), by=2)] = limits$x
  tmp = formatC(tmp, digits=4, width=16, format="f")
  tab = data.frame(matrix(tmp, ncol=4, byrow=T), stringsAsFactors=F)
  tab = apply(tab, 1, paste, collapse="")
  write(tab, file=geom_name, append=T)
  
  ## center point of the reach
  moyX = mean(limits$x)
  moyY = mean(limits$y)
  tmp = paste(formatC(c(moyX, moyY), digits=3, width=8, format="f"), collapse=",")
  write(paste("\nRch Text X Y= ", tmp,
              "\nReverse River Text= 0\n", sep=""),
        file=geom_name, append=T)
  
  return(geom_name)
}

#' Write hec-ras geometry - part 2                                              
#' # LPo : écriture de la partie 2 du fichier géométrie (.g01)      
#' @noRd
HEC_RAS_Geom_Part2 = function(Begining, k, Profil_MNT, vec_dist, Manning)
{
  
  n_pt = nrow(Profil_MNT)
  
  ## limitation of the number of points hec-ras can handle
  if(n_pt > 500)
  {
    pt = Profil_MNT$Z; dist = Profil_MNT$POSI; posi = Profil_MNT$IPOSI
    npt = n_pt; dz=0
    i_out = 0
    while (length(pt) > 500)
    {
      dz = dz + 0.01
      inc = 1
      while (inc < (length(pt)-2))
      {
        delta = 
          pt[inc+1] - pt[inc] + 
          (pt[inc+2] - pt[inc]) / (dist[inc+2]-dist[inc]) *(dist[inc+1]-dist[inc]);
        if (abs(delta) < dz)
        {
          i_out = c(i_out, posi[inc+1])
          pt = pt[-(inc+1)]; dist = dist[-(inc+1)]; posi = posi[-(inc+1)]
        } else inc=inc+1
      }
    }
    Profil_MNT = Profil_MNT[-i_out, ]; n_pt = nrow(Profil_MNT)
  }
  
  ## write for each new line
  tmp = vec_dist
  write(paste0("Type RM Length L Ch R = 1 ,", k,
               "      ,    ", tmp, ",   ", tmp, ",    ", tmp), Begining, append=T)
  write(paste0("XS GIS Cut Line=", n_pt), Begining, append=T)
  
  ## four columns table
  tmp = rep(Profil_MNT$Y, each=2)
  tmp[seq(1, length.out=length(Profil_MNT$X), by=2)] = Profil_MNT$X
  tmp = formatC(tmp, digits=3, width=16, format="f")
  tab = data.frame(matrix(tmp, ncol=4, byrow=T), stringsAsFactors=F)
  tab = apply(tab, 1, paste, collapse="")
  write(tab, Begining, append=T)
  
  ## write for the elevation data
  write(paste("#Sta/Elev= ", n_pt, sep=""), Begining, append=T)
  
  ## ten columns table with position - elevation data
  tmp = rep(Profil_MNT$Z, each=2)
  tmp[seq(1, length.out=length(Profil_MNT$POSI), by=2)] = Profil_MNT$POSI
  tmp = formatC(tmp, digits=2, width=8, format="f")
  tab = data.frame(matrix(tmp, ncol=10, byrow=T))
  tab = apply(tab, 1, paste, collapse="")
  write(tab, Begining, append=T)
  
  ## write friction values - here same values
  write(paste("#Mann=",3,",",0,",",0,sep=" "), Begining, append=T)
  
  ## find bank positions - here first and last point
  Bank1 = format(Profil_MNT$POSI[1], digits=2, nsmall=2)
  Bank2 = format(Profil_MNT$POSI[nrow(Profil_MNT)], digits=2, nsmall=2)
  tmp = c(0, Manning, 0, Bank1, Manning, 0, Bank2, Manning, 0)
  tmp = formatC(tmp, digits=3, width=8, format="f")
  
  ## write bank positions and friction values
  write(paste(tmp, collapse=""), Begining, append=T)
  write(paste0("Bank Sta=", Bank1, ",", Bank2), Begining, append=T)
  write(paste0("XS Rating Curve= 0 ,0\n",
               "Exp/Cntr=0.3,0.1\n"), Begining, append=T)
}


#' @title Lancement du modèle hydraulique HEC-RAS 
#' @description
#' ENG : Run hydraulic model hec-ras                                                                        
#' 
#' FR : Cette fonction appartient au script 03-PrepHydrauHEC.R
#' 
#' Création et écriture des fichiers nécessaires au bon fonctionnement d'HEC-RAS :
#' 
#' - Fichier projet (.prj) ;
#' - Fichier plan (.p01) ;
#' - Fichier flux (.f01) ;
#' - Fichier géométrie partie 1 puis partie 2 (.g01).
#' 
#' Stocke les résultats dans la variable Bief_results_final.
#' 
#' @param df_bief la fonction s'applique sur la géométrie des biefs / reach geometry
#' @param rep_res la fonction s'applique sur le dossier résultats / results directory
#' @param name_run la fonction s'applique sur la variable nom qui comprend le débit + le bief + le BV étudié / run name
#' @param Bief_MNT_Iter la fonction s'applique sur le tableau de données avec l'altitude pour chaque profil en travers 
#' / dataframe with elevation data for each section
#' @param Bief_STA la fonction s'applique sur le tableau de données avec les caractéristiques de chaque profil en travers 
#' / dataframe with info for each section
#' @return la fonction retourne un tableau de données avec les résultats hydrauliques à chaque profil en travers 
#' / dataframe with results for each section

run_hydrau_hecras = function(df_bief, rep_res, name_run, Bief_MNT_Iter, Bief_STA)           # LPo : lancement d'HECRAS
{
  ## some parameters and constant values for hec-ras
  Profil_name = hecras_profil_name
  Reach_name = hecras_reach_name
  Manning = round(1/Strick[1], digits=3)
  #Manning = Manning_hecras[i_coeff]
  slope_val = hecras_slope_val
  
  ## create some input files
  dir_prj = file.path(rep_resu, rep_hecras); dir.create(dir_prj, recursive=T, showWarnings=F)
  HEC_RAS_Project(dir_prj, name_run)
  HEC_RAS_Plan(dir_prj, name_run)
  
  ## write first part of the geometry file
  Begining = Hec_RAS_Geom_Part1(df_bief, dir_prj, name_run)
  
  ## loop over the sections
  Nprofils = nrow(Bief_STA)
  for(j in 1:Nprofils)
  {
    Profil_MNT = Bief_MNT_Iter %>% filter(NSection==j)
    HEC_RAS_Geom_Part2(Begining, Nprofils-j+1, Profil_MNT, Bief_STA$ECART[j], Manning)
  }
  
  ## write the end of the file
  write(paste("Chan Stop Cuts=-1\n\n\n",
              "Use User Specified Reach Order=0\n",
              "GIS Ratio Cuts To Invert=-1\n",
              "GIS Limit At Bridges=0\n",
              "Composite Channel Slope=5", sep=""), Begining, append=T)
  
  ## write flow file
  HEC_RAS_Flow(Bief_STA$Q, slope_val, name_run, Profil_name, Reach_name, dir_prj)
  
  ## run and get results with c++ exe and api                                               
  # LPo : résultats avec c++ exe et api                   
  nomfichier = file.path(getwd(), rep_resu, rep_hecras, name_run)
  txtfile = file.path(rep_resu, rep_hecras, paste0("nomfichier.txt"))
  write(nomfichier, txtfile)
  file.copy(from=file.path(dir_exe, exe_hecras), to=dir_prj)
  #file.copy(from=file.path(dir_exe, "hecras_504_x64.exe"), to=dir_prj)
  tmp_wd = getwd(); 
  
  csvfile = file.path(getwd(),rep_resu, rep_hecras, paste0(name_run, ".csv"))
  file.remove(csvfile, showWarnings = TRUE)
  
  setwd(dir_prj)
  system(exe_hecras, show.output.on.console=F); setwd(tmp_wd)
  
  if (file.exists(csvfile)==F) 
  {
    Bief_results_final=NULL
  }else{
    lecture_output = read.csv(csvfile)
    
    ## format results
    # LPo : formatage des résultats
    Bief_results_final = lecture_output[, c(7,4,3,1,10,2,9,5,6,11)]                           
    names(Bief_results_final) = 
      c("Hydraulic_Depth_m","CHAR_m","SL_m",
        "Q_m3_s","HEADLOSS_m","HYDR_RADIUS_m","FRCTN_LOSS_m", 
        "Vel_Chnl_m_s","maxChDepth_m", "Froude")
    Bief_results_final %<>% dplyr::mutate_all(as.numeric)
    
    Bief_results_final$NSection = Bief_STA$NSection
    Bief_results_final$NBief = Bief_STA$NBief
    Bief_results_final$NBasVers = num_bv
  }
  
  return(Bief_results_final)
}

