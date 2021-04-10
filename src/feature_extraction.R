
#' A function that calculate actual pet temperature (based on engineer knowledge)
#' 
#' @param df The data (data.table type)
#' @param config The configuration file (JSON)
#' @param direction The direction (S, N) 
#' @param zone The zone number
#' @param init Initial PET film temperature

calculate_heat_calories <- function(df, config, direction = "S", zone = 1, init = 30){
  # validate arguments
  if( !data.table::is.data.table(x = df) ) stop(" 'df' must be data.table ")
  if( !direction %in% c("S", "N") ) stop(" 'direction' must be one of c('S', 'N') ")
  if( !zone %in% 1:13 ) stop(" 'zone' must be one of 1:13 ")
  
  # read config & define arguments
  doff   = config$doff
  thick_ = config$thick
  speed  = config$speed
  rpm_indicator = config$rpm
  pv_indicator = config$pv
  ptn = config$pattern
  
  # define doff
  DOFF = df[, doff, with = FALSE][[1]]
  
  # define rpm, temperature, speed from input data
  RPM   = df[, paste(direction, rpm_indicator, zone, sep = "_"), with = FALSE][[1]]
  PV    = df[, paste(direction, pv_indicator, zone, sep = "_"), with = FALSE][[1]]
  SPEED = df[, speed, with = FALSE][[1]]
  
  # define calculated value (thick & pattern)
  cal_df = calculate_thick(df = df, doff = doff, thick = thick_, speed = speed, ptn = ptn)
  thick  = cal_df[, paste("thick", direction, zone, sep = "_")]
  ptn    = cal_df[, paste("pattern", direction, zone, sep = "_")] * 10^(-3)
  
  # -- define constant value of factory
  # NW: The width of Nozzle
  # NL: The length of Nozzle
  # TL: The Gap between film and Nozzle
  # chamber: The number of chambers
  NW = ifelse(  test = zone < 3, yes = .015
                , no = ifelse(  test = zone < 6, yes = .010
                                , no = ifelse(test = zone < 11, yes = .006, no = .003) ) )
  NL = ifelse(  test = zone < 3, yes = 2.74
                , no = ifelse(  test = zone == 3, yes = 3.74
                                , no = ifelse( test = zone == 4, yes = 4.74
                                               , no = ifelse(test = zone == 5, yes = 5.74, no = 6.74) ) ) )
  TL = 1.5
  chamber = 4
  
  # -- START: Calculate
  # Q: Air volume
  Q = (RPM * 240 / 3490) / chamber
  
  # V: Nozzle discharge speed
  V = (Q / 60)  / (NW * NL)
  
  # D: Nozzle cross section diameter
  D = (4*NW*NL) / (2*NW + 2*NL)
  
  # estimate u using PV
  u = 1.16 * (10^(-7)) * PV + .0000113
  
  # RE NO.
  Re = (D * V) / u
  
  # PR NO.
  PR = ( .0000007) * (PV)^2 - .0004*(PV) + 0.7386
  
  # Nu
  f0 = (60 + 4*((.14/D) -2)^(2))^(-0.5)
  f  = .0014 + (.125 / (Re^(.32)))
  Nu = PR^( .42 ) * (2/3) * f0^( .75 ) * (( 2*Re / (f/f0 + f0/f))^(2/3))
  
  # K: Air thermal conductivity
  K = 7 * PV *(10^-5) + .0243
  
  # h = Nu * K / D
  h = Nu * K / D
  U = 1 / (1/h + (thick * 10^(-6) ) / .29)
  
  # Initial value
  T0 = init
  
  # Total Heat Calories
  # Q_tot = ifelse(SPEED >= 90, 8* U * ptn * NW * (PV - T0) * TL * (60) * (0.75) / SPEED,
  #                ifelse(SPEED <= 60, 8* U * ptn * NW * (PV - T0) * TL * (60) * (1.4) / SPEED,
  #                       8* U * ptn * NW * (PV - T0) * TL * (60) * (-0.0216 * SPEED + 2.6899) / SPEED))
  Q_tot = 8* U * ptn * NW * (PV - T0) * TL * (60) * 
    (-.000002542 * SPEED^3 + .001042009 * SPEED^2 - 0.132937732*SPEED + 6.189357073) / SPEED
  
  # Cp Value
  Cp = -.00002 * (T0 + 273)^2 + .0204 * (T0 + 273) - 3.1623
  m  = 1.4 * 10^(6) * NW * ptn * thick * 10^(-6) * 8 
  dT = Q_tot / (m * Cp)
  
  # Film Temperature
  Pet_temp = dT + T0
  # -- END: Calculate
  
  # dat.TC = data.frame(NW, NL, D,
  #                     Q, V, u, Re, PR, f0, f, K, Nu, h, thick, ptn, 
  #                     U, Q_tot, Cp, TL,  
  #                     T0, m, dT, film.temp)
  
  
  # out = data.table::data.table(DOFF, thick, Pet_temp)
  # names(x = out) = c(doff, thick_, paste("temp", direction, zone, sep = "_"))
  
  return( Pet_temp )
}



#' A function that calculate actual pet thick (based on engineer knowledge)
#' 
#' @param df The data (data.table type)
#' @param doff The name of doff number
#' @param thick The name of thick variable
#' @param speed The name of speed variable
#' @param ptn The indicator of pattern variable

calculate_thick <- function(df, doff, thick, speed, ptn){
  pattern_data = data.frame(  DOFF_NO = df[, get(doff)]
                              , THICK = df[, get(thick)]
                              , SPEED = df[, get(speed)]
                              , df[, grep(pattern = ptn, x = names(x = df)), with = FALSE])
  
  # replace pattern1 ~ 3 to pattern4
  pattern_data[, paste(ptn, 1:3, sep = "_")] = pattern_data[, paste(ptn, 4, sep = "_")]
  
  # define max pattern & index
  max_pattern   = pattern_data[, paste(ptn, 17, sep = "_")]
  pattern_index = c(2:7, 9:13, 15, 17)
  
  # -- START: Calculate actual thick
  tmp = list()
  i = 1
  for( i in 1:length(pattern_index) ){
    start_i = i
    end_i = i+1
    if( i == length(x = pattern_index) ) end_i = i
    
    start_index = pattern_index[ start_i ]
    end_index   = pattern_index[ end_i ]
    
    start_pattern = pattern_data[, paste(ptn, start_index, sep = "_")]
    end_pattern   = pattern_data[, paste(ptn, end_index, sep = "_")]
    
    # -- Calculate S, N direction pattern length and center pattern length
    pattern_S = start_pattern + (end_pattern - start_pattern ) * .25 # S direction
    pattern_N = start_pattern + (end_pattern - start_pattern ) * .75 # N direction
    pattern_C = (start_pattern + end_pattern) * .5                   # Center
    
    # -- Calculate thick of each tenter using pattern length
    thick_S = (max_pattern / pattern_S) * pattern_data$THICK
    thick_N = max_pattern / pattern_N * pattern_data$THICK
    thick_C = max_pattern / pattern_C * pattern_data$THICK
    
    tmp[[i]] = data.frame( pattern_S, pattern_N, pattern_C, thick_S, thick_N, thick_C )
    names( x = tmp[[i]] ) = c(  paste("pattern", c("N", "S", "C"), i, sep = "_")
                                , paste("thick", c("N", "S", "C"), i, sep = "_")  )
  }
  thick_df = do.call(what = "cbind", args = tmp)
  
  return( thick_df )
}
