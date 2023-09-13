#' Lage faktorer for kalendereffekter 
#'
#' Dette programmet genererer korrigerte faktorer for kalendereffekter 
#' @param forste_ar  kalenderfil lages fra januar forste aar 
#' @param siste_ar kalenderfil lages til og med desember siste aar. 
#' Siste ar kan settes langt fram i tid, og boer settes minst ett aar lenger 
#' enn dataene kalenderfilen skal benyttes paa. 
#'
#' @param k_roddag kan settes til enten TRUE eller FALSE. 
#' Dersom den er satt til TRUE og parameteren
#' 't_roddag' er satt til 'egen_effekt', vil korrigerte faktorer genereres for de
#' roedene dagene som er spesifisert i parameteren 'rod_dag'. Hvis 't_roddag'
#' er satt til 'sondag', vil de spesifiserte roededagene bli behandlet som ekstra
#' soendager. Dersom parameteren 'k_roddag' er satt til FALSE,
#'  er det ikke noedvendig aa spesifisere verdier for parameterne 
#' 't_roddag' og 'rod_dag'. Default verdien er TRUE. 
#' 
#' @param t_roddag  to valgmuligheter 'egen_effekt' eller 'sondag'. 
#' Hvis parameteren k_roddag er TRUE og t_roddag er 'egen_effekt', 
#' vil effekter av roede dager bli estimert med separate variable, 
#' ellers vil de bli betraktet som ekstra soendager i TD/WD variable. 
#' Default verdien er 'sondag'.  
#' 
#' @param rod_dag inneholder de faste hellig- og hoeytidsdagene (roede) 
#' som skal korrigeres, evt. andre dager (for eksempel des24 og des31). 
#' Kristihimmelfartsdag blir automatisk behandlet som en ekstra soendag i programmet 
#' naar TD-/WD-variable konstrueres
#'
#' @param k_fpk TRUE eller FALSE. Hvis det blir TRUE, vil variable for en periode 
#' foer skjaertorsdag bli konstruert. Default verdien er TRUE. Dersom parameteren 
#' k_fpk er satt til FALSE, er det ikke noedvendig aa spesifisere verdier 
#' for ant_fpk og t_fpk.
#' 
#' @param ant_fpk  tallet representerer antall dager foer skjaertorsdag som 
#' blir justert paa grunn av endret aktivitet i tilknytning til paasken (som kan havne 
#' i mars eller april). For eksempel vil ant_fpk lik 3 bety mandag, tirsdag og onsdag 
#' foer skjaertorsdag. Default verdien er 3. 
#' 
#' @param t_fpk  to valgmuligheter 'egen_effekt' og 'sondag'. 
#' Hvis det blir 'egen_effekt' vil effekten foer paasken bli estimert ellers 
#' det blir betrakt som ekstra soendager. Default verdien er 'egen_effekt'. 
#' 
#' @param t_pklordag  'egen_effekt' eller 'sondag'. Hvis 'egen_effekt' velges, 
#' vil effekten av paaske-aften bli estimert, ellers vil det bli betrakt som en soendag. 
#' Default verdien er 'sondag'. 
#' 
#' @param paske_egen kan settes til enten 1 eller 2. Hvis det settes til 1 (default), vil 
#' Skjaertorsdag, Langfredag og 2. paaskedag regnes som ekstra soendager. Paaskeaften 
#' kan enten regnes som en soendag eller ha sin egen effekt. Hvis paaske_egen derimot settes 
#' til 2, vil Skjaertorsdag, Langfredag, paaskeaften og 2. paaskedag ha sin egen felles effekt. 
#' Paaskevariablen er sesongrenset.  
#' 
#' @param paske_mdl 'X12' eller 'linear_reg'. Hvis vi antar at effekten er jevn den samme av 
#' alle prepaaske dagene, vil metoden 'X12' bli brukt, 
#' ellers vil metoden 'linear_reg' bli brukt, som betyr lineeart stigende effekt av hver dag 
#' i paaske perioden eller prepaaske perioden. Default verdien er 'X12'.   
#' 
#' @param t_pinse 'sondag' eller 'egen_effekt'. Hvis 'egen_effekt' velges, vil effekten av 2.
#' pinse-dag bli estimert, ellers vil det bli betrakt som en soendag. Default verdien er 'sondag'.  
#' 
#' @param k_td TRUE eller FALSE. Hvis det blir TRUE, vil trading day variable bli laget. 
#' Default verdien er TRUE. Dersom parameteren 'k_td' er satt til FALSE, er det ikke 
#' noedvendig aa spesifisere verdier for 'td_type', 'k_grupper' og 'monster'.  
#' 
#' @param td_type Det er 4 valgt TD5, TD6, TD15 og TD16. Der 'TD5' har 5 arbeidsdager i uken 
#' fra mandag til fredag med ulike aktiviteter. Loerdag og soendag er fri. 
#' 'TD6' har 6 handledager i uken fra mandag til loerdag, soendag er fri. 
#' 'TD15' er en variant av 'TD5' hvor det antas samme aktivitet 
#' paa de 5 arbeidsdagene fra mandag til fredag. 'TD16' er en variant av 
#' 'TD6' hvor handledagene har de samme handle intensitet. Maa sette k_td = TRUE hvis du vil at programmet kjoeres med verdien av td_type. 
#' 
#' @param k_grupper TRUE eller FALSE. Hvis det er TRUE, vil ukedagene bli gruppert i grupper. 
#' Default verdien er FALSE. Dersom parameteren 'k_grupper' er FALSE, er det ikke noedvendig 
#' aa spesifisere verdi til 'monster'.   
#' 
#' @param monster Dette viser hvilke ukedager som vil bli gruppert sammen, 
#' der 6 elementer spesifiseres dersom TD6 benyttes og 5 elementer spesifiseres dersom 
#' TD5 benyttes over. For eksempel, hvis 'monster' er satt til c(1, 1, 1, 1, 2, 2), 
#' vil mandag, tirsdag, onsdag og torsdag bli gruppert sammen i en gruppe, 
#' mens fredag og loerdag vil vaere i en annen gruppe. Ett annet eksempel er 
#' c(1, 2, 2, 2, 3), da mandag en egen gruppe, mens  tirsdag, onsdag og torsdag er
#' en annen gruppe, mens fredag er en 3. gruppe. Da er loerdag og soendag kontrast gruppen. 
#' 
#' @param fjerne_se_td TRUE eller FALSE. Hvis det blir TRUE, vil sesong av kalendereffekter 
#' bli trukket fra trading day regressorer av. Default verdien er TRUE. Hvis TRUE
#' fjernes sesongeffektene i TD variable basert paa perioden angitt (i forste_ar og siste_ar).
#' 
#' @param skudd_ar TRUE eller FALSE. Hvis det settes til TRUE vil programmet lage en variabel "skudd_ar" for skuddaar. 
#' Default verdien er TRUE, 
#'
#' @return  korrigerte faktorer
#' @importFrom stats aggregate ar
#' @export
#' 
#' @note This function is copied from an internal package (kalfak) at SSB maintained by Dinh Pham. 
#' 
#' @examples 
#' konstruksjon(forste_ar = 2000, siste_ar = 2050, 
#'   k_roddag = TRUE, t_roddag = "sondag", 
#'   rod_dag = c("jan1", "mai1", "mai17", "des24", "des25", "des26"), 
#'   k_fpk = TRUE, ant_fpk = 7, t_fpk = "egen_effekt", 
#'   paske_egen = 1, paske_mdl = "X12", t_pklordag = "egen_effekt",
#'   t_pinse = "sondag", k_td = TRUE, td_type = "TD6", 
#'   k_grupper = TRUE, monster = c(1, 1, 2, 2, 2, 3), 
#'   fjerne_se_td = TRUE, skudd_ar = TRUE)
#'
konstruksjon <- function(forste_ar, siste_ar, k_roddag = TRUE,  
    t_roddag = "sondag", rod_dag = c("jan1", "mai1", "mai17", "des25", "des26"), 
    k_fpk = TRUE, ant_fpk = 3, t_fpk = "egen_effekt", paske_egen = 1, 
    paske_mdl = "X12", t_pklordag = "sondag", t_pinse = "sondag", k_td = FALSE, 
    td_type = "TD6", k_grupper = FALSE, monster = c(1, 2, 3, 4, 5, 6), fjerne_se_td = TRUE, skudd_ar = TRUE) {



nextBlackFriday = function(year){
    first_november = lubridate::ymd(paste(year,"1101"))
    day_of_week = lubridate::wday(first_november, label = TRUE)
    shift = ifelse(as.numeric(day_of_week) < 6, 0, 7)
    next_friday = first_november + lubridate::days(6 - as.numeric(day_of_week) + shift)
    next_black_friday = next_friday + lubridate::weeks(3)
    return(next_black_friday)
    }



siste_ar0   <- siste_ar 
hj1         <- unique(monster)
a_tdgrupper <- length(hj1)



## For Skjaertorsdag, Langfredag og loerdag (paaskeaften)
ant_rode <- length(rod_dag) 



## Antall aar i tidsserien
ant_ar    <- siste_ar - forste_ar + 1


## For paaske 
if (paske_egen == 1) {
    t_paske   <- c("sondag", "sondag", t_pklordag, "sondag", "sondag")
} else if (paske_egen == 2) {
      t_pklordag = "egen_effekt"
      t_paske    = c("egen_effekt", "egen_effekt", t_pklordag, "sondag", "egen_effekt")
} ## End if (paske_egen == 1)
## t_paske for  c("Skj.tosdag", "Langfredag", "paaskeaften", "paaskesoendag", "paaskemandag") 



## Korrigering for Kristihimmelfartsdag
k_kristi <- TRUE  
t_kristi <- "sondag"



## Korrigering for pinse 
k_pinse <- TRUE 



## Testing for t_roddag, t_paske, t_fpk, t_kristi og t_pinse
if (!(t_roddag %in% c("sondag", "egen_effekt", NA))) {
    stop("Du har angitt feil verdi til t_roddag, \n")
}


for (i in 1:length(t_paske)) {
    if (!(t_paske[i] %in% c("sondag", "egen_effekt", NA))) {
        stop("Du har angitt feil(e) verdi(er) til t_paske, \n")
    }
}


if (!(t_fpk %in% c("sondag", "egen_effekt", NA))) {
    stop("Du har angitt feil verdi til t_fpk, \n")
}


if (!(t_kristi %in% c("sondag", "egen_effekt", NA))) {
    stop("Du har angitt feil verdi til t_kristi, \n")
}


if (!(t_pinse %in% c("sondag", "egen_effekt", NA))) {
    stop("Du har angitt feil verdi til t_pinse, \n")
}



if (k_td == FALSE) {
    td_type = "TD15"
}



if (k_grupper == TRUE) {
    if (td_type == "TD6") {
        if (!length(monster) == 6) 
            stop("Du har angitt feilverdi til monster.\n")
    } else if (td_type == "TD5") {
          if (!length(monster) == 5) 
              stop("Du har angitt feilverdi til monster.\n")
    } ## End if (td_type ==
} ## End if (k_grupper == TRUE)







## Beregning starter fra her =============================================
sistear_1 <- forste_ar - 1

date1 <- as.Date(paste(forste_ar, "-01-01", sep = ""))
date2 <- as.Date(paste(siste_ar, "-12-31", sep = ""))
ant_obs = as.numeric(difftime(date2, date1, units = "days")) + 1



## Lage dataset for kalender
m_kal <- as.Date(1:ant_obs, origin = paste(sistear_1, "-12-31", sep = ""))
m_kal <- as.data.frame(m_kal)
colnames(m_kal) <- c("periode")



## Lage ukedager 
m_kal$ukedag <- lubridate::wday(m_kal$periode, week_start = 1)


m_kal$ar   <- as.numeric(substr(m_kal$periode, 1, 4))
m_kal$mn   <- as.numeric(substr(m_kal$periode, 6, 7))
m_kal$dato <- as.numeric(substr(m_kal$periode, 9, 10))



## Beholder opprinnelige dataset m_kal
m_kal0 <- m_kal 
## =======================================================================================



## Lage paaske
hj1 <- timeDate::Easter(forste_ar:siste_ar)
hj2 <- as.numeric(substr(hj1, 1, 4))
hj3 <- as.numeric(substr(hj1, 6, 7))
hj4 <- as.numeric(substr(hj1, 9, 10))

m_paske <- as.data.frame(cbind(hj2, hj3, hj4))
colnames(m_paske) <- c("ar", "mn", "dato_pk")



## Lage kristihimmelfartsdag
if (k_kristi == TRUE) {
    hj1 <- timeDate::Ascension(forste_ar:siste_ar)
    hj2 <- as.numeric(substr(hj1, 1, 4))
    hj3 <- as.numeric(substr(hj1, 6, 7))
    hj4 <- as.numeric(substr(hj1, 9, 10))

    m_kristi <- as.data.frame(cbind(hj2, hj3, hj4))
    colnames(m_kristi) <- c("ar", "mn", "dato_kr")
} ## End if (k_kristi == TRUE)



## Lage pinse 
if (k_pinse == TRUE) {
    hj1 <- timeDate::Pentecost(forste_ar:siste_ar)
    hj2 <- as.numeric(substr(hj1, 1, 4))
    hj3 <- as.numeric(substr(hj1, 6, 7))
    hj4 <- as.numeric(substr(hj1, 9, 10))

    m_pinse <- as.data.frame(cbind(hj2, hj3, hj4))
    colnames(m_pinse) <- c("ar", "mn", "dato_pi")
} ## End if (k_pinse == TRUE)



## Lager dummy variable for roede dager
if (k_roddag == TRUE) { 
    m_rode <- matrix(0, ant_obs, (ant_rode + 3))
    m_rode[, 1] <- m_kal$ar
    m_rode[, 2] <- m_kal$mn
    m_rode[, 3] <- m_kal$dato

    for (i in 1:ant_rode) {
        hj1 <- substr(rod_dag[i], 1, 3) 
        if (hj1 == "jan") rod1 = 1
            else if (hj1 == "feb") rod1 = 2
            else if (hj1 == "mar") rod1 = 3 
            else if (hj1 == "apr") rod1 = 4 
            else if (hj1 == "mai") rod1 = 5 
            else if (hj1 == "jun") rod1 = 6 
            else if (hj1 == "jul") rod1 = 7 
            else if (hj1 == "aug") rod1 = 8 
            else if (hj1 == "sep") rod1 = 9 
            else if (hj1 == "okt") rod1 = 10
            else if (hj1 == "nov") rod1 = 11
            else if (hj1 == "des") rod1 = 12

        rod2 <- as.numeric(substr(rod_dag[i], 4, 5)) ## dato til en roed dag

        for (j in 1:ant_obs) {
            k <- 3 + i
            m_rode[j, k] <- ifelse (m_rode[j, 2] == rod1 & m_rode[j, 3] == rod2, 1, 0) 
            if (m_rode[j, k] == 1 & m_kal$ukedag[j] == 7) m_rode[j, k] <- 0 
        } ## End for (j in 1:ant_obs) {
    } ## End for (i in 1:ant_rode) {

    m_rode <- as.data.frame(m_rode)
    colnames(m_rode) <- c("ar", "mn", "dato", rod_dag)
} ## End if (k_roddag == TRUE)

m_kal <- as.list(m_kal)  # list isfaster

## Lage dummy variable for paaske
Easter_day <- as.Date(timeDate::Easter(forste_ar:siste_ar))
m_kal$paske <- rep(0, ant_obs)

for (i in 1:length(Easter_day)) {
  easter_day_i <- m_kal$periode == Easter_day[i]
  m_kal$paske[easter_day_i] <- 0
  m_kal$ukedag[easter_day_i] <- 7
  for (k in 1:3) {
    ## 1:3 for loerdag, Lang fredag og Skjaertorsdag
    day_j_4_k <- rep(FALSE, ant_obs)
    day_j_4_k[which(easter_day_i) - 4 + k] <- TRUE
    
    k_sondag <- t_paske[k] == "sondag"
    
    m_kal$paske[day_j_4_k & k_sondag] <- 0
    m_kal$ukedag[day_j_4_k & k_sondag] <- 7
    
    m_kal$paske[day_j_4_k & !k_sondag] <- 1
    m_kal$ukedag[day_j_4_k & !k_sondag] <- 0
    
  }  ## End for (k in 1:3)
  
  day_j_1 <- rep(FALSE, ant_obs)
  day_j_1[which(easter_day_i) + 1] <- TRUE
  
  if (t_paske[5] == "sondag") {
    m_kal$paske[day_j_1] <- 0
    m_kal$ukedag[day_j_1] <- 7
  } else {
    m_kal$paske[day_j_1] <- 1
    m_kal$ukedag[day_j_1] <- 0
  } ## End if (t_paske[5] == "sondag")
  
}  ## End for (i in 1:length(Easter_day))




## Lage dummy variables for before paaske
if (k_fpk == TRUE) {
  m_kal$f_pk  <- rep(0, ant_obs)
  for (i in 1:length(Easter_day)) { 
    easter_day_i <- m_kal$periode == Easter_day[i]
    for (l in 1:ant_fpk) { 
      day_j_3_l <- rep(FALSE, ant_obs)
      day_j_3_l[which(easter_day_i) - 3 - l] <- TRUE
      if (t_fpk == "sondag") {
        m_kal$f_pk[day_j_3_l] <- 0
        m_kal$ukedag[day_j_3_l] <- 7
      } else if (!(t_fpk == "sondag")) {
        ukedag7 <- m_kal$ukedag == 7
        m_kal$f_pk[day_j_3_l & ukedag7] <- 0
        m_kal$ukedag[day_j_3_l & ukedag7] <- 7
        m_kal$f_pk[day_j_3_l & !ukedag7] <- 1
        m_kal$ukedag[day_j_3_l & !ukedag7] <- 0
      } ## End if (t_fpk == "sondag")
    }}}
## Proev aa liste ut m_kal[70:110, ]



## Lage kristihimmelfartsdag
if (k_kristi == TRUE) {
    Ascens_day <- as.Date(timeDate::Ascension(forste_ar:siste_ar))
    m_kal$kristi <- rep(0, ant_obs)
    for (i in 1:length(Ascens_day)) {
      ascens_day_i <- m_kal$periode == Ascens_day[i]      
      if (t_kristi == "sondag") { 
        m_kal$kristi[ascens_day_i] <- 0 
        m_kal$ukedag[ascens_day_i] <- 7
      } else {
        m_kal$kristi[ascens_day_i] <- 1 
        m_kal$ukedag[ascens_day_i] <- 0 
      } ## End  if (t_kristi == "sondag")
    } ## End for (i in 1:length(Easter_day))
} ## End if (k_kristi == TRUE)



## Lage Pentecost
if (k_pinse == TRUE) {
  Pente_day <- as.Date(timeDate::Pentecost(forste_ar:siste_ar))
  m_kal$pinse <- rep(0, ant_obs)
  for (i in 1:length(Pente_day)) {
    pente_day_i <- m_kal$periode == Pente_day[i]   
    day_j_1 <- rep(FALSE, ant_obs)
    day_j_1[which(pente_day_i) + 1] <- TRUE
    if (t_pinse == "sondag") { 
      m_kal$pinse[pente_day_i]  <- 0 
      m_kal$ukedag[pente_day_i] <- 7
      m_kal$pinse[day_j_1]  <- 0 
      m_kal$ukedag[day_j_1] <- 7
    } else {
      m_kal$pinse[pente_day_i]  <- 0 
      m_kal$ukedag[pente_day_i] <- 7
      m_kal$pinse[day_j_1]  <- 1 
      m_kal$ukedag[day_j_1] <- 0 
}}} 



## Tester for t_roddag = "sondag"
if (k_roddag == TRUE) {
  if (t_roddag == "sondag") {
    for (k in 4:ncol(m_rode)) { 
      m_rode_k_1 <- m_rode[, k] == 1
      m_rode[m_rode_k_1, k] <- 0
      m_kal$ukedag[m_rode_k_1] <- 7
    } ## End for (k in 1:ncol(m_rode)
  } else {
    for (k in 4:ncol(m_rode)) { 
      m_kal$ukedag[m_rode[ , k] == 1] <- 0     
    } ## End for (k in 4:ncol(m_rode))
  } ## End else 
## End if (t_roddag == "sondag")
} ## End if (k_roddag == TRUE)             



## Lager ukedager
m_kal$man <- as.numeric(m_kal$ukedag == 1)
m_kal$tir <- as.numeric(m_kal$ukedag == 2)
m_kal$ons <- as.numeric(m_kal$ukedag == 3)
m_kal$tor <- as.numeric(m_kal$ukedag == 4)
m_kal$fre <- as.numeric(m_kal$ukedag == 5)
m_kal$lor <- as.numeric(m_kal$ukedag == 6)
m_kal$son <- as.numeric(m_kal$ukedag == 7)

## =======================================================================================


m_kal <- as.data.frame(m_kal) # back to data.frame


## Merge sammen m_kal og m_rode
if (k_roddag == TRUE) {
    df_list    <- list(m_kal, m_rode)
    ## samle_alle <- df_list %>% reduce(inner_join, by = c("dato", "mn", "ar"))
    ## samle_alle <- purrr::reduce(function(x, y) inner_join(x, y, by = c("dato", "mn", "ar")), df_list)
    samle_alle <- merge(m_kal, m_rode, by = c("dato", "mn", "ar")) 
} else samle_alle <- m_kal



ttest_x <- apply(samle_alle, 2, function(x) sum(x != 0) == 0)
samle_1 <- samle_alle[, -which(ttest_x)] 

if (ncol(samle_1) == 0) {
    samle_1 = samle_alle
} ## End if (ncol(samle_1)



hj1 <- colnames(samle_1)
names_agg <- hj1[!(hj1 %in% c("periode", "ukedag", "ar", "mn", "dato"))]
lgt_agg   <- length(hj1)

samle_mnd <- aggregate(samle_1[6:lgt_agg], by = list(samle_1$mn, samle_1$ar), 
                       FUN = sum)
names(samle_mnd)[1] <- "mn"
names(samle_mnd)[2] <- "ar"
samle_mnd <- as.data.frame(samle_mnd)


samle_mnd_cp0 <- samle_mnd
## =======================================================================================



## Lager regressorer for ukedager
if (td_type == "TD6") {
    samle_mnd$man <- samle_mnd$man - samle_mnd$son 
    samle_mnd$tir <- samle_mnd$tir - samle_mnd$son 
    samle_mnd$ons <- samle_mnd$ons - samle_mnd$son 
    samle_mnd$tor <- samle_mnd$tor - samle_mnd$son 
    samle_mnd$fre <- samle_mnd$fre - samle_mnd$son 
    samle_mnd$lor <- samle_mnd$lor - samle_mnd$son 
    ## samle_mnd <- samle_mnd %>% select(- son)
    samle_mnd     <- samle_mnd[!(colnames(samle_mnd) %in% "son")] 
} else if (td_type == "TD5") {
      samle_mnd$man <- samle_mnd$man - 0.5 * (samle_mnd$lor + samle_mnd$son) 
      samle_mnd$tir <- samle_mnd$tir - 0.5 * (samle_mnd$lor + samle_mnd$son) 
      samle_mnd$ons <- samle_mnd$ons - 0.5 * (samle_mnd$lor + samle_mnd$son) 
      samle_mnd$tor <- samle_mnd$tor - 0.5 * (samle_mnd$lor + samle_mnd$son) 
      samle_mnd$fre <- samle_mnd$fre - 0.5 * (samle_mnd$lor + samle_mnd$son) 
      ## samle_mnd <- samle_mnd %>% select(- c(lor, son))
      samle_mnd     <- samle_mnd[!(colnames(samle_mnd) %in% c("lor", "son"))]
} else if (td_type == "TD15") {
      samle_mnd$td15 <- (samle_mnd$man + samle_mnd$tir + samle_mnd$ons + 
                         samle_mnd$tor + samle_mnd$fre) - 
                         2.5 * (samle_mnd$lor + samle_mnd$son) 
      ## samle_mnd <- samle_mnd %>% select(- c(man, tir, ons, tor, fre, lor, son)) 
      samle_mnd <- samle_mnd[!(colnames(samle_mnd) %in% c("man", "tir", "ons", "tor", "fre","lor", "son"))] 
} else if (td_type == "TD16") {
      samle_mnd$td16 <- (samle_mnd$man + samle_mnd$tir + samle_mnd$ons + 
                         samle_mnd$tor + samle_mnd$fre + samle_mnd$lor) - 
                         6 * samle_mnd$son  
      ## samle_mnd <- samle_mnd %>% select(- c(man, tir, ons, tor, fre, lor, son)) 
      samle_mnd      <- samle_mnd[!(colnames(samle_mnd) %in% c("man", "tir", "ons", "tor", "fre","lor", "son"))] 

} ## End else if (td_type == "TD16")


samle_mnd_cp1 <- samle_mnd
## =======================================================================================



## Lage dummy variable for foer (before) paaske ved aa dividere med ant_fpk 
if (k_fpk == TRUE & !(t_fpk == "sondag")) {
    ## hj1 <- sum(m_kal$f_pk) / ant_ar 
    if (paske_mdl == "X12") { 
        samle_mnd$f_pk <- samle_mnd$f_pk / ant_fpk
    } else if (paske_mdl == "linear_reg") { 
          for (i in 1:nrow(samle_mnd)) {
              if (samle_mnd$mn[i] == 3) {
                  samle_mnd$f_pk[i] <- (samle_mnd$f_pk[i] / ant_fpk) ** 2
                  samle_mnd$f_pk[(i + 1)] <- 1 - samle_mnd$f_pk[i]
              } ## End if (samle_mnd$mn[i] == 3)
          } ## End for (i in 1:nrow(samle_mnd))
    } ## End if (paske_mdl == "X12")

    gj_fpk <- aggregate(samle_mnd$f_pk, list(samle_mnd$mn), FUN = mean) 
    hj1 <- do.call("rbind", rep(list(gj_fpk), ant_ar))
    samle_mnd$f_pk <- samle_mnd$f_pk - hj1$x 
} ## End if (k_fpk == TRUE & t_fpk == "egen_effekt")



## Lage dummy variable for paaske ved aa dividere med antall paaskedager
hj1 <- t_paske %in% "egen_effekt"
if (sum(hj1) > 0) {
    l_paske <- length(t_paske[!(t_paske %in% c("sondag"))]) 
    if (l_paske > 0) {
        if (paske_mdl == "X12") {
            samle_mnd$paske <- samle_mnd$paske / l_paske 
        } else if (paske_mdl == "linear_reg") { 
              for (i in 1:nrow(samle_mnd)) {
                  if (samle_mnd$mn[i] == 3) { 
                      samle_mnd$paske[i] <- (samle_mnd$paske[i] / l_paske) ** 2
                      samle_mnd$paske[(i + 1)] <- 1 - samle_mnd$paske[i]
                  } ## End if (samle_mnd$mn[i] == 3)
              } ## End for (i in 1:nrow(samle_mnd))
        } ## End if (paske_mdl == "X12")
    } ## End if (l_paske > 0)

    gj_paske <- aggregate(samle_mnd$paske, list(samle_mnd$mn), FUN = mean)  
    hj1 <- do.call("rbind", rep(list(gj_paske), ant_ar))
    samle_mnd$paske <- samle_mnd$paske - hj1$x 
} ## End if (sum(hj1) > 0)
## =======================================================================================



## Lage dummy variable for pinse 
if (k_pinse == TRUE & !(t_pinse == "sondag")) {
    gj_pinse <- aggregate(samle_mnd$pinse, list(samle_mnd$mn), FUN = mean) 
    hj1 <- do.call("rbind", rep(list(gj_pinse), ant_ar))
    samle_mnd$pinse <- samle_mnd$pinse - hj1$x 
} ## End if (k_pinse == TRUE & t_pinse == "egen_effekt")



kjor_td <- fjerne_se_td 
if (fjerne_se_td == TRUE) { 
## For ukedager 
## For TD6
if (td_type == "TD6") {
    gj_ukedager <- aggregate(list(samle_mnd$man, samle_mnd$tir, samle_mnd$ons, samle_mnd$tor, 
                   samle_mnd$fre, samle_mnd$lor), list(samle_mnd$mn), FUN = mean) 
    gj_ukedager <- as.matrix(gj_ukedager[, - 1]) 
    colnames(gj_ukedager) <- NULL
    rep_gj <- do.call(rbind, replicate(ant_ar, gj_ukedager, simplify = FALSE))

    samle_mnd[, c("man", "tir", "ons", "tor", "fre", "lor")]  <- 
              samle_mnd[, c("man", "tir", "ons", "tor", "fre", "lor")] - rep_gj
} else if (td_type == "TD5") {
      gj_ukedager <- aggregate(list(samle_mnd$man, samle_mnd$tir, samle_mnd$ons, samle_mnd$tor, 
                     samle_mnd$fre), list(samle_mnd$mn), FUN = mean) 
      gj_ukedager <- as.matrix(gj_ukedager[, - 1]) 
      colnames(gj_ukedager) <- NULL
      rep_gj <- do.call(rbind, replicate(ant_ar, gj_ukedager, simplify = FALSE))
 
      samle_mnd[, c("man", "tir", "ons", "tor", "fre")]  <- 
                samle_mnd[, c("man", "tir", "ons", "tor", "fre")] - rep_gj
} else if (td_type == "TD16") {
      gj_ukedager <- aggregate(list(samle_mnd$td16), list(samle_mnd$mn), FUN = mean) 
      gj_ukedager <- as.matrix(gj_ukedager[, - 1]) 
      colnames(gj_ukedager) <- NULL
      rep_gj <- do.call(rbind, replicate(ant_ar, gj_ukedager, simplify = FALSE))
 
      samle_mnd$td16  <- samle_mnd$td16 - rep_gj
} else if (td_type == "TD15") {
      gj_ukedager <- aggregate(list(samle_mnd$td15), list(samle_mnd$mn), FUN = mean) 
      gj_ukedager <- as.matrix(gj_ukedager[, - 1]) 
      colnames(gj_ukedager) <- NULL
      rep_gj <- do.call(rbind, replicate(ant_ar, gj_ukedager, simplify = FALSE))
 
      samle_mnd$td15  <- samle_mnd$td15 - rep_gj
} ## End if (td_type == 
} ## End if (fjerne_se_td == TRUE
## =======================================================================================



## Lage riktig format for dato
nymn <- sprintf("%02d", samle_mnd$mn)
samle_mnd$dato1 <- as.Date(paste(samle_mnd$ar, "-", nymn, "-01", sep = "")) 
## samle_mnd <- samle_mnd %>% select(- c(ar, mn))
## samle_mnd <- subset(samle_mnd, select = -c(ar, mn))
samle_mnd <- samle_mnd[!(colnames(samle_mnd) %in% c("ar", "mn"))] 


## samle_mnd00 <- samle_mnd




## Lager grupper for trading day 
if (td_type == "TD6") {
    ant_dag <- 6
} else if (td_type == "TD5") {
      ant_dag <- 5
} else ant_dag <- 1
## End if (td_type == 


if (k_grupper == TRUE) {
    mtd_grupper <- matrix(0, ant_dag, a_tdgrupper) 
    for (i in 1:ant_dag) {
        mtd_grupper[i, monster[i]] <- 1
    } ## End for (i in 1:ant_dag) 
   
    if (td_type == "TD6") { 
        ## df1 <- subset(samle_mnd, select = c(man, tir, ons, tor, fre, lor))
        df1    <- samle_mnd[colnames(samle_mnd) %in% c("man", "tir", "ons", "tor", "fre", "lor")] 
        ## df2 <- subset(samle_mnd, select = - c(man, tir, ons, tor, fre, lor))
        df2    <- samle_mnd[!(colnames(samle_mnd) %in% c("man", "tir", "ons", "tor", "fre", "lor"))] 
        df3    <- as.matrix(df1) %*% mtd_grupper
        colnames(df3) <- paste("gr_", 1:a_tdgrupper, sep = "")
        nysamle_mnd   <- cbind(df2, df3)
        samle_mnd <- nysamle_mnd 
    } else if (td_type == "TD5") { 
          ## df1 <- subset(samle_mnd, select =   c(man, tir, ons, tor, fre))
          df1    <- samle_mnd[colnames(samle_mnd) %in% c("man", "tir", "ons", "tor", "fre")] 
          ## df2 <- subset(samle_mnd, select = - c(man, tir, ons, tor, fre))
          df2    <- samle_mnd[!(colnames(samle_mnd) %in% c("man", "tir", "ons", "tor", "fre"))]
          df3    <- as.matrix(df1) %*% mtd_grupper
          colnames(df3) <- paste("gr_", 1:a_tdgrupper, sep = "")
          nysamle_mnd <- cbind(df2, df3)
          samle_mnd <- nysamle_mnd 
    } ## End if (td_type == "TD6") 

} ## End if (k_grupper == TRUE)



## For leap year
if (skudd_ar == TRUE) {
samle_mnd$skudd_ar <- 0 
j <- 0
for (i in forste_ar:siste_ar) {
     j <- j + 1 
    namnh <- lubridate::leap_year(i) 
    if (namnh == TRUE) {
        samle_mnd$skudd_ar[(j - 1) * 12 +2] <- 0.75
    } else {
           samle_mnd$skudd_ar[(j - 1) * 12 +2] <- - 0.25
    }
} ## for (i in forste_ar
} ## End if (skudd_ar == TRUE)



## Reordering av samle_mnd
hj1       <- colnames(samle_mnd)
hj2       <- c(hj1[hj1 == "dato1"], hj1[hj1 != "dato1"]) 
samle_mnd <- samle_mnd[, hj2] 



if (k_td == FALSE) {
    samle_mnd <- samle_mnd[ , !names(samle_mnd) %in% "td15"]
}



## Lage kvartalstall fra maanedstall
mj1 <- samle_mnd[!(colnames(samle_mnd) %in% c("dato1"))]
cn_mj1 <- colnames(mj1)
ac_mj1 <- ncol(mj1)

mj1$ar <- lubridate::year(samle_mnd$dato1)
mj1$kv <- lubridate::quarter(samle_mnd$dato1)

samle_kv    <- aggregate(mj1[, 1:ac_mj1], by = list(mj1$kv, mj1$ar), FUN = sum) 
samle_kv    <- samle_kv[ , - c(1, 2)] 
samle_kv$kv <- samle_mnd$dato1[seq(1, nrow(samle_mnd), by = 3)]



## Reordering av samle_kv
hj1      <- colnames(samle_kv)
hj2      <- c(hj1[hj1 == "kv"], hj1[hj1 != "kv"]) 
samle_kv <- samle_kv[, hj2] 
samle_kv <- samle_kv[!(colnames(samle_kv) %in% c("ar"))]



## Lage utfil 
list(samle_mnd = samle_mnd, 
     samle_kv  = samle_kv, 
     m_paske   = m_paske,
     m_kristi  = m_kristi,
     m_pinse   = m_pinse)

} ## End konstruksjon
