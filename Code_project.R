library(dplyr)
library(stringr)
library(pbapply)
library(parallel)
library(tidyr)

###########################################
#           data preperation              #
###########################################

data1 = read.csv("lista_meczow.csv")
data2 = read.csv("lista_meczow_sezon_wczesniej.csv")
data3 = read.csv("lista_meczow_dwa_sezon_wczesniej.csv")
calendar <- read.csv("lista_meczow_calendar.csv")

data1 <- data1 %>%
  arrange(desc(date))

data2 <- data2 %>%
  arrange(desc(date))

data3 <- data3 %>%
  arrange(desc(date))


data <- bind_rows(data1, data2, data3)

data <- data %>%
  filter(homeTeam != ('LKS Lodz') & homeTeam != 'Miedz Legnica' & homeTeam != 'Puszcza Niepolomice' & homeTeam!= 'Ruch Chorzow',
         homeTeam != 'Slask Wroclaw' & homeTeam != 'Stal Mielec' & homeTeam != 'Warta Poznan') %>%
  filter(awayTeam != ('LKS Lodz') & awayTeam != 'Miedz Legnica' & awayTeam != 'Puszcza Niepolomice' & awayTeam!= 'Ruch Chorzow',
         awayTeam != 'Slask Wroclaw' & awayTeam != 'Stal Mielec' & awayTeam != 'Warta Poznan')


train <- data %>%              #train set
  slice_tail(n = floor(nrow(data)-nrow(data1)))

validate <- data %>%           #validation set
  slice_head(n=ceiling(nrow(data1)))



train <- train %>%
  mutate(date = as.Date(date)) %>%
  mutate("t" = as.numeric(max(date) - date)/7) %>%
  select(-ď.żid, -date)

validate <- validate %>%
  mutate(date = as.Date(date)) %>%
  select(-ď.żid, -date)

data <- data %>%
  mutate(date = as.Date(date)) %>%
  mutate("t" = as.numeric(max(date) - date)/7) %>%
  select(-ď.żid, -date)





teams_1 <- data %>% distinct(homeTeam)
teams_2 <- data %>% distinct(awayTeam)
colnames(teams_1) <- "Team"
colnames(teams_2) <- "Team"
clubs <- union(teams_1, teams_2)

clubs <- clubs %>%   #table with clubs and parameters names
  mutate("shortName" = ifelse(Team == "Cracovia","cracovia",
                              str_to_lower(str_sub(Team, start = 1, end = str_locate(Team, ' ')[,1]))
  )) %>%
  mutate("alfa_name" = paste0('alfa_', shortName)) %>%
  mutate("beta_name" = paste0('beta_', shortName)) %>%
  arrange(Team)


all_info <- train %>%
  inner_join(clubs, by = c("homeTeam" = "Team")) %>%
  inner_join(clubs, by = c("awayTeam" = 'Team')) %>%
  select(-shortName.x, - shortName.y) %>%
  rename(alfa_nameHome = alfa_name.x,
         beta_nameHome = beta_name.x,
         alfa_nameAway = alfa_name.y,
         beta_nameAway = beta_name.y)


all_info_validate <- validate %>%
  inner_join(clubs, by = c("homeTeam" = "Team")) %>%
  inner_join(clubs, by = c("awayTeam" = 'Team')) %>%
  select(-shortName.x, - shortName.y) %>%
  rename(alfa_nameHome = alfa_name.x,
         beta_nameHome = beta_name.x,
         alfa_nameAway = alfa_name.y,
         beta_nameAway = beta_name.y)


final_set <- data %>%
  inner_join(clubs, by = c("homeTeam" = "Team")) %>%
  inner_join(clubs, by = c("awayTeam" = 'Team')) %>%
  select(-shortName.x, - shortName.y) %>%
  rename(alfa_nameHome = alfa_name.x,
         beta_nameHome = beta_name.x,
         alfa_nameAway = alfa_name.y,
         beta_nameAway = beta_name.y)




tau <- function(x, y, lambda, mu, p) {
  case_when(
    x == 0 & y == 0 ~ 1 - lambda * mu * p,
    x == 0 & y == 1 ~ 1 + lambda * p,
    x == 1 & y == 0 ~ 1 + mu * p,
    x == 1 & y == 1 ~ 1 - p,
    TRUE ~ 1
  )
}




lkf <- function(data, params, epsilon){  #likelihood function for estimating alfas and betas
  gamma <- params['gamma']
  p <- params['p']
 
  alfa_names <- names(params)[startsWith(names(params), "alfa_")]  
  params[alfa_names] <- params[alfa_names] / mean(params[alfa_names])
  
  
  beta_names <- names(params)[startsWith(names(params), "beta_")]  
  params[beta_names] <- params[beta_names] / mean(params[beta_names])
  
  alfaHome <- params[data$alfa_nameHome]
  betaHome <- params[data$beta_nameHome]
  alfaAway <- params[data$alfa_nameAway]
  betaAway <- params[data$beta_nameAway]

  
  lambdas <- alfaHome*betaAway*gamma
  mus <- alfaAway*betaHome
  
  phis <- exp(-epsilon*data$t)
  taus <- tau(data$homeScore, data$awayScore, lambdas, mus, p)
  
  return(-sum(phis*(-lambdas+ log(lambdas)*data$homeScore+ -mus+log(mus)*data$awayScore+log(taus))))
  
}



################################
#       epsilon estimation     #
################################


names_alfa <- sort(union(unique(final_set$alfa_nameHome), unique(final_set$alfa_nameAway)))
names_beta <- sort(union(unique(final_set$beta_nameHome), unique(final_set$beta_nameAway)))

alfas_params <- setNames(rep(1, length(names_alfa)), names_alfa)
betas_params <- setNames(rep(1, length(names_beta)), names_beta)

params <- c(alfas_params, betas_params, gamma = 1, p = 0.01)

dolne_limity <- rep(0.0001, length(params)) 
names(dolne_limity) <- names(params)

gorne_limity <- rep(Inf, length(params))
names(gorne_limity) <- names(params)

dolne_limity['p'] <- -2
gorne_limity['p'] <- 0.99


p_xy <- function(x,y,alfaHome, alfaAway, betaHome, betaAway, gamma, p){
  lambda <- alfaHome*betaAway*gamma
  mu <- alfaAway*betaHome
  poi_x <- exp(-lambda)*lambda^x /factorial(x)
  poi_y <- exp(-mu)*mu^y /factorial(y)
  result <- tau(x,y,lambda,mu,p)*poi_x*poi_y
  return(result)
}

calculate_prob <- function(wynik){
  X <- seq(0, 20, by = 1)
  Y <- seq(0, 20, by = 1)
  combinations <- expand.grid(X,Y)
  combinations <- expand.grid(X,Y)
  combinations_winHome <- combinations %>%
    filter(Var1>Var2)
  combinations_draw <- combinations %>%
    filter(Var1==Var2)
  combinations_winAway <- combinations %>%
    filter(Var1<Var2)
  
  probs_home <- apply(wynik, 1, function(x) { sum(p_xy(combinations_winHome[,1], combinations_winHome[,2], as.numeric(x['value_alfaHome']), as.numeric(x['value_alfaAway']), 
                                                       as.numeric(x['value_betaHome']), as.numeric(x['Value_betaAway']),
                                                       as.numeric(x['gamma']), as.numeric(x['p'])))
  })
  
  probs_draw <-apply(wynik, 1, function(x) { sum(p_xy(combinations_draw[,1], combinations_draw[,2], as.numeric(x['value_alfaHome']), as.numeric(x['value_alfaAway']), 
                                                      as.numeric(x['value_betaHome']), as.numeric(x['Value_betaAway']),
                                                      as.numeric(x['gamma']), as.numeric(x['p'])))
  })
  
  probs_away <- apply(wynik, 1, function(x) { sum(p_xy(combinations_winAway[,1], combinations_winAway[,2], as.numeric(x['value_alfaHome']), as.numeric(x['value_alfaAway']), 
                                                       as.numeric(x['value_betaHome']), as.numeric(x['Value_betaAway']),
                                                       as.numeric(x['gamma']), as.numeric(x['p'])))
  })
  probs_with_deltas <- data.frame(
    'probs_home' = probs_home,
    'probs_draw' = probs_draw,
    'probs_away' = probs_away,
    'delta_home' = wynik['delta_home'],
    'delta_draw' = wynik['delta_draw'],
    'delta_away' = wynik['delta_away']
  )
  return(sum(apply(probs_with_deltas, 1, function(x) { return(log(x['probs_home'])*x['delta_home']+
                                                                log(x['probs_away'])*x['delta_away']+
                                                                log(x['probs_draw'])*x['delta_draw']) })))
}



#########################################
#      looking for the best epsilon     #
#########################################

epsilons <- seq(from = 0.02, to =0.03, by = 0.001)
sepsilon <- numeric(length(epsilons))
i =1

for(e in epsilons){
  wynik <- optim(
    par = params,
    fn = lkf,
    data = all_info,
    method = "L-BFGS-B",
    lower = dolne_limity,
    upper = gorne_limity,
    control = list(
      maxit = 5000,
      factr = 1e1,    
      pgtol = 1e-9,
      trace = 1,
      REPORT = 10 
    ),
    epsilon = e
  )
  
  
  wynik_df <- data.frame(
    `Parameter` = names(wynik$par),
    `Value` = as.numeric(wynik$par)
  )
  
  
  all_info_values <- all_info_validate %>%
    inner_join(wynik_df, by = c('alfa_nameHome' = 'Parameter')) %>%
    inner_join(wynik_df, by = c('alfa_nameAway' = 'Parameter')) %>%
    inner_join(wynik_df, by = c('beta_nameHome' = 'Parameter')) %>%
    inner_join(wynik_df, by = c('beta_nameAway' = 'Parameter')) %>%
    rename(
      value_alfaHome = Value.x,
      value_alfaAway = Value.y,
      value_betaHome = Value.x.x,
      Value_betaAway = Value.y.y
    ) %>%
    mutate('p' = wynik$par['p']) %>%
    mutate('gamma' = wynik$par['gamma']) %>%
    mutate(delta_home = ifelse(homeScore>awayScore, 1, 0 )) %>%
    mutate(delta_draw = ifelse(homeScore==awayScore, 1, 0 )) %>%
    mutate(delta_away = ifelse(homeScore<awayScore, 1, 0 ))
  print(e)
  print(calculate_prob(all_info_values))
  sepsilon[i]<-calculate_prob(all_info_values)
  i = i+1
}



###############################
# calculating alfas and betas #
###############################

names_alfa <- sort(union(unique(final_set$alfa_nameHome), unique(final_set$alfa_nameAway)))
names_beta <- sort(union(unique(final_set$beta_nameHome), unique(final_set$beta_nameAway)))

alfas_params <- setNames(rep(1, length(names_alfa)), names_alfa)
betas_params <- setNames(rep(1, length(names_beta)), names_beta)

params <- c(alfas_params, betas_params, gamma = 1, p = 0.01)

dolne_limity <- rep(0.0001, length(params)) # dla alf i bet > 0
names(dolne_limity) <- names(params)

gorne_limity <- rep(Inf, length(params))
names(gorne_limity) <- names(params)

dolne_limity['p'] <- -2
gorne_limity['p'] <- 0.99



wynik <- optim(
  par = params,
  fn = lkf,
  data = final_set,
  method = "L-BFGS-B",
  lower = dolne_limity,
  upper = gorne_limity,
  control = list(
    maxit = 5000, # Znacznie zwiększa liczbę kroków algorytmu     # Algorytm będzie szukał poprawy aż do bardzo małych różnic
    trace = 1,# Będziesz widzieć postęp (wartość funkcji) w konsoli
    REPORT = 10 # Raportuje postęp co 10 iteracji
  ),
  epsilon = epsilons[which(sepsilon ==max(sepsilon))]
)
wynik_df
wynik_df <- data.frame(
  `Parameter` = names(wynik$par),
  `Value` = as.numeric(wynik$par)
)
wynik_df #parameters

############################################################
  

acceptance_rejection <- function(lambda, mu,p, n){ #acceptance rejection method for generating goals scored
  X_prob <- c(0,1,2)
  M <- max(sapply(X_prob, function(x){ return(tau(x,x, lambda, mu, p))}))
  x <- t(sapply(lambda, function(l){ rpois(10, lambda = l)}))
  y <- t(sapply(mu,  function(m){ rpois(10, lambda = m)}))
  U <- matrix(runif(10*length(lambda)), nrow = length(lambda), ncol = 10)
  lambda_matrix <- matrix(rep(lambda, times =10), nrow=length(lambda))
  mu_matrix <- matrix(rep(mu, times =10 ), nrow = length(mu))
  acceptance_war <- U <= t(mapply(function(i){ 
    tau(c(x[i,]), c(y[i,]), c(lambda_matrix[i,]), c(mu_matrix[i,]),p)
  },
  1:nrow(lambda_matrix)))/M
  chosen_points <- mapply(function(i) {
    ktore <- which(acceptance_war[i, ])[1] 
    return(c(x[i, ktore], y[i, ktore]))
    
  }, 1:nrow(x))
  points <- t(chosen_points)
  return(points)
}


##########################################
#       calculating present table        #
##########################################

validate_2 <- data1
validate_3 <- data1
colnames(validate_2) <- c('id', 'date','Team', 'Opponent', 'Score', 'OpponentScore')
colnames(validate_3) <- c('id','date','Opponent', 'Team', 'OpponentScore','Score')

goals_scored <- bind_rows(validate_2, validate_3) %>%
  group_by(Team) %>%
  summarise(goals_sc = sum(Score))


goals_lost <- bind_rows(validate_2, validate_3) %>%
  group_by(Team) %>%
  summarise(goals_lst = sum(OpponentScore))


number_results <- bind_rows(validate_2, validate_3) %>%
  mutate('Result' = ifelse(Score>OpponentScore, "W", ifelse(Score==OpponentScore, 'D', 'L'))) %>%
  select(-Opponent, -date, -id) %>%
  group_by(Team, Result) %>%
  summarise('Quantity' = n()) %>%
  pivot_wider(names_from = Result,
              values_from = Quantity)




table_now <- apply(number_results, 1, function(x){ as.numeric(x['W'])*3 + as.numeric(x['D'])*1} )

table_now <-cbind(number_results, "Points" = table_now) %>%
  inner_join(goals_scored, by = 'Team') %>%
  inner_join(goals_lost, by = 'Team') %>%
  mutate(Points = ifelse(Team == 'Lechia Gdansk', Points-5, Points))

table_now #present table


###################################

wynik_df_alfa <- wynik_df %>%
  filter(str_detect(Parameter,'alfa_')) %>%
  left_join(clubs, by = c('Parameter'="alfa_name")) %>%
  select(Parameter, Value, Team)
  
wynik_df_beta <- wynik_df %>%
  filter(str_detect(Parameter,'beta_')) %>%
  left_join(clubs, by = c('Parameter'="beta_name")) %>%
  select(Parameter, Value, Team)

wynik_notalfa_beta <- wynik_df %>%
  filter(!str_detect(Parameter, 'alfa_'),
         !str_detect(Parameter, 'beta_'))

wynik_df <- bind_rows(wynik_df_alfa, wynik_df_beta, wynik_notalfa_beta)



###################################



predict_data <- function(data){
  data1 <- data
  colnames(data1) <- c('Team', 'Opponent', 'TeamScore', 'OpponentScore')
  colnames(data) <- c( 'Opponent','Team', 'OpponentScore', 'TeamScore')
  whole_data <- bind_rows(data1, data) %>%
    mutate('Result'=ifelse(TeamScore>OpponentScore, "W", ifelse(TeamScore==OpponentScore, "D", "L")))
  data_wins_and_losses <- whole_data %>%
    group_by(Team,Result) %>%
    summarise(Quantity = n()) %>%
    pivot_wider(names_from = Result,
                values_from = Quantity,
                values_fill = 0)
  
  for(col in c("W","D","L")){
    if(!col %in% names(data_wins_and_losses)){
      data_wins_and_losses[[col]] <- 0
    }
  }
  goals_scored <- whole_data %>%
    group_by(Team) %>%
    summarise("goals_sc" = sum(TeamScore),
              'goals_lst' = sum(OpponentScore))
  
  whole_info <- data_wins_and_losses %>%
    inner_join(goals_scored, by = 'Team') 
  Points = apply(whole_info,1, function(x){ as.numeric(x['W'])*3+ as.numeric(x['D'])})
  
  whole_info <- cbind(whole_info, 'Points' = Points)
  
  return(whole_info)
}



equal_points <- function(tabela, results){ #function that helps us in case of the draw in points (which team should be higher)
  rowne <- tabela   %>%
    group_by(Points) %>%
    filter(n()>1) %>%
    group_split()
  
  tabela1 <- tabela %>%
    select(-D, -L, -Points, -Position)
  updates <- list()
  i <- 1
  for(x in rowne){ 
    
    rowne_positions <- x$Position
    int_games <- results %>%
      filter(homeTeam %in% x$Team, awayTeam %in% x$Team)
    mini_table <- predict_data(int_games) %>%
      mutate('Bilans_goals' = goals_sc-goals_lst)%>%
      inner_join(tabela1, by = 'Team') %>%
      mutate('Ogolny_bilans' = goals_sc.y - goals_lst.y) %>%
      arrange(desc(Points), desc(Bilans_goals),desc(Ogolny_bilans),desc(goals_sc.y), desc(W.y)) 
    
    mini_table$Position <- rowne_positions
    mini_table <- mini_table %>%
      select(Team, Position)
    
    
    updates[[i]] <- mini_table
    i <- i + 1
  } 
  updates <- bind_rows(updates)
  
  if(nrow(updates) == 0){
    return(tabela %>% arrange(Position))
  }
  tabela <- tabela %>%
    rows_update(updates, by = "Team")
  
  return(tabela %>%
           arrange((Position)))
  
}

###############################
#      season simulations     #
###############################

simulate_season <- function(times, calendar, model, games_played){   #simulating season
  gamma <- as.numeric(model %>%
                        filter(Parameter == 'gamma') %>%
                        select(Value))
  
  p <- as.numeric(model %>%
                    filter(Parameter == 'p') %>%
                    select(Value))
  positions <- matrix(0, ncol = times, nrow = 18)
  points <- matrix(0, ncol = times, nrow = 18)
  teams <- sort(union(unique(validate$homeTeam), unique(validate$awayTeam)))
  rownames(positions) <- teams
  rownames(points) <- teams
  calendar_all <- calendar %>%
    inner_join(wynik_df %>% filter(str_detect(Parameter, 'alfa_')), by = c('homeTeam' = 'Team'))%>%
    select(-Parameter) %>%
    rename(alfaHome = Value) %>%
    inner_join(wynik_df %>% filter(str_detect(Parameter, 'alfa_')), by = c('awayTeam' = 'Team'))%>%
    select(-Parameter) %>%
    rename(alfaAway = Value)%>%
    inner_join(wynik_df %>% filter(str_detect(Parameter, 'beta_')), by = c('homeTeam' = 'Team'))%>%
    select(-Parameter) %>%
    rename(betaHome = Value)%>%
    inner_join(wynik_df %>% filter(str_detect(Parameter, 'beta_')), by = c('awayTeam' = 'Team'))%>%
    select(-Parameter) %>%
    rename(betaAway = Value) %>%
    mutate(gamma = (wynik_df%>%filter(Parameter == 'gamma'))$Value)%>%
    mutate(lambda = alfaHome*betaAway*gamma,
           mu = alfaAway*betaHome)
  one_simulation <- function(dummy, calendar, calendar_all, games_played, 
                             p, teams, table_now){ #to help speed up simulations
    
    predicted_results <- acceptance_rejection(calendar_all$lambda, calendar_all$mu, p, nrow(calendar_all))
    
    colnames(predicted_results) <- c('homeScore', "awayScore")
    predicted_results <- cbind(calendar, predicted_results) %>%
      select(-date, -ď.żid) 
    
    all_games <- rbind(games_played, predicted_results)
    
    tabela2 <- predict_data(predicted_results)
    
    tabela_suma <- bind_rows(table_now, tabela2) %>%
      group_by(Team) %>%
      summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Points)) %>%
      mutate("Position" = seq(from =1, to = 18, by =1))
    
    
    tabela_suma <- equal_points(tabela_suma, all_games)
    list(
      positions = tabela_suma$Position[match(teams, tabela_suma$Team)],
      points    = tabela_suma$Points[match(teams, tabela_suma$Team)]
    )
  }
  
  #AI helped from there
  cl <- makeCluster(15)
  clusterExport(cl, c("tau", "acceptance_rejection", "predict_data", 
                      "equal_points", "table_now", "wynik_df"))
  clusterEvalQ(cl, library(dplyr))
  clusterEvalQ(cl, library(stringr))
  clusterEvalQ(cl, library(tidyr))
  
  results <- pblapply(1:times, one_simulation,
                      calendar = calendar,
                      calendar_all = calendar_all,
                      games_played = games_played,
                      p = p,
                      teams = teams,
                      table_now = table_now,
                      cl = cl)  
  stopCluster(cl)
  
  positions <- sapply(results, function(r) r$positions)
  points    <- sapply(results, function(r) r$points)
  rownames(positions) <- teams
  rownames(points)    <- teams
  return(list(positions, points))
}

set.seed(8)
simulated_positions <- simulate_season(100000, calendar, wynik_df, validate)
positions_simualted <- simulated_positions[[1]] #simulated positions
points_simulated <- simulated_positions[[2]] #simulated points


