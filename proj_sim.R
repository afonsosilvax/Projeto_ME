#install.packages("simmer")
library(simmer)
`%!in%` <- function(x, y) !(x %in% y) # para poder usar i %!in%


consulta <- sample(0:2, 1, replace = T, prob = c(0.30, 0.60, 0.10))
# 0-sem consulta, 1-consulta telefónica, 2-consulta presencial
sem_consulta <- ifelse(consulta==0, T, F)
tipo_consulta<-c("telefonica", "presencial")

# Funções
chegada_contactos <- function() {set.seed(2023)
  rexp(1, 1/40)}

telefonemas <- function() {
  telefonema <- c()
  set.seed(2023)
  for(val in chegada_contactos()) {
    if (rnorm(1)<=0.85) {
      telefonema <- c(telefonema, val)
    }
  }
  return(telefonema)
}

mensagens <- function() {
  mensagem <- c()
  for(val in chegada_contactos()) {
    if (val %!in% telefonemas()) {
      mensagem <- c(mensagem, val)
    }
  }
  return(mensagem)
}
  
triagem <- function() rnorm(1, 4, 1)
tomada_decisao <- function() rnorm(1, 1, 0.25)
marcacao_consulta <- function() rnorm(1, 1, 0.25)


contactos <- trajectory("gestao_contactos") %>%
  log_("vou agora ser atendido") %>%
  seize("administrativo", 2) %>%
  timeout(triagem) %>%
  release("administrativo") %>%
  log_("já fui atendido")
  
  
  #trajectory("gestao_contactos") %>%
  #renege_in(5,out = trajectory("Rejeita chamada") %>%
              #log_("Desliga chamada"))

  #seize("administrativo", 2) %>%
  #renege_abort()%>%
  #timeout() %>%
  #set_attribute("tipo_contato", function() ifelse(runif(1) < 0.85, "telefonica", "eletronica")) %>%
  #set_attribute("tipo_consulta", function() ifelse(tipo_consulta<=0.3, "Sem consulta", ifelse(tipo_consulta>0.3 & tipo_consulta <= 90, "telefonica", "Presencial")) |>
  #timeout(function() ifelse(get_attribute(env, "tipo_consullta") == "telefonica" |"presencial", rnorm(1, mean = 1, sd = 0.25)+rnorm(1, mean = 1, sd = 0.25), 0)) %>%
  #release("administrativos")
  #log_("Já fui atendido")
  
env <- simmer("servico") %>%
  add_resource("administrativos", 2)%>%
  add_generator("Telefonema", telefonema, at(telefonemas)) %>%
  add_generator("Mensagem", mensagem, at(mensagens))
  