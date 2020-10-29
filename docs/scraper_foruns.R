library(tidyverse)

codigos <- tjsp::codigos
tjsp::autenticar()
cnj_sequencial<- JurisMiner::cnj_sequencial

cnj <- cnj_quantidade(50000,0,2019,8,26,696, tjsp::baixar_cpopg)


tjsp <- tjsp_quantidade(1,50000,2019,696,tjsp::baixar_cpopg)


url <- "https://esaj.tjsp.jus.br/cjpg/varasTreeSelect.do?campoId=varas&mostrarBotoesSelecaoRapida=true&conversationId="

s <- httr::GET(url)
writeBin(s$content,"data-raw/a.html")

conteudo <- httr::content(s)

foro <- xml2::xml_find_all(conteudo,"//li[@class=' open']//span[contains(text(),'Foro')]") %>%
        xml2::xml_attr("searchid")

foro2 <- xml2::xml_find_all(conteudo,"//li[@class=' open']//span[contains(text(),'Foro')]") %>%
  xml2::xml_text()

d <- tibble(foro, foro2)
d <- distinct(d)

codigos <- d %>%
        select(distribuidor = foro, descricao = foro2)

codigos <- codigos %>%
          mutate(distribuidor = as.integer(distribuidor))




