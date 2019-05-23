## Dataset assuntos. Como alguns assuntos se repetem entre as áreas,
## optamos por ordená-los conforme a precedência da matéria.
## Entre direito ireito penal e direito civil não há precedência, mas smj,
## eles apenas se repetem e cartas de ordem e precatória. Isso pode ser resolvido
## na própria função.

ordem <- c(
  "287 - DIREITO PENAL",
  "899 - DIREITO CIVIL",
  "1209 - DIREITO PROCESSUAL PENAL",
  "8826 - DIREITO PROCESSUAL CIVIL E DO TRABALHO",
  "9985 - DIREITO ADMINISTRATIVO E OUTRAS MATÉRIAS DE DIREITO PÚBLICO",
  "14 - DIREITO TRIBUTÁRIO",
  "1156 - DIREITO DO CONSUMIDOR",
  "9633 - DIREITO DA CRIANÇA E DO ADOLESCENTE",
  "195 - DIREITO PREVIDENCIÁRIO",
  "864 - DIREITO DO TRABALHO",
  "7724 - REGISTROS PÚBLICOS",
  "0 - Assunto não Especificado",
  "1146 - DIREITO MARÍTIMO",
  "6191 - DIREITO INTERNACIONAL",
  "99999999 - ASSUNTOS ANTIGOS DO SAJ"
)

assuntos <- esaj::cjsg_table("subjects") %>%
  dplyr::group_by(.data$name0) %>%
  dplyr::group_map(~ {
    .x %>%
      dplyr::select(dplyr::starts_with("name")) %>%
      unlist() %>%
      tibble::enframe("id", "assunto")
  }) %>%
  dplyr::ungroup() %>%
  dplyr::select(area = .data$name0, .data$assunto) %>%
  dplyr::mutate(assunto = dplyr::coalesce(.data$assunto, .data$area)) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    assunto_ = stringr::str_remove(.data$assunto, "^\\d.+?\\- ") %>%
      abjutils::rm_accent() %>%
      stringr::str_to_lower() %>%
      stringr::str_squish()
  ) %>%
  dplyr::arrange(.data$assunto_) %>%
  dplyr::mutate(area = factor(.data$area, levels = !!ordem)) %>%
  dplyr::arrange(area)



readr::write_csv(assuntos, "data-raw/assuntos.csv")

usethis::use_data(assuntos, compress = "xz", overwrite = TRUE)
