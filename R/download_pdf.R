#' download_pdf
#'
#' Download PDF of a decision
#'
#' Download PDF of a decision (second instance)
#'
#' @param codes character vector containing id of the cases, obtainable from
#'   \code{\link{cjsg}} function.
#' @param dest destination folder of the pdf file
#' @param giveup Give up after \code{giveup} failed transfers
#' @param wait seconds to wait between failed transfers
#' @param overwrite overwrite?
#' @param verbose print error messages
#'
#' @export
download_pdf <- function(codes, dest = '.', giveup = 10, wait = 5,
                         overwrite = FALSE, verbose = TRUE) {
  # codes <- '10510490'
  dir.create(dest, showWarnings = FALSE, recursive = TRUE)
  codes <- unique(codes)
  safe_download_pdf_one <- purrr::possibly(download_pdf_one, 'error', quiet = !verbose)
  pr <- progress::progress_bar$new(total = length(codes))
  files <- purrr::map_chr(codes, ~{
    result <- safe_download_pdf_one(.x, dest, overwrite)
    ntry <- 0
    while (result == 'error' && ntry < giveup) {
      ntry <- ntry + 1
      if (verbose) {
        msg <- 'Error downloading code=%s. Trying for the %sth time after %s seconds\n'
        message(sprintf(msg, .x, ntry, wait))
      }
      Sys.sleep(wait)
      url_base <- 'http://esaj.tjsp.jus.br/cjsg/getArquivo.do'
      httr::handle_reset(url_base)
      result <- safe_download_pdf_one(.x, dest, overwrite)
    }
    if (result == 'error' && verbose) {
      message(sprintf('gave up downloading code=%s', .x))
    }
    pr$tick()
    result
  })
  invisible(setNames(files, codes))
}

download_pdf_one <- function(cd_acordao, dest, ow) {
  pdf_file <- sprintf('%s/%s.pdf', dest, cd_acordao)
  if (file.exists(pdf_file) & !ow) return(pdf_file)
  url_base <- 'http://esaj.tjsp.jus.br/cjsg/getArquivo.do'
  query <- list(cdAcordao = cd_acordao)
  r0 <- httr::GET(url_base, query = query)
  if (is_pdf(r0)) {
    writeBin(httr::content(r0, 'raw'), con = pdf_file)
  } else {
    if (is_blocked(r0)) stop('Failed to download captcha-page.')
    if (has_captcha(r0)) {
      u_img <- 'http://esaj.tjsp.jus.br/cjsg/imagemCaptcha.do'
      u_aud <- 'http://esaj.tjsp.jus.br/cjsg/somCaptcha.do'
      r_img <- httr::GET(u_img)
      if (r_img$status_code != 200) stop('Failed to download captcha-image.')
      tmp <- tempfile('captcha', tmpdir = dest, fileext = '.mp3')
      r_aud <- httr::GET(u_aud, httr::write_disk(tmp))
      if (file.size(tmp) == 0) stop('Failed to download captcha-audio.')
      captcha <- captchaSajAudio::decifrar(tmp)
      file.remove(tmp)
    }
    query$vlCaptcha <- captcha
    r_final <- httr::GET(url_base, query = query)
    if (is_pdf(r_final)) {
      writeBin(httr::content(r_final, 'raw'), con = pdf_file)
    } else {
      stop('Failed to download pdf file.')
    }
  }
  invisible(pdf_file)
}

is_pdf <- function(r) {
  if (!r$status_code == 200) return(FALSE)
  stringr::str_detect(r$headers[['content-type']], 'application/pdf')
}

is_blocked <- function(r) {
  # not implemented yet
  FALSE
}

has_captcha <- function(r) {
  r %>%
    httr::content('text') %>%
    xml2::read_html() %>%
    rvest::html_node('#captcha') %>%
    length() %>%
    magrittr::is_greater_than(0)
}
