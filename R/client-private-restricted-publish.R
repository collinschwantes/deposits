# ---------------------------------------------------
# Private methods for restricted and publish API methods,
# including service-specific methods.
# ---------------------------------------------------

# ---------------------------------------------------
# ----------------- Restricted METHODS -----------------
# ---------------------------------------------------

#' @description Method to set embargo date
#'
#' @param embargo_date Date of expiry for embargo.
#' @return Updated client with embargo metadata
#' @noRd

depositsClient$set ("private", "set_restricted", function (access_conditions) {

    if (self$service == "zenodo") {

        self <- private$restrict_zenodo ( access_conditions)

    } else if (self$service == "figshare") {
        stop("Restricted access not yet available for figshare")
        # embargo_type <- match.arg (embargo_type)
        # if (embargo_type == "deposit") {
        #     embargo_type <- "article"
        # }
        # if (!is.null (embargo_reason)) {
        #     checkmate::assert_character (embargo_reason, len = 1L)
        # }
        # self <- private$embargo_figshare (
        #     embargo_date, embargo_type, embargo_reason
        # )
    }

    invisible (self)
})


#' @description Restricted method for Zenodo service
#'
#' Zenodo has no API methods for restricted access. The process is entirely specified by
#' metadata fields. This method will also update an access conditions if one has been
#' previously specified.
#'
#' @param access_conditions Character. Description of access conditions.
#' @return Updated client with restricted metadata
#' @noRd

depositsClient$set ("private", "restrict_zenodo", function ( access_conditions) {

    s_meta <- private$metadata_service$metadata

    if (!"access_right" %in% names (s_meta)) {

        s_meta$access_right <- "restricted"
        s_meta$access_conditions <- access_conditions

    } else {

        if (!s_meta$access_right %in% c ("closed","open", "embargoed")) {
            stop (
                "deposit already has 'access_right' = [",
                s_meta$access_right, "]",
                .call = FALSE
            )
        }
        s_meta$access_right <- "restricted"
        s_meta$access_conditions <- access_conditions
    }

    private$metadata_service$metadata <- s_meta

    # http methods to update deposit:
    url <- paste0 (get_service_url (self), "/", self$id)

    req <- create_httr2_helper (url, self$headers$Authorization, "PUT")
    req <- httr2::req_body_json (req, data = private$metadata_service)

    resp <- httr2::req_perform (req)

    self$hostdata <- httr2::resp_body_json (resp)

    invisible (self)
})
