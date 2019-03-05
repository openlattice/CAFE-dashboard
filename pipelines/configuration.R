library(openlattice)
library(tidyverse)
library(auth0)
library(httr)
library(yaml)

get_master_jwt <- function() {
    secrets <- read_yaml("secrets.yaml")
    r <- POST (
        "https://openlattice.auth0.com/oauth/token",
        body = secrets$auth0config,
        encode = "json"
    )
    if (r$status_code == 200) {
        return (content(r)$access_token)
    } else {
        return (NULL)
    }
}

get_apis <- function(jwt, local = FALSE) {
    basepath = ifelse(local == TRUE,
                      "http://localhost:8080",
                      "https://api.openlattice.com")

    # setting up for user

    header_params <-
        unlist(list("Authorization" = paste("Bearer", jwt)))
    client <- ApiClient$new(defaultHeaders = header_params,
                            basePath = basepath)

    edmApi <- EdmApi$new(apiClient = client)
    dataApi <- DataApi$new(apiClient = client)
    searchApi <- SearchApi$new(apiClient = client)
    prinApi <- PrincipalApi$new(apiClient = client)
    authorizationsApi <- AuthorizationsApi$new(apiClient = client)

    if ("TimeUseDiary READ" %in% prinApi$get_current_roles()$title) {
        master = get_master_jwt()
        if (local == TRUE){
            print("You're not authorized to see all figures !")
            master_jwt <- jwt
        } else {
            master_jwt <- master
        }
    } else {
        print("You're not authorized to see this data !")
        return (NULL)
    }

    # setting up for master

    header_params <-
        unlist(list("Authorization" = paste("Bearer", master_jwt)))
    client <- ApiClient$new(defaultHeaders = header_params,
                            basePath = basepath)

    edmApiMaster <- EdmApi$new(apiClient = client)
    dataApiMaster <- DataApi$new(apiClient = client)
    searchApiMaster <- SearchApi$new(apiClient = client)
    prinApiMaster <- PrincipalApi$new(apiClient = client)
    authorizationsApiMaster <- AuthorizationsApi$new(apiClient = client)

    return (list (
        personal = list(
            edmApi = edmApi,
            dataApi = dataApi,
            searchApi = searchApi,
            prinApi = prinApi,
            authorizationsApi = authorizationsApi
        ),
        master = list(
            edmApi = edmApiMaster,
            dataApi = dataApiMaster,
            searchApi = searchApiMaster,
            prinApi = prinApiMaster,
            authorizationsApi = authorizationsApiMaster
        )
    ))

}
