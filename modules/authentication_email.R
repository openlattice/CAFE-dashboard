# here you can write your own authentication function:
# i.e. who has access to the basics of the app?
#
# This example checks whether the user has the role
# "OpenLattice User Role" (every user has this role)
#
# You can add more authentication and data reads down
# in the app as well.


emails <- c(
    "jina@openlattice.com",
    "joke@openlattice.com",
    "cafe@openlattice.com",
    "shilpa@openlattice.com",
    "mollie@openlattice.com",
    "smcoyne@byu.edu",
    "lstockdale@byu.edu",
    "jradesky@umich.edu",
    'parentingtech@umich.edu',
    'aschalle@umich.edu',
    'vadktori@umich.edu',
    'kirkorian@wisc.edu',
    'earlylearning@georgetown.edu',
    'ckblackwell@northwestern.edu',
    'oab7@georgetown.edu',
    'rfb5@georgetown.edu',
    'snr30@georgetown.edu', 
    'roslatva@med.umich.edu'
)

custom_auth_function <- function(jwt, local_access = NA) {
    if (!is.na(local_access)) {
        r <- POST(
            'https://openlattice.auth0.com/userinfo',
            body = list('access_token' = local_access),
            content_type("application/json"),
            encode = 'json'
        )
        email = content(r)$email
        if (email %in% emails) {
            return(TRUE)
        }
    } else {
        role <-  "OpenLattice User Role"
        basepath = "https://api.openlattice.com"
        header_params <-
            unlist(list("Authorization" = paste("Bearer", jwt)))
        client <- ApiClient$new(defaultHeaders = header_params,
                                basePath = basepath)
        prinApi <- PrincipalApi$new(apiClient = client)
        all_roles = prinApi$get_current_roles()$title
        return(role %in% all_roles)
    }
    
}

redirect <- function(url) {
    loginurl <- paste0('window.location.replace("',
                       url,
                       '");')
    runjs(loginurl)
    
}

redirectback <- function() {
    runjs(
        'history.pushState("", document.title, window.location.pathname + window.location.search);'
    )
}

#######################
## SERVER COMPONENTS ##
#######################

authentication_email_server <-
    function(input,
             output,
             session,
             jwt) {
        ns <- session$ns
        
        observe({
            local_access = NA
            local_jwt = NA
            
            # define loginurl (redirect to go login)
            loginurl <- URLencode(
                paste0(
                    "https://openlattice.com/login/?redirectUrl=https://",
                    session$clientData$url_hostname,
                    session$clientData$url_pathname
                )
            )
            
            # define baseurl (redirect to strip url hash)
            redirectbackurl <- URLencode(
                paste0(
                    "https://",
                    session$clientData$url_hostname,
                    session$clientData$url_pathname
                )
            )
            
            # parse hash in url
            query <-
                parseQueryString(session$clientData$url_hash_initial)
            
            if ("id_token" %in% names(query)) {

                local_access <- query[['#access_token']]
                redirectback()
                
            } else {
                redirect(loginurl)
                
            }
            
            if (!custom_auth_function(local_jwt, local_access = local_access)) {
                redirect(loginurl)
            } else {
                runjs("console.log('login successful');")
                jwt(TRUE)
            }
            
            
            
        })
        
        return(jwt)
        
        
        
    }
