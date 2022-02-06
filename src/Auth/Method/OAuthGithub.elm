module Auth.Method.OAuthGithub exposing (..)

import Auth.Common exposing (..)
import Auth.Protocol.OAuth
import Base64.Encode as Base64
import Bytes exposing (Bytes)
import Bytes.Encode as Bytes
import Http
import HttpHelpers
import Json.Decode as Json
import Json.Decode.Pipeline exposing (..)
import List.Extra as List
import OAuth
import OAuth.AuthorizationCode as OAuth
import Task exposing (Task)
import Url exposing (Protocol(..), Url)
import Url.Builder


configuration :
    String
    -> String
    ->
        Configuration
            frontendMsg
            backendMsg
            { frontendModel | authFlow : Flow, authRedirectBaseUrl : Url }
            backendModel
configuration clientId clientSecret =
    ProtocolOAuth
        { id = "OAuthGithub"
        , authorizationEndpoint = { defaultHttpsUrl | host = "github.com", path = "/login/oauth/authorize" }
        , tokenEndpoint = { defaultHttpsUrl | host = "github.com", path = "/login/oauth/access_token" }
        , clientId = clientId
        , clientSecret = clientSecret
        , scope = [ "read:user", "user:email" ]
        , getUserInfo = getUserInfo
        , onFrontendCallbackInit = Auth.Protocol.OAuth.onFrontendCallbackInit
        , placeholder = \x -> ()

        -- , onAuthCallbackReceived = Debug.todo "onAuthCallbackReceived"
        }


getUserInfo :
    OAuth.AuthenticationSuccess
    -> Task Auth.Common.Error UserInfo
getUserInfo authenticationSuccess =
    getUserInfoTask authenticationSuccess
        |> Task.andThen
            (\userInfo ->
                if userInfo.email == "" then
                    fallbackGetEmailFromEmails authenticationSuccess userInfo

                else
                    Task.succeed userInfo
            )


fallbackGetEmailFromEmails : OAuth.AuthenticationSuccess -> UserInfo -> Task Auth.Common.Error UserInfo
fallbackGetEmailFromEmails authenticationSuccess userInfo =
    getUserEmailsTask authenticationSuccess
        |> Task.andThen
            (\userEmails ->
                case userEmails |> List.find (\v -> v.primary == True) of
                    Just record ->
                        Task.succeed { userInfo | email = record.email }

                    Nothing ->
                        Task.fail <|
                            HttpHelpers.customError
                                "Could not retrieve an email from Github profile or emails list."
            )
        |> Task.mapError (Auth.Common.ErrAuthString << HttpHelpers.httpErrorToString)


getUserInfoTask : OAuth.AuthenticationSuccess -> Task Auth.Common.Error UserInfo
getUserInfoTask authenticationSuccess =
    Http.task
        { method = "GET"
        , headers = OAuth.useToken authenticationSuccess.token []
        , url = Url.toString { defaultHttpsUrl | host = "api.github.com", path = "/user" }
        , body = Http.emptyBody
        , resolver =
            HttpHelpers.jsonResolver
                (Json.succeed UserInfo
                    |> required "name" Json.string
                    |> optional "email" Json.string ""
                )
        , timeout = Nothing
        }
        |> Task.mapError (Auth.Common.ErrAuthString << HttpHelpers.httpErrorToString)


type alias GithubEmail =
    { primary : Bool, email : String }


getUserEmailsTask : OAuth.AuthenticationSuccess -> Task Http.Error (List GithubEmail)
getUserEmailsTask authenticationSuccess =
    Http.task
        { method = "GET"
        , headers = OAuth.useToken authenticationSuccess.token []
        , url = Url.toString { defaultHttpsUrl | host = "api.github.com", path = "/user/emails" }
        , body = Http.emptyBody
        , resolver =
            HttpHelpers.jsonResolver
                (Json.list
                    (Json.map2 GithubEmail
                        (Json.field "primary" Json.bool)
                        (Json.field "email" Json.string)
                    )
                )
        , timeout = Nothing
        }
