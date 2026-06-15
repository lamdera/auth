module Auth.Method.OAuthGithub exposing (..)

import Auth.Common exposing (..)
import Auth.HttpHelpers as HttpHelpers
import Auth.Protocol.OAuth
import Base64.Encode as Base64
import Bytes exposing (Bytes)
import Bytes.Encode as Bytes
import Effect.Browser.Navigation as Navigation
import Effect.Command exposing (FrontendOnly)
import Effect.Http
import Effect.Task exposing (Task)
import Json.Decode as Json
import Json.Decode.Pipeline exposing (..)
import List.Extra as List
import OAuth
import OAuth.AuthorizationCode as OAuth
import Url exposing (Protocol(..), Url)
import Url.Builder


configuration :
    String
    -> String
    ->
        Method
            frontendMsg
            backendMsg
            { frontendModel | authFlow : Flow, authRedirectBaseUrl : Url }
            backendModel
            FrontendOnly
            toMsg
configuration clientId clientSecret =
    ProtocolOAuth
        { id = "OAuthGithub"
        , authorizationEndpoint = { defaultHttpsUrl | host = "github.com", path = "/login/oauth/authorize" }
        , tokenEndpoint = { defaultHttpsUrl | host = "github.com", path = "/login/oauth/access_token" }
        , logoutEndpoint = Home { returnPath = "/logout/OAuthGithub/callback" }
        , allowLoginQueryParameters = True
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
    -> Effect.Task.Task restriction Auth.Common.Error UserInfo
getUserInfo authenticationSuccess =
    getUserInfoTask authenticationSuccess
        |> Effect.Task.andThen
            (\userInfo ->
                if userInfo.email == "" then
                    fallbackGetEmailFromEmails authenticationSuccess userInfo

                else
                    Effect.Task.succeed userInfo
            )


fallbackGetEmailFromEmails : OAuth.AuthenticationSuccess -> UserInfo -> Effect.Task.Task restriction Auth.Common.Error UserInfo
fallbackGetEmailFromEmails authenticationSuccess userInfo =
    getUserEmailsTask authenticationSuccess
        |> Effect.Task.andThen
            (\userEmails ->
                case userEmails |> List.find (\v -> v.primary == True) of
                    Just record ->
                        Effect.Task.succeed { userInfo | email = record.email }

                    Nothing ->
                        Effect.Task.fail <|
                            HttpHelpers.customError
                                "Could not retrieve an email from Github profile or emails list."
            )
        |> Effect.Task.mapError (HttpHelpers.httpErrorToString >> Auth.Common.ErrAuthString)


getUserInfoTask : OAuth.AuthenticationSuccess -> Effect.Task.Task restriction Auth.Common.Error UserInfo
getUserInfoTask authenticationSuccess =
    Effect.Http.task
        { method = "GET"
        , headers = OAuth.useToken authenticationSuccess.token []
        , url = Url.toString { defaultHttpsUrl | host = "api.github.com", path = "/user" }
        , body = Effect.Http.emptyBody
        , resolver =
            HttpHelpers.jsonResolver
                (Json.succeed UserInfo
                    |> optional "email" Json.string ""
                    |> optional "name" decodeNonEmptyString Nothing
                    |> optional "login" decodeNonEmptyString Nothing
                )
        , timeout = Nothing
        }
        |> Effect.Task.mapError (HttpHelpers.httpErrorToString >> Auth.Common.ErrAuthString)


decodeNonEmptyString : Json.Decoder (Maybe String)
decodeNonEmptyString =
    Json.string |> Json.map nothingIfEmpty


type alias GithubEmail =
    { primary : Bool, email : String }


getUserEmailsTask : OAuth.AuthenticationSuccess -> Effect.Task.Task restriction Effect.Http.Error (List GithubEmail)
getUserEmailsTask authenticationSuccess =
    Effect.Http.task
        { method = "GET"
        , headers = OAuth.useToken authenticationSuccess.token []
        , url = Url.toString { defaultHttpsUrl | host = "api.github.com", path = "/user/emails" }
        , body = Effect.Http.emptyBody
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
