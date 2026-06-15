module Auth.Protocol.OAuth exposing (..)

import Auth.Common exposing (..)
import Auth.HttpHelpers as HttpHelpers
import Duration
import Effect.Browser.Navigation as Navigation
import Effect.Command as Command exposing (BackendOnly, Command, FrontendOnly)
import Effect.Http
import Effect.Lamdera exposing (SessionId, sessionIdToString)
import Effect.Task exposing (Task)
import Effect.Time
import Json.Decode as Json
import OAuth
import OAuth.AuthorizationCode as OAuth
import SHA1
import SeqDict as Dict exposing (SeqDict)
import Url exposing (Url)


onFrontendCallbackInit :
    { frontendModel | authFlow : Flow, authRedirectBaseUrl : Url }
    -> Auth.Common.MethodId
    -> Url
    -> Navigation.Key
    -> (Auth.Common.ToBackend -> Command FrontendOnly toMsg frontendMsg)
    -> ( { frontendModel | authFlow : Flow, authRedirectBaseUrl : Url }, Command FrontendOnly toMsg frontendMsg )
onFrontendCallbackInit model methodId origin navigationKey toBackendFn =
    let
        redirectUri =
            { origin | query = Nothing, fragment = Nothing }

        clearUrl =
            Navigation.replaceUrl navigationKey (Url.toString model.authRedirectBaseUrl)
    in
    case OAuth.parseCode origin of
        OAuth.Empty ->
            ( { model | authFlow = Idle }
            , Command.none
            )

        OAuth.Success { code, state } ->
            let
                state_ =
                    state |> Maybe.withDefault ""

                model_ =
                    { model | authFlow = Authorized code state_ }

                ( newModel, newCmds ) =
                    accessTokenRequested model_ methodId code state_
            in
            ( newModel
            , Command.batch [ toBackendFn newCmds, clearUrl ]
            )

        OAuth.Error error ->
            ( { model | authFlow = Errored <| ErrAuthorization error }
            , clearUrl
            )


accessTokenRequested :
    { frontendModel | authFlow : Flow, authRedirectBaseUrl : Url }
    -> Auth.Common.MethodId
    -> OAuth.AuthorizationCode
    -> Auth.Common.State
    -> ( { frontendModel | authFlow : Flow, authRedirectBaseUrl : Url }, Auth.Common.ToBackend )
accessTokenRequested model methodId code state =
    ( { model | authFlow = Authorized code state }
    , AuthCallbackReceived methodId model.authRedirectBaseUrl code state
    )


initiateSignin : Bool -> SessionId -> Url -> ConfigurationOAuth frontendMsg backendMsg frontendModel backendModel restriction toMsg -> (BackendMsg -> backendMsg) -> Effect.Time.Posix -> { b | pendingAuths : SeqDict Effect.Lamdera.SessionId PendingAuth } -> ( { b | pendingAuths : SeqDict Effect.Lamdera.SessionId PendingAuth }, Command BackendOnly toMsg backendMsg )
initiateSignin isDev sessionId baseUrl config asBackendMsg now backendModel =
    let
        signedState =
            SHA1.toBase64 <|
                SHA1.fromString <|
                    (String.fromInt <| Effect.Time.posixToMillis <| now)
                        -- @TODO this needs to be user-injected config
                        ++ "0x3vd7a"
                        ++ sessionIdToString sessionId

        newPendingAuth : PendingAuth
        newPendingAuth =
            { sessionId = sessionId
            , created = now
            , state = signedState
            }

        url =
            generateSigninUrl baseUrl signedState config
    in
    ( { backendModel
        | pendingAuths = backendModel.pendingAuths |> Dict.insert sessionId newPendingAuth
      }
    , Auth.Common.sleepTask
        isDev
        (asBackendMsg
            (AuthSigninInitiatedDelayed_
                sessionId
                (AuthInitiateSignin url)
            )
        )
    )


generateSigninUrl : Url -> Auth.Common.State -> Auth.Common.ConfigurationOAuth frontendMsg backendMsg frontendModel backendModel restriction toMsg -> Url
generateSigninUrl baseUrl state configuration =
    let
        queryAdjustedUrl =
            -- google auth is an example where, at time of writing, query parameters are not allowed in a login redirect url
            if configuration.allowLoginQueryParameters then
                baseUrl

            else
                { baseUrl | query = Nothing }

        authorization =
            { clientId = configuration.clientId
            , redirectUri = { queryAdjustedUrl | path = "/login/" ++ configuration.id ++ "/callback" }
            , scope = configuration.scope
            , state = Just state
            , url = configuration.authorizationEndpoint
            }
    in
    authorization
        |> OAuth.makeAuthorizationUrl


onAuthCallbackReceived : SessionId -> Effect.Lamdera.ClientId -> { a | clientId : String, clientSecret : String, tokenEndpoint : Url, id : MethodId, getUserInfo : OAuth.AuthenticationSuccess -> Task BackendOnly Error UserInfo } -> Url -> OAuth.AuthorizationCode -> String -> Effect.Time.Posix -> (BackendMsg -> backendMsg) -> { backendModel | pendingAuths : SeqDict SessionId PendingAuth } -> ( { backendModel | pendingAuths : SeqDict SessionId PendingAuth }, Command BackendOnly toMsg backendMsg )
onAuthCallbackReceived sessionId clientId method receivedUrl code state now asBackendMsg backendModel =
    ( backendModel
    , validateCallbackToken method.clientId method.clientSecret method.tokenEndpoint receivedUrl code
        |> Effect.Task.andThen
            (\authenticationResponse ->
                case backendModel.pendingAuths |> Dict.get sessionId of
                    Just pendingAuth ->
                        let
                            authToken =
                                Just (makeToken method.id authenticationResponse now)
                        in
                        if pendingAuth.state == state then
                            method.getUserInfo
                                authenticationResponse
                                |> Effect.Task.map (\userInfo -> ( userInfo, authToken ))

                        else
                            Effect.Task.fail <| Auth.Common.ErrAuthString "Invalid auth state. Please log in again or report this issue."

                    Nothing ->
                        Effect.Task.fail <| Auth.Common.ErrAuthString "Couldn't validate auth, please login again."
            )
        |> Effect.Task.attempt (Auth.Common.AuthSuccess sessionId clientId method.id now >> asBackendMsg)
    )


validateCallbackToken :
    String
    -> String
    -> Url
    -> Url
    -> OAuth.AuthorizationCode
    -> Effect.Task.Task BackendOnly Auth.Common.Error OAuth.AuthenticationSuccess
validateCallbackToken clientId clientSecret tokenEndpoint redirectUri code =
    let
        req =
            OAuth.makeTokenRequest (always ())
                { credentials =
                    { clientId = clientId
                    , secret = Just clientSecret
                    }
                , code = code
                , url = tokenEndpoint
                , redirectUri = { redirectUri | query = Nothing, fragment = Nothing }
                }
    in
    { method = req.method
    , headers = req.headers ++ [ Effect.Http.header "Accept" "application/json" ]
    , url = req.url
    , body = req.body
    , resolver = HttpHelpers.jsonResolver OAuth.defaultAuthenticationSuccessDecoder
    , timeout = req.timeout |> Maybe.map Duration.milliseconds
    }
        |> Effect.Http.task
        |> Effect.Task.mapError parseAuthenticationResponseError


parseAuthenticationResponse : Result Effect.Http.Error OAuth.AuthenticationSuccess -> Result Auth.Common.Error OAuth.AuthenticationSuccess
parseAuthenticationResponse res =
    case res of
        Err (Effect.Http.BadBody body) ->
            case Json.decodeString OAuth.defaultAuthenticationErrorDecoder body of
                Ok error ->
                    Err <| Auth.Common.ErrAuthentication error

                _ ->
                    Err Auth.Common.ErrHTTPGetAccessToken

        Err _ ->
            Err Auth.Common.ErrHTTPGetAccessToken

        Ok authenticationSuccess ->
            Ok authenticationSuccess


parseAuthenticationResponseError : Effect.Http.Error -> Auth.Common.Error
parseAuthenticationResponseError httpErr =
    case httpErr of
        Effect.Http.BadBody body ->
            case Json.decodeString OAuth.defaultAuthenticationErrorDecoder body of
                Ok error ->
                    Auth.Common.ErrAuthentication error

                _ ->
                    Auth.Common.ErrHTTPGetAccessToken

        _ ->
            Auth.Common.ErrHTTPGetAccessToken


makeToken : Auth.Common.MethodId -> OAuth.AuthenticationSuccess -> Effect.Time.Posix -> Auth.Common.Token
makeToken methodId authenticationSuccess now =
    { methodId = methodId
    , token = authenticationSuccess.token
    , created = now
    , expires =
        (Effect.Time.posixToMillis now
            + ((authenticationSuccess.expiresIn |> Maybe.withDefault 0) * 1000)
        )
            |> Effect.Time.millisToPosix
    }
