module Auth.Flow exposing (..)

import Auth.Common exposing (LogoutEndpointConfig(..), MethodId, ToBackend(..))
import Auth.Method.EmailMagicLink
import Auth.Protocol.OAuth
import Effect.Browser.Navigation as Navigation
import Effect.Command as Command exposing (BackendOnly, Command, FrontendOnly)
import Effect.Lamdera exposing (ClientId, SessionId, clientIdToString, sessionIdFromString)
import Effect.Task
import Effect.Time
import List.Extra as List
import SeqDict as Dict exposing (SeqDict)
import Url exposing (Protocol(..), Url)
import Url.Builder exposing (QueryParameter)


init :
    { frontendModel | authFlow : Auth.Common.Flow, authRedirectBaseUrl : Url }
    -> Auth.Common.MethodId
    -> Url
    -> Navigation.Key
    -> (Auth.Common.ToBackend -> Command FrontendOnly toMsg frontendMsg)
    ->
        ( { frontendModel | authFlow : Auth.Common.Flow, authRedirectBaseUrl : Url }
        , Command FrontendOnly toMsg frontendMsg
        )
init model methodId origin navigationKey toBackendFn =
    case methodId of
        "EmailMagicLink" ->
            Auth.Method.EmailMagicLink.onFrontendCallbackInit model methodId origin navigationKey toBackendFn

        "OAuthGithub" ->
            Auth.Protocol.OAuth.onFrontendCallbackInit model methodId origin navigationKey toBackendFn

        "OAuthGoogle" ->
            Auth.Protocol.OAuth.onFrontendCallbackInit model methodId origin navigationKey toBackendFn

        "OAuthAuth0" ->
            Auth.Protocol.OAuth.onFrontendCallbackInit model methodId origin navigationKey toBackendFn

        _ ->
            let
                clearUrl =
                    Navigation.replaceUrl navigationKey (Url.toString model.authRedirectBaseUrl)
            in
            ( { model | authFlow = Auth.Common.Errored <| Auth.Common.ErrAuthString ("Unsupported auth method: " ++ methodId) }
            , clearUrl
            )


onFrontendLogoutCallback navigationMsg =
    navigationMsg


updateFromFrontend { asBackendMsg } clientId sessionId authToBackend model =
    case authToBackend of
        Auth.Common.AuthSigninInitiated params ->
            ( model
            , withCurrentTime
                (\now ->
                    asBackendMsg <|
                        Auth.Common.AuthSigninInitiated_
                            { sessionId = sessionId
                            , clientId = clientId
                            , methodId = params.methodId
                            , baseUrl = params.baseUrl
                            , now = now
                            , username = params.username
                            }
                )
            )

        Auth.Common.AuthCallbackReceived methodId receivedUrl code state ->
            ( model
            , Effect.Time.now
                |> Effect.Task.perform
                    (\now ->
                        asBackendMsg <|
                            Auth.Common.AuthCallbackReceived_
                                sessionId
                                clientId
                                methodId
                                receivedUrl
                                code
                                state
                                now
                    )
            )

        Auth.Common.AuthRenewSessionRequested ->
            ( model
            , Effect.Time.now
                |> Effect.Task.perform
                    (\_ ->
                        asBackendMsg <|
                            Auth.Common.AuthRenewSession sessionId clientId
                    )
            )

        Auth.Common.AuthLogoutRequested ->
            ( model
            , Effect.Time.now
                |> Effect.Task.perform
                    (\_ ->
                        asBackendMsg <|
                            Auth.Common.AuthLogout sessionId clientId
                    )
            )


type alias BackendUpdateConfig frontendMsg backendMsg toFrontend frontendModel backendModel toMsg =
    { asToFrontend : Auth.Common.ToFrontend -> toFrontend
    , asBackendMsg : Auth.Common.BackendMsg -> backendMsg
    , sendToFrontend :
        SessionId
        -> toFrontend
        -> Command BackendOnly toMsg backendMsg
    , backendModel : { backendModel | pendingAuths : SeqDict SessionId Auth.Common.PendingAuth }
    , loadMethod :
        Auth.Common.MethodId
        -> Maybe (Auth.Common.Method frontendMsg backendMsg frontendModel backendModel BackendOnly toMsg)
    , handleAuthSuccess :
        SessionId
        -> ClientId
        -> Auth.Common.UserInfo
        -> MethodId
        -> Maybe Auth.Common.Token
        -> Effect.Time.Posix
        -> ( { backendModel | pendingAuths : SeqDict SessionId Auth.Common.PendingAuth }, Command BackendOnly toMsg backendMsg )
    , renewSession : SessionId -> ClientId -> backendModel -> ( backendModel, Command BackendOnly toMsg backendMsg )
    , logout : SessionId -> ClientId -> backendModel -> ( backendModel, Command BackendOnly toMsg backendMsg )
    , isDev : Bool
    }


backendUpdate :
    BackendUpdateConfig frontendMsg backendMsg toFrontend frontendModel { backendModel | pendingAuths : SeqDict SessionId Auth.Common.PendingAuth } toMsg
    -> Auth.Common.BackendMsg
    -> ( { backendModel | pendingAuths : SeqDict SessionId Auth.Common.PendingAuth }, Command BackendOnly toMsg backendMsg )
backendUpdate { asToFrontend, asBackendMsg, sendToFrontend, backendModel, loadMethod, handleAuthSuccess, renewSession, logout, isDev } authBackendMsg =
    let
        authError str =
            asToFrontend (Auth.Common.AuthError (Auth.Common.ErrAuthString str))

        withMethod :
            Auth.Common.MethodId
            -> SessionId
            ->
                (Auth.Common.Method frontendMsg backendMsg frontendModel { backendModel | pendingAuths : SeqDict SessionId Auth.Common.PendingAuth } BackendOnly toMsg
                 ->
                    ( { backendModel | pendingAuths : SeqDict SessionId Auth.Common.PendingAuth }
                    , Command BackendOnly toMsg backendMsg
                    )
                )
            ->
                ( { backendModel | pendingAuths : SeqDict SessionId Auth.Common.PendingAuth }
                , Command BackendOnly toMsg backendMsg
                )
        withMethod methodId clientId fn =
            case loadMethod methodId of
                Nothing ->
                    ( backendModel
                    , sendToFrontend clientId <| authError ("Unsupported auth method: " ++ methodId)
                    )

                Just method ->
                    fn method
    in
    case authBackendMsg of
        Auth.Common.AuthSigninInitiated_ { sessionId, clientId, methodId, baseUrl, now, username } ->
            withMethod methodId
                (clientId |> clientIdToString |> sessionIdFromString)
                (\method ->
                    case method of
                        Auth.Common.ProtocolEmailMagicLink config ->
                            config.initiateSignin sessionId clientId backendModel { username = username } now

                        Auth.Common.ProtocolOAuth config ->
                            Auth.Protocol.OAuth.initiateSignin isDev sessionId baseUrl config asBackendMsg now backendModel
                )

        Auth.Common.AuthSigninInitiatedDelayed_ sessionId initiateMsg ->
            ( backendModel, sendToFrontend sessionId (asToFrontend initiateMsg) )

        Auth.Common.AuthCallbackReceived_ sessionId clientId methodId receivedUrl code state now ->
            withMethod methodId
                (clientId |> clientIdToString |> sessionIdFromString)
                (\method ->
                    case method of
                        Auth.Common.ProtocolEmailMagicLink config ->
                            config.onAuthCallbackReceived sessionId clientId receivedUrl code state now asBackendMsg backendModel

                        Auth.Common.ProtocolOAuth config ->
                            Auth.Protocol.OAuth.onAuthCallbackReceived sessionId clientId config receivedUrl code state now asBackendMsg backendModel
                )

        Auth.Common.AuthSuccess sessionId clientId methodId now res ->
            let
                removeSession backendModel_ =
                    { backendModel_ | pendingAuths = backendModel_.pendingAuths |> Dict.remove sessionId }
            in
            withMethod methodId
                (clientId |> clientIdToString |> sessionIdFromString)
                (\_ ->
                    case res of
                        Ok ( userInfo, authToken ) ->
                            handleAuthSuccess sessionId clientId userInfo methodId authToken now
                                |> Tuple.mapFirst removeSession

                        Err err ->
                            ( backendModel, sendToFrontend sessionId (asToFrontend <| Auth.Common.AuthError err) )
                )

        Auth.Common.AuthRenewSession sessionId clientId ->
            renewSession sessionId clientId backendModel

        Auth.Common.AuthLogout sessionId clientId ->
            logout sessionId clientId backendModel


signInRequested :
    Auth.Common.MethodId
    -> { frontendModel | authFlow : Auth.Common.Flow, authRedirectBaseUrl : Url }
    -> Maybe String
    -> ( { frontendModel | authFlow : Auth.Common.Flow, authRedirectBaseUrl : Url }, Auth.Common.ToBackend )
signInRequested methodId model username =
    ( { model | authFlow = Auth.Common.Requested methodId }
    , Auth.Common.AuthSigninInitiated { methodId = methodId, baseUrl = model.authRedirectBaseUrl, username = username }
    )


signOutRequested :
    Maybe LogoutEndpointConfig
    -> List QueryParameter
    -> { a | authFlow : Auth.Common.Flow, authLogoutReturnUrlBase : Url }
    ->
        ( { a | authFlow : Auth.Common.Flow, authLogoutReturnUrlBase : Url }
        , Command FrontendOnly toMsg msg
        )
signOutRequested maybeUrlConfig callBackQueries model =
    ( { model | authFlow = Auth.Common.Idle }
    , case maybeUrlConfig of
        Just (Tenant urlConfig) ->
            Navigation.load <|
                Url.toString urlConfig.url
                    ++ Url.toString model.authLogoutReturnUrlBase
                    ++ urlConfig.returnPath
                    ++ Url.Builder.toQuery callBackQueries

        Just (Home homeUrlConfig) ->
            Navigation.load <|
                Url.toString model.authLogoutReturnUrlBase
                    ++ homeUrlConfig.returnPath
                    ++ Url.Builder.toQuery callBackQueries

        Nothing ->
            Navigation.load <|
                Url.toString model.authLogoutReturnUrlBase
                    ++ Url.Builder.toQuery callBackQueries
    )


startProviderSignin :
    Url
    -> { frontendModel | authFlow : Auth.Common.Flow }
    -> ( { frontendModel | authFlow : Auth.Common.Flow }, Command FrontendOnly toMsg msg )
startProviderSignin url model =
    ( { model | authFlow = Auth.Common.Pending }
    , Navigation.load (Url.toString url)
    )


setError :
    { frontendModel | authFlow : Auth.Common.Flow }
    -> Auth.Common.Error
    -> ( { frontendModel | authFlow : Auth.Common.Flow }, Command restriction toMsg msg )
setError model err =
    setAuthFlow model <| Auth.Common.Errored err


setAuthFlow :
    { frontendModel | authFlow : Auth.Common.Flow }
    -> Auth.Common.Flow
    -> ( { frontendModel | authFlow : Auth.Common.Flow }, Command restriction toMsg msg )
setAuthFlow model flow =
    ( { model | authFlow = flow }, Command.none )


errorToString : Auth.Common.Error -> String
errorToString error =
    case error of
        Auth.Common.ErrStateMismatch ->
            "ErrStateMismatch"

        Auth.Common.ErrAuthorization _ ->
            "ErrAuthorization"

        Auth.Common.ErrAuthentication _ ->
            "ErrAuthentication"

        Auth.Common.ErrHTTPGetAccessToken ->
            "ErrHTTPGetAccessToken"

        Auth.Common.ErrHTTPGetUserInfo ->
            "ErrHTTPGetUserInfo"

        Auth.Common.ErrAuthString err ->
            err


withCurrentTime fn =
    Effect.Time.now |> Effect.Task.perform fn


methodLoaderFrontend :
    List
        (Auth.Common.Method
            frontendMsg
            backendMsg
            frontendModel
            backendModel
            FrontendOnly
            toBackend
        )
    -> Auth.Common.MethodId
    ->
        Maybe
            (Auth.Common.Method
                frontendMsg
                backendMsg
                frontendModel
                backendModel
                FrontendOnly
                toBackend
            )
methodLoaderFrontend methods methodId =
    methods
        |> List.find
            (\cfg ->
                case cfg of
                    Auth.Common.ProtocolEmailMagicLink method ->
                        method.id == methodId

                    Auth.Common.ProtocolOAuth method ->
                        method.id == methodId
            )


methodLoaderBackend :
    List
        (Auth.Common.Method
            frontendMsg
            backendMsg
            frontendModel
            backendModel
            BackendOnly
            toBackend
        )
    -> Auth.Common.MethodId
    -> Maybe (Auth.Common.Method frontendMsg backendMsg frontendModel backendModel BackendOnly toBackend)
methodLoaderBackend methods methodId =
    methods
        |> List.find
            (\cfg ->
                case cfg of
                    Auth.Common.ProtocolEmailMagicLink method ->
                        method.id == methodId

                    Auth.Common.ProtocolOAuth method ->
                        method.id == methodId
            )


findMethod :
    Auth.Common.MethodId
    -> Auth.Common.Config frontendMsg toBackend backendMsg toFrontend frontendModel backendModel toMsg
    -> Maybe (Auth.Common.Method frontendMsg backendMsg frontendModel backendModel FrontendOnly toMsg)
findMethod methodId config =
    methodLoaderFrontend config.methods methodId
