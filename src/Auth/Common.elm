module Auth.Common exposing (..)

import Base64.Encode as Base64
import Bytes exposing (Bytes)
import Bytes.Encode as Bytes
import Duration
import Effect.Browser.Navigation
import Effect.Command exposing (BackendOnly, Command, FrontendOnly)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Process
import Effect.Task
import Effect.Time
import OAuth
import OAuth.AuthorizationCode as OAuth
import Url exposing (Protocol(..), Url)


type alias Config frontendMsg toBackend backendMsg toFrontend frontendModel backendModel toMsg =
    { toBackend : ToBackend -> toBackend
    , toFrontend : ToFrontend -> toFrontend
    , backendMsg : BackendMsg -> backendMsg
    , sendToFrontend : SessionId -> toFrontend -> Command BackendOnly toFrontend backendMsg
    , sendToBackend : toBackend -> Command FrontendOnly toBackend frontendMsg
    , methods : List (Method frontendMsg backendMsg frontendModel backendModel FrontendOnly toMsg)
    , renewSession : SessionId -> ClientId -> backendModel -> ( backendModel, Command BackendOnly toMsg backendMsg )
    }


type Method frontendMsg backendMsg frontendModel backendModel restriction toMsg
    = ProtocolOAuth (ConfigurationOAuth frontendMsg backendMsg frontendModel backendModel restriction toMsg)
    | ProtocolEmailMagicLink (ConfigurationEmailMagicLink frontendMsg backendMsg frontendModel backendModel restriction toMsg)


type alias ConfigurationEmailMagicLink frontendMsg backendMsg frontendModel backendModel restriction toMsg =
    { id : String
    , initiateSignin :
        SessionId
        -> ClientId
        -> backendModel
        -> { username : Maybe String }
        -> Effect.Time.Posix
        -> ( backendModel, Command BackendOnly toMsg backendMsg )
    , onFrontendCallbackInit :
        frontendModel
        -> MethodId
        -> Url
        -> Effect.Browser.Navigation.Key
        -> (ToBackend -> Command restriction toMsg frontendMsg)
        -> ( frontendModel, Command restriction toMsg frontendMsg )
    , onAuthCallbackReceived :
        SessionId
        -> ClientId
        -> Url
        -> AuthCode
        -> State
        -> Effect.Time.Posix
        -> (BackendMsg -> backendMsg)
        -> backendModel
        -> ( backendModel, Command BackendOnly toMsg backendMsg )
    , placeholder : frontendMsg -> backendMsg -> frontendModel -> backendModel -> ()
    }


type alias ConfigurationOAuth frontendMsg backendMsg frontendModel backendModel restriction toMsg =
    { id : String
    , authorizationEndpoint : Url
    , tokenEndpoint : Url
    , logoutEndpoint : LogoutEndpointConfig
    , allowLoginQueryParameters : Bool
    , clientId : String
    , clientSecret : String
    , scope : List String
    , getUserInfo : OAuth.AuthenticationSuccess -> Effect.Task.Task restriction Error UserInfo
    , onFrontendCallbackInit :
        frontendModel
        -> MethodId
        -> Url
        -> Effect.Browser.Navigation.Key
        -> (ToBackend -> Command FrontendOnly toMsg frontendMsg)
        -> ( frontendModel, Command FrontendOnly toMsg frontendMsg )
    , placeholder : ( backendModel, backendMsg ) -> ()
    }


type alias SessionIdString =
    String


type FrontendMsg
    = AuthSigninRequested Provider


type ToBackend
    = AuthSigninInitiated { methodId : MethodId, baseUrl : Url, username : Maybe String }
    | AuthCallbackReceived MethodId Url AuthCode State
    | AuthRenewSessionRequested
    | AuthLogoutRequested


type BackendMsg
    = AuthSigninInitiated_ { sessionId : SessionId, clientId : ClientId, methodId : MethodId, baseUrl : Url, now : Effect.Time.Posix, username : Maybe String }
    | AuthSigninInitiatedDelayed_ SessionId ToFrontend
    | AuthCallbackReceived_ SessionId ClientId MethodId Url String String Effect.Time.Posix
    | AuthSuccess SessionId ClientId MethodId Effect.Time.Posix (Result Error ( UserInfo, Maybe Token ))
    | AuthRenewSession SessionId ClientId
    | AuthLogout SessionId ClientId


type ToFrontend
    = AuthInitiateSignin Url
    | AuthError Error
    | AuthSessionChallenge AuthChallengeReason


type AuthChallengeReason
    = AuthSessionMissing
    | AuthSessionInvalid
    | AuthSessionExpired
    | AuthSessionLoggedOut


type alias Token =
    { methodId : MethodId
    , token : OAuth.Token
    , created : Effect.Time.Posix
    , expires : Effect.Time.Posix
    }


type LogoutEndpointConfig
    = Home { returnPath : String }
    | Tenant { url : Url, returnPath : String }


type Provider
    = EmailMagicLink
    | OAuthGithub
    | OAuthGoogle
    | OAuthAuth0


type Flow
    = Idle
    | Requested MethodId
    | Pending
    | Authorized AuthCode String
    | Authenticated OAuth.Token
    | Done UserInfo
    | Errored Error


type Error
    = ErrStateMismatch
    | ErrAuthorization OAuth.AuthorizationError
    | ErrAuthentication OAuth.AuthenticationError
    | ErrHTTPGetAccessToken
    | ErrHTTPGetUserInfo
      -- Lazy string error until we classify everything nicely
    | ErrAuthString String


type alias State =
    String


type alias MethodId =
    String


type alias AuthCode =
    String


type alias UserInfo =
    { email : String
    , name : Maybe String
    , username : Maybe String
    }


type alias PendingAuth =
    { created : Effect.Time.Posix
    , sessionId : SessionId
    , state : String
    }


type alias PendingEmailAuth =
    { created : Effect.Time.Posix
    , sessionId : SessionId
    , username : String
    , fullname : String
    , token : String
    }



--
-- Helpers
--


toBytes : List Int -> Bytes
toBytes =
    List.map Bytes.unsignedInt8 >> Bytes.sequence >> Bytes.encode


base64 : Bytes -> String
base64 =
    Base64.bytes >> Base64.encode


convertBytes : List Int -> { state : String }
convertBytes =
    toBytes >> base64 >> (\state -> { state = state })


defaultHttpsUrl : Url
defaultHttpsUrl =
    { protocol = Https
    , host = ""
    , path = ""
    , port_ = Nothing
    , query = Nothing
    , fragment = Nothing
    }


sleepTask isDev msg =
    -- Because in dev the backendmodel is only persisted every 2 seconds, we need to
    -- make sure we sleep a little before a redirect otherwise we won't have our
    -- persisted state.
    (if isDev then
        Effect.Process.sleep (Duration.milliseconds 3000)

     else
        Effect.Process.sleep (Duration.milliseconds 0)
    )
        |> Effect.Task.perform (always msg)


nothingIfEmpty s =
    let
        trimmed =
            String.trim s
    in
    if trimmed == "" then
        Nothing

    else
        Just trimmed



-- Lamdera aliases
