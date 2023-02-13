module Auth.Protocol.OAuth exposing (..)

import Auth.Common exposing (..)
import Auth.HttpHelpers as HttpHelpers
import Browser.Navigation as Navigation
import Dict exposing (Dict)
import Http
import Json.Decode as Json
import Json.Decode.Pipeline exposing (..)
import List.Extra as List
import OAuth
import OAuth.AuthorizationCode as OAuth
import Process
import SHA1
import Task exposing (Task)
import Time
import Url exposing (Url)


onFrontendCallbackInit :
    { frontendModel | authFlow : Flow, authRedirectBaseUrl : Url }
    -> Auth.Common.MethodId
    -> Url
    -> Navigation.Key
    -> (Auth.Common.ToBackend -> Cmd frontendMsg)
    -> ( { frontendModel | authFlow : Flow, authRedirectBaseUrl : Url }, Cmd frontendMsg )
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
            , Cmd.none
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
            , Cmd.batch [ toBackendFn newCmds, clearUrl ]
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


initiateSignin sessionId baseUrl isDev config asBackendMsg now backendModel =
    let
        signedState =
            SHA1.toBase64 <|
                SHA1.fromString <|
                    (String.fromInt <| Time.posixToMillis <| now)
                        ++ "0x3vd7a"
                        ++ sessionId

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
    , Auth.Common.sleepTask isDev
        (asBackendMsg
            (AuthSigninInitiatedDelayed_
                sessionId
                (AuthInitiateSignin url)
            )
        )
    )


generateSigninUrl : Url -> Auth.Common.State -> Auth.Common.ConfigurationOAuth frontendMsg backendMsg frontendModel backendModel -> Url
generateSigninUrl baseUrl state configuration =
    let
        authorization =
            { clientId = configuration.clientId
            , redirectUri = baseUrl |> (\v -> { v | path = "/login/" ++ configuration.id ++ "/callback" })
            , scope = configuration.scope
            , state = Just state
            , url = configuration.authorizationEndpoint
            }
    in
    authorization
        |> OAuth.makeAuthorizationUrl


onAuthCallbackReceived sessionId clientId method receivedUrl code state now asBackendMsg backendModel =
    ( backendModel
    , validateCallbackToken method.clientId method.clientSecret method.tokenEndpoint receivedUrl code
        |> Task.andThen
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
                                |> Task.map (\userInfo -> ( userInfo, authToken ))

                        else
                            Task.fail <| Auth.Common.ErrAuthString "Invalid auth state. Please log in again or report this issue."

                    Nothing ->
                        Task.fail <| Auth.Common.ErrAuthString "Couldn't validate auth, please login again."
            )
        |> Task.attempt (Auth.Common.AuthSuccess sessionId clientId method.id now >> asBackendMsg)
    )


validateCallbackToken :
    String
    -> String
    -> Url
    -> Url
    -> OAuth.AuthorizationCode
    -> Task Auth.Common.Error OAuth.AuthenticationSuccess
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
    , headers = req.headers ++ [ Http.header "Accept" "application/json" ]
    , url = req.url
    , body = req.body
    , resolver = HttpHelpers.jsonResolver authenticationSuccessDecoder
    , timeout = req.timeout
    }
        |> Http.task
        |> Task.mapError parseAuthenticationResponseError


authenticationSuccessDecoder =
    -- The default decoder requires a token_type of 'bearer', but some OAuth imlementations (Shopify) don't have this
    -- OAuth.defaultAuthenticationSuccessDecoder
    Json.map5 OAuth.AuthenticationSuccess
        -- OAuth.defaultTokenDecoder
        tokenDecoder
        OAuth.defaultRefreshTokenDecoder
        OAuth.defaultExpiresInDecoder
        OAuth.defaultScopeDecoder
        idJwtDecoder


idJwtDecoder =
    Json.maybe <| Json.field "id_token" Json.string


tokenDecoder =
    Json.andThen (decoderFromJust "missing or invalid 'access_token' / 'token_type'") <|
        (Json.succeed OAuth.makeToken
            -- @TODO this is a bit dumb, need to add another type other than Bearer?
            |> optional "token_type" (Json.maybe Json.string) (Just "bearer")
            |> optional "access_token" (Json.maybe Json.string) Nothing
        )


decoderFromJust : String -> Maybe a -> Json.Decoder a
decoderFromJust msg =
    Maybe.map Json.succeed >> Maybe.withDefault (Json.fail msg)


parseAuthenticationResponse : Result Http.Error OAuth.AuthenticationSuccess -> Result Auth.Common.Error OAuth.AuthenticationSuccess
parseAuthenticationResponse res =
    case res of
        Err (Http.BadBody body) ->
            case Json.decodeString OAuth.defaultAuthenticationErrorDecoder body of
                Ok error ->
                    Err <| Auth.Common.ErrAuthentication error

                _ ->
                    Err <| Auth.Common.ErrHTTPGetAccessToken body

        Err httpErr ->
            Err <| Auth.Common.ErrHTTPGetAccessToken (HttpHelpers.httpErrorToString httpErr)

        Ok authenticationSuccess ->
            Ok authenticationSuccess


parseAuthenticationResponseError : Http.Error -> Auth.Common.Error
parseAuthenticationResponseError httpErr =
    case httpErr of
        Http.BadBody body ->
            case Json.decodeString OAuth.defaultAuthenticationErrorDecoder body of
                Ok error ->
                    Auth.Common.ErrAuthentication error

                _ ->
                    Auth.Common.ErrHTTPGetAccessToken (HttpHelpers.httpErrorToString httpErr)

        _ ->
            Auth.Common.ErrHTTPGetAccessToken (HttpHelpers.httpErrorToString httpErr)


makeToken : Auth.Common.MethodId -> OAuth.AuthenticationSuccess -> Time.Posix -> Auth.Common.Token
makeToken methodId authenticationSuccess now =
    { methodId = methodId
    , token = authenticationSuccess.token
    , created = now
    , expires =
        (Time.posixToMillis now
            + ((authenticationSuccess.expiresIn |> Maybe.withDefault 0) * 1000)
        )
            |> Time.millisToPosix
    }
