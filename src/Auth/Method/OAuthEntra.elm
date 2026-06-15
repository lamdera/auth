module Auth.Method.OAuthEntra exposing (..)

import Auth.Common exposing (..)
import Auth.HttpHelpers as HttpHelpers
import Auth.Protocol.OAuth
import Dict exposing (Dict)
import Http
import JWT exposing (..)
import JWT.JWS as JWS
import Json.Decode as Json
import OAuth
import OAuth.AuthorizationCode as OAuth
import Task exposing (Task)
import Url exposing (Protocol(..), Url)


configuration :
    String
    -> String
    -> String
    ->
        Method
            frontendMsg
            backendMsg
            { frontendModel | authFlow : Flow, authRedirectBaseUrl : Url }
            backendModel
configuration clientId clientSecret tenantId =
    ProtocolOAuth
        { id = "OAuthEntra"
        , authorizationEndpoint =
            { defaultHttpsUrl
                | host = "login.microsoftonline.com"
                , path = "/" ++ tenantId ++ "/oauth2/v2.0/authorize"
            }
        , tokenEndpoint =
            { defaultHttpsUrl
                | host = "login.microsoftonline.com"
                , path = "/" ++ tenantId ++ "/oauth2/v2.0/token"
            }
        , logoutEndpoint =
            Tenant
                { url =
                    { defaultHttpsUrl
                        | host = "login.microsoftonline.com"
                        , path = "/" ++ tenantId ++ "/oauth2/v2.0/logout"
                        , query = Just "post_logout_redirect_uri="
                    }
                , returnPath = "/logout/OAuthEntra/callback"
                }
        , allowLoginQueryParameters = True
        , clientId = clientId
        , clientSecret = clientSecret
        , scope = [ "openid email profile" ]
        , getUserInfo = getUserInfo
        , onFrontendCallbackInit = Auth.Protocol.OAuth.onFrontendCallbackInit
        , placeholder = \_ -> ()
        }


getUserInfo :
    OAuth.AuthenticationSuccess
    -> Task Auth.Common.Error UserInfo
getUserInfo authenticationSuccess =
    let
        extract : String -> Json.Decoder a -> Dict String Json.Value -> Result String a
        extract k d v =
            Dict.get k v
                |> Maybe.map
                    (\v_ ->
                        Json.decodeValue d v_
                            |> Result.mapError Json.errorToString
                    )
                |> Maybe.withDefault (Err <| "Key " ++ k ++ " not found")

        extractOptional : a -> String -> Json.Decoder a -> Dict String Json.Value -> Result String a
        extractOptional default k d v =
            Dict.get k v
                |> Maybe.map
                    (\v_ ->
                        Json.decodeValue d v_
                            |> Result.mapError Json.errorToString
                    )
                |> Maybe.withDefault (Ok <| default)

        tokenR =
            case authenticationSuccess.idJwt of
                Nothing ->
                    Err "Identity JWT missing in authentication response. Please report this issue."

                Just idJwt ->
                    case JWT.fromString idJwt of
                        Ok (JWS t) ->
                            Ok t

                        Err err ->
                            Err <| jwtErrorToString err

        stuff =
            tokenR
                |> Result.andThen
                    (\token ->
                        let
                            meta =
                                token.claims.metadata

                            -- `email` requires the optional claim to be enabled in the Entra app manifest
                            -- (Token configuration → Add optional claim → ID → email).
                            -- `preferred_username` is always present and is the user's UPN, typically their email.
                            maybeEmail =
                                extractOptional Nothing "email" (Json.string |> Json.nullable) meta
                                    |> Result.withDefault Nothing

                            maybeName =
                                extractOptional Nothing "name" (Json.string |> Json.nullable) meta
                                    |> Result.withDefault Nothing
                                    |> Maybe.andThen nothingIfEmpty
                        in
                        extract "preferred_username" Json.string meta
                            |> Result.map
                                (\preferredUsername ->
                                    { email = maybeEmail |> Maybe.withDefault preferredUsername
                                    , name = maybeName
                                    , username = Just preferredUsername
                                    }
                                )
                    )
    in
    Task.mapError (Auth.Common.ErrAuthString << HttpHelpers.httpErrorToString) <|
        case stuff of
            Ok userInfo ->
                Task.succeed userInfo

            Err err ->
                Task.fail (Http.BadBody err)


jwtErrorToString : DecodeError -> String
jwtErrorToString err =
    case err of
        TokenTypeUnknown ->
            "Unsupported auth token type."

        JWSError decodeError ->
            case decodeError of
                JWS.Base64DecodeError ->
                    "Base64DecodeError"

                JWS.MalformedSignature ->
                    "MalformedSignature"

                JWS.InvalidHeader jsonError ->
                    "InvalidHeader: " ++ Json.errorToString jsonError

                JWS.InvalidClaims jsonError ->
                    "InvalidClaims: " ++ Json.errorToString jsonError
