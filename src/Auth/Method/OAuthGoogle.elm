module Auth.Method.OAuthGoogle exposing (..)

import Auth.Common exposing (..)
import Auth.HttpHelpers as HttpHelpers
import Auth.Protocol.OAuth
import Effect.Command exposing (BackendOnly)
import Effect.Http
import Effect.Task
import JWT exposing (..)
import JWT.JWS as JWS
import Json.Decode as Json
import OAuth
import OAuth.AuthorizationCode as OAuth
import SeqDict as Dict exposing (SeqDict)
import Url exposing (Protocol(..), Url)


configuration :
    String
    -> String
    ->
        Method
            frontendMsg
            backendMsg
            { frontendModel | authFlow : Flow, authRedirectBaseUrl : Url }
            backendModel
            BackendOnly
            toMsg
configuration clientId clientSecret =
    ProtocolOAuth
        { id = "OAuthGoogle"
        , authorizationEndpoint = { defaultHttpsUrl | host = "accounts.google.com", path = "/o/oauth2/v2/auth" }
        , tokenEndpoint = { defaultHttpsUrl | host = "oauth2.googleapis.com", path = "/token" }
        , logoutEndpoint = Home { returnPath = "/logout/OAuthGoogle/callback" }
        , allowLoginQueryParameters = False
        , clientId = clientId
        , clientSecret = clientSecret
        , scope = [ "openid email profile" ]
        , getUserInfo = getUserInfo
        , onFrontendCallbackInit = Auth.Protocol.OAuth.onFrontendCallbackInit
        , placeholder = \_ -> ()

        -- , onAuthCallbackReceived = Debug.todo "onAuthCallbackReceived"
        }


getUserInfo :
    OAuth.AuthenticationSuccess
    -> Effect.Task.Task restriction Auth.Common.Error UserInfo
getUserInfo authenticationSuccess =
    let
        extract : String -> Json.Decoder a -> SeqDict String Json.Value -> Result String a
        extract k d v =
            Dict.get k v
                |> Maybe.map
                    (\v_ ->
                        Json.decodeValue d v_
                            |> Result.mapError Json.errorToString
                    )
                |> Maybe.withDefault (Err <| "Key " ++ k ++ " not found")

        extractOptional : a -> String -> Json.Decoder a -> SeqDict String Json.Value -> Result String a
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
                        in
                        Result.map4
                            (\email email_verified given_name family_name ->
                                { email = email
                                , email_verified = email_verified
                                , given_name = given_name
                                , family_name = family_name
                                }
                            )
                            (extract "email" Json.string meta)
                            (extract "email_verified" Json.bool meta)
                            (extract "given_name" Json.string meta)
                            (extractOptional Nothing "family_name" (Json.string |> Json.nullable) meta)
                    )
    in
    Effect.Task.mapError (Auth.Common.ErrAuthString << HttpHelpers.httpErrorToString) <|
        case stuff of
            Ok result ->
                Effect.Task.succeed
                    { email = result.email
                    , name =
                        [ result.given_name, Maybe.withDefault "" result.family_name ]
                            |> String.join " "
                            |> nothingIfEmpty
                    , username = Nothing
                    }

            Err err ->
                Effect.Task.fail (Effect.Http.BadBody err)


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
