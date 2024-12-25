module Auth.Method.EmailMagicLink exposing (..)

import Auth.Common exposing (..)
import Effect.Browser.Navigation
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Task
import Effect.Time
import SeqDict exposing (SeqDict)
import Url exposing (Protocol(..), Url)
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query as Query


configuration :
    { initiateSignin :
        SessionId
        -> ClientId
        -> { backendModel | pendingAuths : SeqDict SessionId PendingAuth }
        -> { username : Maybe String }
        -> Effect.Time.Posix
        -> ( { backendModel | pendingAuths : SeqDict SessionId PendingAuth }, Command BackendOnly toMsg backendMsg )
    , onAuthCallbackReceived :
        SessionId
        -> ClientId
        -> Url
        -> AuthCode
        -> State
        -> Effect.Time.Posix
        -> (BackendMsg -> backendMsg)
        -> { backendModel | pendingAuths : SeqDict SessionId PendingAuth }
        -> ( { backendModel | pendingAuths : SeqDict SessionId PendingAuth }, Command BackendOnly toMsg backendMsg )
    }
    ->
        Method
            frontendMsg
            backendMsg
            { frontendModel | authFlow : Flow, authRedirectBaseUrl : Url }
            { backendModel | pendingAuths : SeqDict SessionId PendingAuth }
            BackendOnly
            toMsg
configuration { initiateSignin, onAuthCallbackReceived } =
    ProtocolEmailMagicLink
        { id = "EmailMagicLink"
        , initiateSignin = initiateSignin
        , onFrontendCallbackInit = onFrontendCallbackInit
        , onAuthCallbackReceived = onAuthCallbackReceived
        , placeholder = \_ _ _ _ -> ()
        }


onFrontendCallbackInit :
    { frontendModel | authFlow : Auth.Common.Flow }
    -> MethodId
    -> Url
    -> Effect.Browser.Navigation.Key
    -> (ToBackend -> Command restriction toMsg frontendMsg)
    -> ( { frontendModel | authFlow : Auth.Common.Flow }, Command restriction toMsg frontendMsg )
onFrontendCallbackInit frontendModel methodId origin _ toBackend =
    case origin |> Url.Parser.parse (callbackUrl methodId <?> queryParams) of
        Just ( Just token, Just email ) ->
            ( { frontendModel | authFlow = Auth.Common.Pending }
            , toBackend <| Auth.Common.AuthCallbackReceived methodId origin token email
            )

        _ ->
            ( { frontendModel | authFlow = Errored <| ErrAuthString "Missing token and/or email parameters. Please try again." }
            , Command.none
            )


trigger msg =
    Effect.Time.now |> Effect.Task.perform (always msg)


callbackUrl methodId =
    Url.Parser.s "login" </> Url.Parser.s methodId </> Url.Parser.s "callback"


queryParams =
    -- @TODO why doesn't query params parsing work by itself?
    Query.map2 Tuple.pair (Query.string "token") (Query.string "email")
