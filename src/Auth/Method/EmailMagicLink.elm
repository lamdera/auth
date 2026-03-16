module Auth.Method.EmailMagicLink exposing (..)

import Auth.Common exposing (..)
import Effect.Command as Command exposing (BackendOnly, Command)
import Effect.Lamdera exposing (ClientId, SessionId)
import Effect.Task as Task exposing (Task)
import Effect.Time as Time
import Url exposing (Protocol(..), Url)
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query as Query


configuration :
    { initiateSignin :
        SessionId
        -> ClientId
        -> backendModel
        -> { username : Maybe String }
        -> Time.Posix
        -> ( backendModel, Command BackendOnly toFrontend backendMsg )
    , onAuthCallbackReceived :
        SessionId
        -> ClientId
        -> Url
        -> AuthCode
        -> State
        -> Time.Posix
        -> (BackendMsg -> backendMsg)
        -> backendModel
        -> ( backendModel, Command BackendOnly toFrontend backendMsg )
    }
    ->
        Method
            frontendMsg
            toBackend
            backendMsg
            toFrontend
            { frontendModel | authFlow : Flow, authRedirectBaseUrl : Url }
            backendModel
configuration { initiateSignin, onAuthCallbackReceived } =
    ProtocolEmailMagicLink
        { id = "EmailMagicLink"
        , initiateSignin = initiateSignin
        , onFrontendCallbackInit = onFrontendCallbackInit
        , onAuthCallbackReceived = onAuthCallbackReceived
        , placeholder = \frontendMsg backendMsg frontendModel backendModel -> ()
        }


onFrontendCallbackInit frontendModel methodId origin key toBackend =
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
    Time.now |> Task.perform (always msg)


callbackUrl methodId =
    Url.Parser.s "login" </> Url.Parser.s methodId </> Url.Parser.s "callback"


queryParams =
    -- @TODO why doesn't query params parsing work by itself?
    Query.map2 Tuple.pair (Query.string "token") (Query.string "email")
