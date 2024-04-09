# Lamdera Auth

This library is a work in progress! We hope to clean up the API some more before a formal release.

It is however entirely functional, and being used in a number of production Lamdera apps, including the entire login mechanism for [https://dashboard.lamdera.app/](https://dashboard.lamdera.app/docs/wire).

### Example implementations

- You can see a draft implementation of [lamdera/auth replacing a fake user/pass auth with a real Github Auth on the Lamdera Realword repo](https://github.com/supermario/lamdera-realworld/compare/lamdera-explore-auth-draft).
- There's also a vanilla Lamdera app example thanks to @vpjapp: [https://github.com/vpjapp/lamdera-auth-example](https://github.com/vpjapp/lamdera-auth-example)

### Progress

Progress:

- [x] OAuth flow
- [x] Email magic link flow
- [x] URL route handling
- [x] Logout handling
- [ ] Improve the API surface area
- [ ] Actually make this an installable package with nice docs
- [ ] Stretch goal: elm-review rule to set everything up!


### Vendored packages

This package vendors two other Elm packages in order to make modifications:

- [`truqu/elm-oauth2`](https://github.com/truqu/elm-oauth2): [license (MIT)](src/JWT/LICENSE), [modification notes](src/JWT/readme.md)
- [`leojpod/elm-jwt`](https://github.com/leojpod/elm-jwt): [license (GPLv3)](src/OAuth/LICENSE), [modification notes](src/OAuth/readme.md)

Ideally these will be de-vendored into a regular Elm dependencies in future.



### Install

Until this is available as a package:

- Clone this repo into your project as a git submodule (or vendor it manually by copy pasting src)
- Reference `src` in your project's `elm.json:source-directories`
- Install the relevant deps:
```
yes | lamdera install elm/browser
yes | lamdera install elm/bytes
yes | lamdera install elm/http
yes | lamdera install elm/json
yes | lamdera install elm/regex
yes | lamdera install elm/time
yes | lamdera install elm/url
yes | lamdera install elm-community/dict-extra
yes | lamdera install elm-community/list-extra
yes | lamdera install chelovek0v/bbase64
yes | lamdera install ktonon/elm-crypto
yes | lamdera install ktonon/elm-word
yes | lamdera install NoRedInk/elm-json-decode-pipeline
yes | lamdera install TSFoster/elm-sha1
```

You might also have luck with [elm-git-install](https://github.com/robinheghan/elm-git-install), though its not been tried yet.


### Setup

:warning: This is the conceptual target API, not actual instructions for this code yet! (Instead, follow the types!)


1. Create `src/Auth.elm`:

```elm
module Auth exposing (..)

import Auth.Common
import Auth.Method.EmailMagicLink
import Auth.Method.OAuthGithub
import Auth.Method.OAuthGoogle
import Lamdera
import Types exposing (..)


config : Auth.Common.Config FrontendMsg ToBackend BackendMsg ToFrontend FrontendModel BackendModel
config =
    { toBackend = AuthToBackend
    , toFrontend = AuthToFrontend
    , backendMsg = AuthBackendMsg
    , sendToFrontend = Lamdera.sendToFrontend
    , sendToBackend = Lamdera.sendToBackend
    , methods =
        [ Auth.Method.EmailMagicLink.configuration
        , Auth.Method.OAuthGithub.configuration Config.githubAppClientId Config.githubAppClientSecret
        , Auth.Method.OAuthGoogle.configuration Config.googleAppClientId Config.googleAppClientSecret
        ]
    }
```

2. Modify the 2 core Model types in `src/Types.elm`:


```elm
import Auth.Common
import Dict exposing (Dict)
import Lamdera
import Url exposing (Url)

type alias FrontendModel =
  { ...
  , authFlow : Auth.Common.Flow
  , authRedirectBaseUrl : Url
  }

type alias BackendModel =
  { ...
  , pendingAuths : Dict Lamdera.SessionId Auth.Common.PendingAuth
  }
```

3. Modify the 4 core Msg types in `src/Types.elm`:

```elm
import Auth.Common

type FrontendMsg
    ...
    | AuthSigninRequested { methodId : Auth.Common.MethodId, username : Maybe String }

type ToBackend
    ...
    | AuthToBackend Auth.Common.ToBackend

type BackendMsg
    ...
    | AuthBackendMsg Auth.Common.BackendMsg

type ToFrontend
    ...
    | AuthToFrontend Auth.Common.ToFrontend
```

4. Implement the 4 new Msg variants:

`Frontend.elm`:

```elm
update msg model =
  ...
    AuthSigninRequested { methodId, username } ->
      Auth.Flow.signInRequested methodId model username
          |> Tuple.mapSecond (AuthToBackend >> sendToBackend)

updateFromBackend msg model =
  ...
    AuthToFrontend authToFrontendMsg ->
      Auth.updateFromBackend authToFrontendMsg model
```

`Backend.elm`:

```elm
update msg model =
  ...
     AuthBackendMsg authMsg ->
         Auth.Flow.backendUpdate (Auth.backendConfig model) authMsg

updateFromFrontend sessionId clientId msg model =
  ...
    AuthToBackend authToBackend ->
      Auth.Flow.updateFromFrontend (Auth.backendConfig model) clientId sessionId authToBackend model

```

5. Adjust your routing handlers:

How you do page routing will vary on your app approach (i.e. manual, elm-land, etc), but here's an example route matcher using `elm/url:Url.Parser`:

```elm
map LoginCallback (s "login" </> string </> s "callback")
```

Add the additional Msg variant to your FrontendMsg:

```elm
type FrontendMsg =
  ...
  | LoginCallback String
```

And add the handler to your update function:

```elm
update msg model =
  ...
  LoginCallback methodId ->
    Auth.Flow.init model
      methodId
      url
      key
      (\msg -> Lamdera.sendToBackend (AuthToBackend msg))
```
