
## Setting up OAuth with Google

### 1. Create production Google OAuth credentials

- Go to https://console.cloud.google.com/apis/credentials > Create credentials > OAuth Client ID
    - You might be asked to go on a quick detour to "Configure your consent screen" if you haven't already
- Application type: Web application
- For "Authorized Javascript origins" set
  ```
  https://appname.lamdera.app
  ```
- For "Authorized redirect URI" set
  ```
  https://appname.lamdera.app/login/OAuthGoogle/callback
  ```

Note the `clientId` and `clientSecret` values.

Note: if you prefer, you can add `http://localhost:8000` and `http://localhost:8000/login/OAuthGoogle/callback` respectively to these settings instead of creating separate development credentials, but there's a higher risk you'll accidentally commit your prod keys into git that way!

### 2. Create development Google OAuth credentials

If you didn't combine your dev config into the production credentials, then follow the same steps to setup a second OAuth client ID for dev:


- For "Authorized Javascript origins" set
  ```
  http://localhost:8000
  ```
- For "Authorized redirect URI" set
  ```
  http://localhost:8000/login/OAuthGoogle/callback
  ```

Note the `clientId` and `clientSecret` values.


### 3. Add secrets config to your app code

Here's a helpful `Config.elm` setup that switches credentials between production and development:


```elm
module Config exposing (..)

import Env


googleOAuthClientId =
    case Env.mode of
        Env.Production ->
            Env.googleOAuthClientId

        _ ->
            "your dev app clientId"


googleOAuthClientSecret =
    case Env.mode of
        Env.Production ->
            Env.googleOAuthClientSecret

        _ ->
            "your dev app clientSecret"
```

Your `Env.elm` can hold the empty placeholders for production:

```elm
module Env exposing (..)

-- The Env.elm file is for per-environment configuration.
-- See https://dashboard.lamdera.app/docs/environment for more info.


googleOAuthClientId =
    ""


googleOAuthClientSecret =
    ""
```

This setup assumes you're happy to commit your dev OAuth secret to code. If not, set it up the same as prod, but beware OAuth with empty values currently fails during OAuth handshake so the error might be a bit confusing later for someone inexperienced with the setup.


### 4. Add production secrets to your app

On the Lamdera Dashboard go to your `App > Config` page to set the production values for `googleOAuthClientId` and `googleOAuthClientSecret` secrets. Make sure to keep them as "backend only" (default), i.e. the green lock icon.


### 5. Add OAuthGoogle method to your methods config

Now you can add the `OAuthGoogle` method to your `lamdera/auth` configuration:

```elm
config : Auth.Common.Config FrontendMsg ToBackend BackendMsg ToFrontend FrontendModel BackendModel
config =
    { ...
    , methods =
        [ Auth.Method.OAuthGoogle.configuration
            Config.googleOAuthClientId
            Config.googleOAuthClientSecret
        ]
    , ...
    }
```

### 6. Initiate Google OAuth when desired

If you've already implemented `lamdera/auth`, then you can now add a `Msg` variant to kick off a Google auth:

```elm
GoogleSigninRequested ->
    Auth.Flow.signInRequested "OAuthGoogle" model Nothing
        |> Tuple.mapSecond (AuthToBackend >> Lamdera.sendToBackend)
```
