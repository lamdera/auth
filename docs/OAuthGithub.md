
## Setting up OAuth with Github

### 1. Create a production Github OAuth app

- For a personal account app: https://github.com/settings/developers
  (Profile icon > Settings > Developer settings > Oauth apps)
- On an org account app: `https://github.com/organizations/ORGNAME/settings/applications`
  (Org > Settings > Developer settings > OAuth Apps)

Create a new OAuth app and set the callback URL according to your `appname`:

```
https://appname.lamdera.app/login/OAuthGithub/callback
```

Save the `clientId` and `clientSecret` values.

### 2. Create a development Github OAuth app

Because Github only allows a single callback URL per app, you'll need a second app for development.

Set the callback URL to:
```
http://localhost:8000/login/OAuthGithub/callback
```

Save the `clientId` and `clientSecret` values.


### 3. Add secrets config to your app code

Here's a helpful `Config.elm` setup that switches credentials between production and development:


```elm
module Config exposing (..)

import Env


githubOAuthClientId =
    case Env.mode of
        Env.Production ->
            Env.githubOAuthClientId

        _ ->
            "your dev app clientId"


githubOAuthClientSecret =
    case Env.mode of
        Env.Production ->
            Env.githubOAuthClientSecret

        _ ->
            "your dev app clientSecret"
```

Your `Env.elm` can hold the empty placeholders for production:

```elm
module Env exposing (..)

-- The Env.elm file is for per-environment configuration.
-- See https://dashboard.lamdera.app/docs/environment for more info.


githubOAuthClientId =
    ""


githubOAuthClientSecret =
    ""
```

This setup assumes you're happy to commit your dev OAuth secret to code. If not, set it up the same as prod, but beware OAuth with empty values currently fails during OAuth handshake so the error might be a bit confusing later for someone inexperienced with the setup.


### 4. Add production secrets to your app

On the Lamdera Dashboard go to your `App > Config` page to set the production values for `githubOAuthClientId` and `githubOAuthClientSecret` secrets. Make sure to keep them as "backend only" (default), i.e. the green lock icon.


### 5. Add OAuthGithub method to your methods config

Now you can add the `OAuthGithub` method to your `lamdera/auth` configuration:

```elm
config : Auth.Common.Config FrontendMsg ToBackend BackendMsg ToFrontend FrontendModel BackendModel
config =
    { ...
    , methods =
        [ Auth.Method.OAuthGithub.configuration
            Config.githubOAuthClientId
            Config.githubOAuthClientSecret
        ]
    , ...
    }
```

### 6. Initiate Github OAuth when desired

If you've already implemented `lamdera/auth`, then you can now add a `Msg` variant to kick off a Github auth:

```elm
GithubSigninRequested ->
    Auth.Flow.signInRequested "OAuthGithub" model Nothing
        |> Tuple.mapSecond (AuthToBackend >> Lamdera.sendToBackend)
```
