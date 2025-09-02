# Unison Share

* [Setup](#setup)
    * [MacOS](#macos)
    * [Nix](#nix)
* [Running Locally](#running-locally)
* [Self Hosting](#self-hosting)
* [Updating unison dependencies](#updating-unison-dependencies)
* [Postgres](#postgres)
    * [Managing Postgres Migrations](#managing-postgres-migrations)
* [Redis](#redis)
    * [Clearing the redis cache](#clearing-the-redis-cache)
* [Testing](#testing)
    * [Running Transcripts](#running-transcripts)
    * [Updating Transcripts](#updating-transcripts)

This service provides the APIs which power the Unison Share web application,
including the APIs for syncing code with UCM.

Unison Share also acts as an authentication server for UCM and Unison Cloud. It implements [OAuth2](https://datatracker.ietf.org/doc/html/rfc6749) with 
the [PKCE extension](https://www.oauth.com/oauth2-servers/pkce/), and a subset of the [OpenID Connect Core](https://openid.net/specs/openid-connect-core-1_0.html) and [OpenID Discovery specifications](https://openid.net/specs/openid-connect-discovery-1_0.html).

## Contributing

Contributions are welcome, however we request that you join the [Unison Discord
server](https://unison-lang.org/discord) and discuss your ideas with the team before starting work on any features.

## Setup

Dependencies:

- docker
- docker-compose
- postgres
- redis-cli
- Haskell stack

This project depends on the `unison` repository as a submodule.
After cloning or pulling this repository you'll need to initialize the git submodules like this:

```sh
git submodule update --init --recursive
```

You can build the project for local development with `stack build --fast`.

### MacOS

Requires postgres and redis.

Note for M1 Arm architecture macs, if using an x86 version of `stack` you'll also need to link against
the x86 postgres lib. You can achieve this by following [these instructions](https://gist.github.com/progrium/b286cd8c82ce0825b2eb3b0b3a0720a0)
to install an x86 version of homebrew, then use that homebrew version to install postgres, then uninstall any postgres from your arm `brew`.

* `brew install postgresql`
* `brew install redis`

### Nix

A [flake.nix](flake.nix) file is provided in this repo. It currently doesn't use a "pure" nix build and could probably use some improvements. However, it generally seems to work as long as you provide the `--nix` and `--no-nix-pure` arguments to `stack`. For example `stack --nix --no-nix-pure test`.

## Running Locally

The first time you run locally, start with `make reset_fixtures`, then on subsequent runs just use `make serve`.
`make reset_fixtures` will set up some basic test data in your local database, and will copy all projects from `./transcripts/fixtures/projects.txt` and `./transcripts/fixtures/custom_projects.txt` from Share into your local database.

Data changes in Postgres using `make serve` are persistent locally.
You can reset the database to a known state with `make reset_fixtures`.

`make transcripts` will take down the database and use a temporary one for running the transcripts.

See the `Makefile` and `./docker/docker-compose.base.yml` to learn more.

### Debugging and observability

Open telemetry tracing:

When using docker compose locally, there's a Jaeger instance running at `http://localhost:16686` which you can use to view traces for your local requests.

Flags:

* Build with `--flag share-api:queryspans` to enable spans for every single database query. Useful for local debugging, but don't enable this in production as it's way too much data and processing.


## Self Hosting

The Docker Compose configuration located in `./docker/docker-compose.yml` is a great place to start for learning how Share's infrastructure is set up.
It details Share's required environment variables and shows how it expects to be connected to Postgres and Redis.

If you have any questions about self hosting, feel free to ask in our [Discord server](https://unison-lang.org/discord).

### Dependencies

Share currently requires a Postgres instance and access to a Redis server.

### Environment Variables

There are a number of environment variables Share requires.
See `local.env` for example values which are used for local development.

- `SHARE_API_ORIGIN`: The URL where the share server is accessible on the web.
- `SHARE_SERVER_PORT`: Which port the server should bind to on startup. This may differ from `SHARE_API_ORIGIN` if you're using a reverse proxy.
- `SHARE_REDIS`: The URL of the redis server.
- `SHARE_POSTGRES`: The URL of the postgres server.
- `SHARE_HMAC_KEY`: A secret key used for cryptographic signing of HashJWTs. This should be at least 32 characters long.
- `SHARE_EDDSA_KEY`: A secret key used for cryptographic signing of user sessions. This should be at least 32 characters long.
- `SHARE_DEPLOYMENT`: The deployment environment. One of: `local`, `staging`, `prod`.
- `SHARE_POSTGRES_CONN_TTL`: The maximum time a connection to the postgres server should be kept alive.
- `SHARE_POSTGRES_CONN_MAX`: The maximum number of connections to the postgres server.
- `SHARE_SHARE_UI_ORIGIN`: The URL where the Share UI is publicly accessible.
- `SHARE_CLOUD_UI_ORIGIN`: The URL where the Unison Cloud UI is publicly accessible.
- `SHARE_HOMEPAGE_ORIGIN`: The URL where the Unison homepage is publicly accessible. If you are self-hosting you can set this to any URL you like.
- `SHARE_CLOUD_HOMEPAGE_ORIGIN`: The URL where the Unison Cloud homepage is publicly accessible. If you are self-hosting you can set this to any URL you like.
- `SHARE_LOG_LEVEL`: Minimum level of logging to emit, One of: `DEBUG`, `INFO`, `ERROR`, `USERERROR`.
- `SHARE_COMMIT`: The git commit hash the share server was built from. This is passed along in error reports
- `SHARE_MAX_PARALLELISM_PER_DOWNLOAD_REQUEST`: How many concurrent workers may serve a single UCM pull. The recommended value for this will depend on the number of cores on your machine, but between 5-8 is a reasonable number.
- `SHARE_MAX_PARALLELISM_PER_UPLOAD_REQUEST`: How many concurrent workers may serve a single UCM push. The recommended value for this will depend on the number of cores on your machine, but between 5-8 is a reasonable number.
- `SHARE_ZENDESK_API_USER`: The username to use for the Zendesk API.
- `SHARE_ZENDESK_API_TOKEN`: The token to use for the Zendesk API.
- `SHARE_GITHUB_CLIENTID`: The client ID for the GitHub OAuth application which will be used to authenticate users with the Share server.
- `SHARE_GITHUB_CLIENT_SECRET`: The client secret for the GitHub OAuth application which will be used to authenticate users with the Share server.
- (optional) `SHARE_SENTRY_DSN`: The URL of the Sentry instance to send error reports to. You may leave this unset.

### Database initialization

Share doesn't currently use any tools for managing database migrations (sorry). To initialize your database, you must run all the **timestamped** migration files inside `./sql`.

We plan to transition to a more robust solution in the future.

## Updating unison dependencies

```sh
$ cd unison
$ git checkout trunk
$ git pull
$ cd ..
$ git add unison
$ git commit
```

## Postgres

### Managing Postgres Migrations

Currently the postgres schema is managed manually.
All the required schema changes exist in the `sql` directory, ordered by timestamp, but they are applied to the database by copy-pasting
them into a postgres terminal session.

It's recommended that you run migrations within a transaction so in the case something goes wrong, you can abort.

E.g.

```
> BEGIN;
> <run migrations>
> COMMIT;
```

## Redis

### Clearing the redis cache

We use redis to cache various api responses. If a change to logic requires clearing the cache, you can do so by
connecting to redis and executing the following command:

```
> FLUSHALL
```

It's fine to clear the cache, but of course don't do it too often if you can help it.
Clearing Redis may also interrupt users who are in the middle of the authentication flow,
but at worst they'd just need to log in again.

## Testing

Similar to UCM, we do most of the testing for Share with golden-file transcripts.
We set the database to an initial state, then interact with Share via UCM or curl commands and save the resulting JSON responses to files.
This allows us to easily see changes in JSON output as we make changes.

The transcript runner isn't perfectly deterministic yet, but is close enough to be useful.

### Running Transcripts

Transcript tests are a series of scripts which interact with Share via its APIs and ucm integrations to produce output files which
describe Share's behaviour. They're run on pull requests to ensure that any changes in behaviour are expected.

To run transcripts, first start the server with `make serve`, then in a separate terminal run `./transcripts/run-transcripts.zsh`.

Note: If you get local authentication errors you may need to: 

* Start the server
* Access `http://localhost:5424/local/user/transcripts/login`
* Run `UNISON_SHARE_HOST=http://localhost:5424 ucm`
* Run `> auth.login` from within UCM

Validate that the git diff is expected (or empty if that's what you expect) and then commit the changes.

### Updating Transcripts

I recommend checking existing transcripts to see how things are being done.
