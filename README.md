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

Start the server and its dependencies with `make serve`.
You may wish to run `make fixtures` to fill some in some data for local testing.

See `./docker/docker-compose.yml` to see how the postgres and redis services are configured.


## Self Hosting

The Docker Compose configuration located in `./docker/docker-compose.yml` is a great place to start for learning how Share's infrastructure is set up.
It details Share's required environment variables and shows how it expects to be connected to Postgres and Redis.

If you have any questions about self hosting, feel free to ask in our [Discord server](https://unison-lang.org/discord).

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
