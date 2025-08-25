.PHONY: all clean install docker_server_build docker_push serve auth_example transcripts fixtures transcripts reset_fixtures

SHARE_PROJECT_ROOT := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
export SHARE_PROJECT_ROOT
UNAME := $(shell uname)
STACK_FLAGS := "--fast"
dist_dir := $(shell stack path | awk '/^dist-dir/{print $$2}')
exe_name := share-api
exe := $(dist_dir)/build/$(exe_name)/$(exe_name)
target_dir := docker/tmp
installed_share := $(target_dir)/$(exe_name)
unison := $(shell command -v unison)
docker_registry=324181518966.dkr.ecr.us-west-2.amazonaws.com
share_commit := $(shell git diff-index --quiet HEAD -- && git rev-parse --short=10 HEAD || echo 'wip')

OPEN_BROWSER ?= "true"

ifeq ($(UNAME),Linux)
  OPEN := xdg-open
endif

ifeq ($(UNAME),Darwin)
  OPEN := open
endif


$(target_dir):
	mkdir $@

$(exe): $(shell find . unison -type f -name '*.hs') package.yaml stack.yaml
	@echo $(exe)
	@echo $@
	stack build $(STACK_FLAGS) share-api:share-api

$(installed_share): $(exe) $(target_dir)
	cp $(exe) $(installed_share)

auth_example:
	stack build --fast test-auth-app

# Build Share and run it alongside its dependencies via docker-compose
serve: $(installed_share)
	@docker compose -f docker/docker-compose.dev.yml down || true
	@trap 'docker compose -f docker/docker-compose.dev.yml down' EXIT INT TERM
	@echo "Booting up docker dependencies..."
	docker compose -f docker/docker-compose.dev.yml up --remove-orphans --detach
	@echo "Booting up docker dependencies...";
	@while  ! (  pg_isready --host localhost -U postgres -p 5432 >/dev/null 2>&1 && redis-cli -p 6379 ping  >/dev/null 2>&1 && VAULT_ADDR=http://localhost:8200 vault status >/dev/null 2>&1 )  do \
	  sleep 1; \
	done;
	@echo "Starting up Share at http://localhost:5424";
	@if [ ${OPEN_BROWSER} = "true" ] ; then \
	  (sleep 1 && $(OPEN) "http://localhost:5424/local/user/test/login" || true) & \
	fi;
	@(. ./local.env && $(exe) 2>&1)

# Loads the local testing share with a bunch of realistic code.
reset_fixtures:
	@docker compose -f docker/docker-compose.dev.yml down || true
	@trap 'docker compose -f docker/docker-compose.dev.yml down' EXIT INT TERM
	# Remove the existing postgres volume to reset the database
	@echo "Removing any existing postgres volume"
	docker volume rm docker_postgresVolume || true
	@echo "Booting up docker dependencies..."
	docker compose -f docker/docker-compose.dev.yml -f docker/docker-compose.fixtures.yml up --remove-orphans --detach
	@echo "Initializing fixture data";
	@while  ! (  pg_isready --host localhost -U postgres -p 5432 >/dev/null 2>&1 && redis-cli -p 6379 ping  >/dev/null 2>&1 && VAULT_ADDR=http://localhost:8200 vault status >/dev/null 2>&1 )  do \
	  sleep 1; \
	done;
	@echo "Booting up share";
	@( . ./local.env \
		$(exe) 2>&1 & \
		SERVER_PID=$$!; \
	trap "kill $$SERVER_PID 2>/dev/null || true" EXIT INT TERM; \
	echo "Loading fixtures"; \
	./transcripts/fixtures/run.zsh; \
	kill $$SERVER_PID 2>/dev/null || true; \
	) 
	@echo "Done!";

transcripts:
	@echo "Taking down any existing docker dependencies"
	@docker compose -f docker/docker-compose.dev.yml down || true
	@trap 'docker compose -f docker/docker-compose.dev.yml down' EXIT INT TERM
	# Remove the existing postgres volume to reset the database
	@echo "Removing any existing postgres volume"
	docker volume rm docker_postgresVolume || true
	@echo "Booting up docker dependencies..."
	docker compose -f docker/docker-compose.dev.yml -f docker/docker-compose.fixtures.yml up --remove-orphans --detach
	@while  ! (  pg_isready --host localhost -U postgres -p 5432 >/dev/null 2>&1 && redis-cli -p 6379 ping  >/dev/null 2>&1 && VAULT_ADDR=http://localhost:8200 vault status >/dev/null 2>&1 )  do \
	  sleep 1; \
	done;
	./transcripts/configure_transcript_database.zsh
	@echo "Booting up share";
	( . ./local.env ; \
		$(exe) & \
		SERVER_PID=$$!; \
		trap "kill $$SERVER_PID 2>/dev/null || true" EXIT INT TERM; \
		echo "Running transcripts"; \
		./transcripts/run-transcripts.zsh ; \
		echo "Done transcripts"; \
		kill $$SERVER_PID 2>/dev/null || true; \
	) 
	@echo "Done!";
