.PHONY: all clean install docker_server_build docker_push serve auth_example transcripts fixtures transcripts serve_transcripts reset_fixtures

SHARE_PROJECT_ROOT := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
export SHARE_PROJECT_ROOT
UNAME := $(shell uname)
STACK_FLAGS := "--fast"
exe_name := share-api
exe := $(shell stack exec -- which $(exe_name))
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

$(exe): $(shell find . unison -type f -name '*.hs') $(shell find . unison -type f -name '*.yaml')
	@echo $(exe)
	@echo $@
	stack build $(STACK_FLAGS)

$(installed_share): $(exe) $(target_dir)
	cp $(exe) $(installed_share)

auth_example:
	stack build --fast test-auth-app

# Build Share and run it alongside its dependencies via docker-compose
serve: $(installed_share)
	@docker compose -f docker/docker-compose.base.yml down || true
	@trap 'docker compose -f docker/docker-compose.base.yml down' EXIT INT TERM
	@echo "Booting up docker dependencies..."
	docker compose -f docker/docker-compose.base.yml -f docker/docker-compose.fixtures.yml up  --remove-orphans --detach
	@echo "Booting up docker dependencies...";
	@while  ! (  pg_isready --host localhost -U postgres -p 5432 >/dev/null 2>&1 && redis-cli -p 6379 ping  >/dev/null 2>&1 && VAULT_ADDR=http://localhost:8200 vault status >/dev/null 2>&1 )  do \
	  sleep 1; \
	done;
	@echo "Starting up Share at http://localhost:5424";
	@if curl -f -s http://localhost:1234 >/dev/null 2>&1 ; then \
	  (sleep 1 && $(OPEN) "http://localhost:5424/local/user/test/login" && $(OPEN) "http://localhost:1234" || true) & \
	fi;
	@(. ./local.env && $(exe) 2>&1)

# Loads the local testing share with a bunch of realistic code.
reset_fixtures: $(installed_share)
	# Prompt for confirmation
	@echo "This will delete all data in your local share database. Are you sure? (y/N) "
	@read -r confirmation && [ "$$confirmation" = "y" ] || [ "$$confirmation" = "Y" ]
	@docker compose -f docker/docker-compose.base.yml down || true
	@trap 'docker compose -f docker/docker-compose.base.yml down' EXIT INT TERM
	# Remove the existing postgres volume to reset the database
	@echo "Removing any existing postgres volume"
	docker volume rm docker_postgresVolume || true
	@echo "Booting up docker dependencies..."
	docker compose -f docker/docker-compose.base.yml -f docker/docker-compose.fixtures.yml up --remove-orphans --detach
	@echo "Initializing fixture data";
	@while  ! (  pg_isready --host localhost -U postgres -p 5432 >/dev/null 2>&1 && redis-cli -p 6379 ping  >/dev/null 2>&1 && VAULT_ADDR=http://localhost:8200 vault status >/dev/null 2>&1 )  do \
	  sleep 1; \
	done;
	@echo "Booting up share";
	@( . ./local.env ; \
		$(exe) & \
		SERVER_PID=$$!; \
	trap "kill $$SERVER_PID 2>/dev/null || true" EXIT INT TERM; \
	echo "Loading fixtures"; \
	./transcripts/fixtures/run.zsh; \
	kill $$SERVER_PID 2>/dev/null || true; \
	) 
	@echo "Done!";


serve_transcripts: $(installed_share)
	@echo "Taking down any existing docker dependencies"
	@docker compose -f docker/docker-compose.base.yml down || true
	@trap 'docker compose -f docker/docker-compose.base.yml down' EXIT INT TERM
	@echo "Booting up transcript docker dependencies..."
	docker compose -f docker/docker-compose.base.yml up --remove-orphans

transcripts: $(installed_share)
	@echo "Taking down any existing docker dependencies"
	@docker compose -f docker/docker-compose.base.yml down || true
	@trap 'docker compose -f docker/docker-compose.base.yml down' EXIT INT TERM
	@echo "Booting up transcript docker dependencies..."
	docker compose -f docker/docker-compose.base.yml up --remove-orphans --detach
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
		./transcripts/run-transcripts.zsh $(pattern); \
		kill $$SERVER_PID 2>/dev/null || true; \
	) 
	@echo "Transcripts complete!";

task-runner: $(installed_share)
	@echo "Taking down any existing docker dependencies"
	@docker compose -f docker/docker-compose.base.yml down || true
	@trap 'docker compose -f docker/docker-compose.base.yml down' EXIT INT TERM
	@echo "Booting up task docker dependencies..."
	docker compose -f docker/docker-compose.base.yml up --remove-orphans --detach
	@while  ! (  pg_isready --host localhost -U postgres -p 5432 >/dev/null 2>&1 && redis-cli -p 6379 ping  >/dev/null 2>&1 && VAULT_ADDR=http://localhost:8200 vault status >/dev/null 2>&1 )  do \
	  sleep 1; \
	done;
	./transcripts/configure_transcript_database.zsh
	@echo "Booting up share";
	( . ./local.env ; \
		$(exe) & \
		SERVER_PID=$$!; \
		trap "kill $$SERVER_PID 2>/dev/null || true" EXIT INT TERM; \
		echo "Running task"; \
		stack exec share-task-runner; \
		kill $$SERVER_PID 2>/dev/null || true; \
	) 
	@echo "Task complete!";
