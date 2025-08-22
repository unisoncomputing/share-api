.PHONY: all clean install docker_server_build docker_push serve auth_example transcripts fixtures transcripts

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
	@trap 'docker compose -f docker/docker-compose.yml down' EXIT INT TERM
	@echo "Booting up docker dependencies..."
	docker compose -f docker/docker-compose.yml up --remove-orphans --detach postgres redis vault http-echo otel-collector jaeger
	@echo "Booting up docker dependencies...";
	@while  ! (  pg_isready --host localhost -U postgres -p 5432 >/dev/null 2>&1 && redis-cli -p 6379 ping  >/dev/null 2>&1 && VAULT_ADDR=http://localhost:8200 vault status >/dev/null 2>&1 )  do \
	  sleep 1; \
	done;
	@echo "Starting up Share at http://localhost:5424";
	@if [ ${OPEN_BROWSER} = "true" ] ; then \
	  (sleep 1 && $(OPEN) "http://localhost:5424/local/user/test/login" || true) & \
	fi;
	@(. ./local.env && $(exe) 2>&1)

fixtures:
	echo "Resetting local database to fixture data"
	PGPASSWORD="sekrit" psql -U postgres -p 5432 -h localhost -f "transcripts/sql/clean.sql"
	PGPASSWORD="sekrit" psql -U postgres -p 5432 -h localhost -f "transcripts/sql/inserts.sql"

transcripts:
	./transcripts/run-transcripts.zsh
