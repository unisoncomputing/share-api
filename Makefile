.PHONY: all clean install docker_server_build docker_push serve auth_example transcripts fixtures

UNAME := $(shell uname)
STACK_FLAGS := "--fast"
dist_dir := $(shell stack path | awk '/^dist-dir/{print $$2}')
exe_name := enlil-exe
inst_exe_name := enlil
exe := $(dist_dir)/build/$(exe_name)/$(exe_name)
target_dir := docker/tmp
installed_enlil := $(target_dir)/$(inst_exe_name)
unison := $(shell command -v unison)
docker_registry=324181518966.dkr.ecr.us-west-2.amazonaws.com
enlil_commit := $(shell git diff-index --quiet HEAD -- && git rev-parse --short=10 HEAD || echo 'wip')

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
	stack build $(STACK_FLAGS) enlil:enlil-exe

$(installed_enlil): $(exe) $(target_dir)
	cp $(exe) $(installed_enlil)

auth_example:
	stack build --fast test-auth-app

docker_server_build: $(installed_enlil)
	docker build $(docker_platform_flag) -f docker/Dockerfile --build-arg ENLIL_COMMIT=$(enlil_commit) -t share docker

docker_server_release: $(installed_enlil)
	docker build $(docker_platform_flag) -f docker/Dockerfile -t $(docker_registry)/share:$(DRONE_BUILD_NUMBER) docker

docker_push: $(docker_server_release)
	docker push $(docker_registry)/share:$(DRONE_BUILD_NUMBER)

docker_staging_release: $(installed_enlil)
	docker build $(docker_platform_flag) -f docker/Dockerfile -t $(docker_registry)/share-staging:$(DRONE_BUILD_NUMBER) docker

docker_staging_push: $(docker_server_release)
	docker push $(docker_registry)/share-staging:$(DRONE_BUILD_NUMBER)

serve: $(installed_enlil)
	trap 'docker-compose -f docker/docker-compose.yml down' EXIT INT TERM
	docker-compose -f docker/docker-compose.yml up postgres redis &
	while  ! (  pg_isready --host localhost -U postgres -p 5432 && redis-cli -p 6379 ping)  do \
		echo "Waiting for postgres and redis..."; \
	  sleep 1; \
	done;
	echo "Running Share at http://localhost:5424"

	if [ ${OPEN_BROWSER} = "true" ] ; then \
	  (sleep 1 && $(OPEN) "http://localhost:5424/local/user/test/login" || true) & \
	fi
	(. ./local.env && $(exe) 2>&1)

fixtures:
	echo "Resetting local database to fixture data"
	PGPASSWORD="sekrit" psql -U postgres -p 5432 -h localhost -f "transcripts/sql/clean.sql"
	PGPASSWORD="sekrit" psql -U postgres -p 5432 -h localhost -f "transcripts/sql/inserts.sql"
