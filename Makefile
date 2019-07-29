.PHONY: help install run-docker-network

.DEFAULT_GOAL := help

export

help: ## Ask for help!
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

install: ## build library and install stack binaries
	stack install

run-docker-network: ## launch the docker netork containing trillian services
	docker-compose up -d
