.PHONY: test lint format format-check clean

all: test lint format-check

node_modules: package.json
	npm install

test: node_modules
	npm test

lint: node_modules
	npm run lint

format: node_modules
	npm run format

format-check: node_modules
	npm run format:check

clean:
	rm -rf node_modules dist package-lock.json
