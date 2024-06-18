.PHONY: test lint clean

all: test lint

node_modules: package.json
	npm install

test: node_modules
	npm test

lint: node_modules
	npm run lint

clean:
	rm -rf node_modules dist package-lock.json
