
.PHONY: all install db

all: install db

install: node_modules

node_modules: package.json
	@npm install

.env: test.env
	@cp $^ "$@"

db: .env
	npm run db:migrate
