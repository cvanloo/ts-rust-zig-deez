help:
	@cat Makefile | grep -E "^\w+$:"

ready: fmt lint test

docker-build:
	docker build --no-cache . -t deez_$(notdir $(shell pwd))

docker-ready: docker-build
	docker run -v $(shell pwd):/deez -t deez_$(notdir $(shell pwd))

fmt:
	clj -M:cljfmt fix

lint:
	clj -M:lint --lint .

.PHONY: test

test:
	clj -M:test
