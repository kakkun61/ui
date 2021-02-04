PWSH = pwsh

.PHONY: build
build: build-deps
	cabal v2-build hui

.PHONY: build-deps
build-deps:
	cabal v2-build --only-dependencies hui

.PHONY: build-build
build-build:
	cabal v2-build hui-build

.PHONY: build-example
build-example:
	cabal v2-build example:flib:HuiLib

.PHONY: deploy-example
deploy-example: build-example build-build
	cabal exec hui -- example ../.NET/Example/HuiLib.dll

.PHONY: test
test: spec-example

.PHONY: spec-example
spec-example:
	cabal v2-test example:test:spec

.PHONY: repl
repl:
	cabal v2-repl

.PHONY: format
format:
	$(PWSH) -Command "& { Get-ChildItem -Filter '*.hs' -Recurse hui\src, hui-build\app, hui-build\src, example\src, example\test | ForEach-Object { stylish-haskell -i $$_.FullName } }"
	stylish-haskell -i example/Setup.hs

.PHONY: lint
lint:
	hlint src app

.PHONY: doc
doc:
	cabal v2-haddock all

.PHONY: clean
clean:
	cabal v2-clean
	- $(PWSH) -Command "& { Remove-Item -Recurse -Force example\gen }"

.PHONY: targets
targets:
	$(PWSH) -Command "& { Get-Content ./Makefile | Where-Object { $$_ -like '.PHONY*' } | ForEach-Object { $$_.Substring(8) } }"
