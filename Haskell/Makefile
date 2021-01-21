PWSH = pwsh

# FIXME temporaly path
dist = dist-newstyle/build/x86_64-windows/ghc-8.10.3

dll = Hui

packages = hui-0.1.0.0

.PHONY: build
build: output/$(dll).dll output/$(dll).dll.a

output/$(dll).dll output/$(dll).dll.a&: build-c build-hs output
	ghc -shared -o output/$(dll).dll $(foreach package, $(packages), $(wildcard $(dist)/$(package)/build/*.o)) $(dll).def

output:
	$(PWSH) -Command "& { New-Item -ItemType Directory output }"

.PHONY: build-c
build-c: $(dist)/hui-0.1.0.0/build/rts.o

$(dist)/hui-0.1.0.0/build/rts.o:
	$(PWSH) -Command "& { Set-Location src; ghc -c rts.c -outputdir ../$(dist)/hui-0.1.0.0/build }"

.PHONY: build-hs
build-hs: build-hs-deps
	cabal v2-build $(CABAL_OPTIONS)

.PHONY: build-hs-deps
build-hs-deps:
	cabal v2-build --only-dependencies $(CABAL_OPTIONS)

.PHONY: test
test: doctest spec

.PHONY: doctest
doctest: build-hs-deps
	cabal v2-test doctest $(CABAL_OPTIONS)

.PHONY: spec
spec: build-hs-deps
	cabal v2-test spec $(CABAL_OPTIONS)

.PHONY: repl
repl:
	cabal v2-repl $(CABAL_OPTIONS)

.PHONY: format
format:
	$(PWSH) -Command "& { Get-ChildItem -Filter '*.hs' -Recurse src, app, test | ForEach-Object { stylish-haskell -i $$_.FullName } }"

.PHONY: lint
lint:
	hlint src app

.PHONY: doc
doc:
	cabal v2-haddock

.PHONY: clean
clean:
	cabal v2-clean
	- $(PWSH) -Command "& { Remove-Item -Recurse -Force output }"

.PHONY: targets
targets:
	$(PWSH) -Command "& { Get-Content ./Makefile | Where-Object { $$_ -like '.PHONY*' } | ForEach-Object { $$_.Substring(8) } }"
