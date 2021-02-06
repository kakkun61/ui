PWSH = pwsh

.PHONY: build
build: .NET/Hui/Logic.dll

.NET/Hui/Logic.dll:
	$(PWSH) -Command "& { Set-Location Haskell; make deploy-example }"

.PHONY: clean
clean:
	- $(PWSH) -Command "& { Set-Location Haskell; make clean }"

.PHONY: targets
targets:
	$(PWSH) -Command "& { Get-Content ./Makefile | Where-Object { $$_ -like '.PHONY*' } | ForEach-Object { $$_.Substring(8) } }"
