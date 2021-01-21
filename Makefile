PWSH = pwsh

.PHONY: build
build: .NET/Hui/HuiLib.dll

.NET/Hui/HuiLib.dll: Haskell/output/Hui.dll
	$(PWSH) -Command "& { Copy-Item Haskell\output\Hui.dll .NET\Hui\HuiLib.dll }"

Haskell/output/Hui.dll:
	$(PWSH) -Command "& { Set-Location Haskell; make }"

.PHONY: clean
clean:
	- $(PWSH) -Command "& { Set-Location Haskell; make clean }"

.PHONY: targets
targets:
	$(PWSH) -Command "& { Get-Content ./Makefile | Where-Object { $$_ -like '.PHONY*' } | ForEach-Object { $$_.Substring(8) } }"
