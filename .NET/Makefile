PWSH = pwsh

.PHONY: compile-proto
compile-proto:
	protoc --proto_path Example\protobuf --csharp_out Example --csharp_opt=file_extension=.g.cs protobuf.proto

.PHONY: targets
targets:
	$(PWSH) -Command "& { Get-Content ./Makefile | Where-Object { $$_ -like '.PHONY*' } | ForEach-Object { $$_.Substring(8) } }"
