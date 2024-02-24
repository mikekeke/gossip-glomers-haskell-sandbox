FOURMOLU_EXTENSIONS := -o -XTypeApplications -o -XTemplateHaskell -o -XImportQualifiedPost -o -XPatternSynonyms

format:
	fourmolu $(FOURMOLU_EXTENSIONS) --mode inplace --check-idempotence $$(find src/ test/ app/ -iregex ".*.hs")

build-install:
	cabal build &&  cabal install exe:haskell --installdir ./bins --overwrite-policy=always