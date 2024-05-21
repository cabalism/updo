project-dhall/pkgs/%.dhall: \
  updo/project-skeleton/pkgs/all-in-one.dhall
	mkdir -p project-dhall/pkgs
	[ -e project-dhall/pkgs/$*.dhall ] || cp $< project-dhall/pkgs/all-in-one.dhall

project-dhall/pkg-groups.dhall: \
  updo/project-skeleton/pkg-groups.dhall \
  $(wildcard project-dhall/pkgs/*.dhall)
	mkdir -p project-dhall
	[ -e $@ ] || cp $< $@

project-dhall/pkgs-upgrade-todo.dhall: \
  updo/project-skeleton/pkgs-upgrade-todo.dhall
	[ -e $@ ] || cp $< $@

project-dhall/ghc-$(GHC_VERSION)/text-templates/cabal-snippet.dhall: \
  updo/project-skeleton/ghc-x.y.z/text-templates/cabal-snippet.dhall
	mkdir -p project-dhall/ghc-$(GHC_VERSION)/text-templates
	[ -e $@ ] || cp $< $@

project-dhall/ghc-$(GHC_VERSION)/text-templates/stack-snippet.dhall: \
  updo/project-skeleton/ghc-x.y.z/text-templates/stack-snippet.dhall
	mkdir -p project-dhall/ghc-$(GHC_VERSION)/text-templates
	[ -e $@ ] || cp $< $@

# dhall2stack needs stack-snippet
project-dhall/ghc-$(GHC_VERSION)/text-templates/dhall2stack.dhall: \
  updo/project-skeleton/ghc-x.y.z/text-templates/dhall2stack.dhall \
  updo/project-skeleton/ghc-x.y.z/text-templates/stack-snippet.dhall \
  project-dhall/pkg-groups.dhall
	mkdir -p project-dhall/ghc-$(GHC_VERSION)/text-templates
	[ -e $@ ] || cp $< $@

# dhall2cabal and dhall2config need cabal-snippet
project-dhall/ghc-$(GHC_VERSION)/text-templates/%.dhall: \
  updo/project-skeleton/ghc-x.y.z/text-templates/%.dhall \
  updo/project-skeleton/ghc-x.y.z/text-templates/cabal-snippet.dhall \
  project-dhall/pkg-groups.dhall
	mkdir -p project-dhall/ghc-$(GHC_VERSION)/text-templates
	[ -e $@ ] || cp $< $@
