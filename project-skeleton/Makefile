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

# "Why .SECONDARY does not work with patterns (%) while .PRECIOUS does?"
#
# > You can also list the target pattern of an implicit rule (such as ‘%.o’) as
# > a prerequisite file of the special target .PRECIOUS
# > SEE: https://www.gnu.org/software/make/manual/html_node/Special-Targets.html
# 
# The docs do not say this about .SECONDARY. But for the few explicit
# exceptions, none of the special targets accept patterns.
# SEE: https://stackoverflow.com/questions/27090032/why-secondary-does-not-work-with-patterns-while-precious-does 
.PRECIOUS: project-dhall/ghc-%/text-templates/wrap-cabal.dhall
.PRECIOUS: project-dhall/ghc-%/text-templates/wrap-stack.dhall
	
project-dhall/ghc-%/text-templates/wrap-cabal.dhall: \
  updo/project-skeleton/ghc-x.y.z/text-templates/wrap-cabal.dhall
	mkdir -p project-dhall/ghc-$*/text-templates
	[ -e $@ ] || cp $< $@

project-dhall/ghc-%/text-templates/wrap-stack.dhall: \
  updo/project-skeleton/ghc-x.y.z/text-templates/wrap-stack.dhall
	mkdir -p project-dhall/ghc-$*/text-templates
	[ -e $@ ] || cp $< $@

# dhall2stack needs wrap-stack
project-dhall/ghc-%/text-templates/dhall2stack.dhall: \
  updo/project-skeleton/ghc-x.y.z/text-templates/dhall2stack.dhall \
  project-dhall/ghc-%/text-templates/wrap-stack.dhall \
  project-dhall/pkg-groups.dhall
	mkdir -p project-dhall/ghc-$*/text-templates
	[ -e $@ ] || cp $< $@

# dhall2cabal needs wrap-cabal
project-dhall/ghc-%/text-templates/dhall2cabal.dhall: \
  updo/project-skeleton/ghc-x.y.z/text-templates/dhall2cabal.dhall \
  project-dhall/ghc-%/text-templates/wrap-cabal.dhall \
  project-dhall/pkg-groups.dhall
	mkdir -p project-dhall/ghc-$*/text-templates
	[ -e $@ ] || cp $< $@
	
# dhall2config needs wrap-cabal
project-dhall/ghc-%/text-templates/dhall2config.dhall: \
  updo/project-skeleton/ghc-x.y.z/text-templates/dhall2config.dhall \
  project-dhall/ghc-%/text-templates/wrap-cabal.dhall \
  project-dhall/pkg-groups.dhall
	mkdir -p project-dhall/ghc-$*/text-templates
	[ -e $@ ] || cp $< $@
