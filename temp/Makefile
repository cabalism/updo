UPDO_TMP ?= .updo

$(UPDO_TMP)/ghc-%/constraints.dhall:
	mkdir -p $(UPDO_TMP)/ghc-$*
	[ -e ./project-dhall/ghc-$*/constraints.dhall ] && cp ./project-dhall/ghc-$*/constraints.dhall $@ || cp ./updo/empty/constraints.dhall $@

$(UPDO_TMP)/ghc-%.dhall:
	mkdir -p $(dir $(UPDO_TMP)/ghc-$*)
	[ -e ./project-dhall/ghc-$*.dhall ] && cp ./project-dhall/ghc-$*.dhall $@ || cp ./updo/empty/source-pkgs.dhall $@

.PHONY: pkgs-sorted
pkgs-sorted: $(UPDO_TMP)/pkgs-sorted.dhall

.PHONY: pkgs-upgrade-done
pkgs-upgrade-done: $(UPDO_TMP)/pkgs-upgrade-done.dhall

$(UPDO_TMP)/pkgs-sorted.dhall: \
  project-dhall/pkg-groups.dhall \
  project-dhall/pkgs/*.dhall
	mkdir -p $(@D) && $(PKGS_SORTED_HS) > $@

$(UPDO_TMP)/pkgs-upgrade-done.dhall: \
  project-dhall/pkgs/*.dhall \
  $(UPDO_TMP)/pkgs-sorted.dhall \
  project-dhall/pkgs-upgrade-todo.dhall
	./updo/project-dhall/pkgs-upgrade-done.hs \
	  ./$(UPDO_TMP)/pkgs-sorted.dhall \
	  ./project-dhall/pkgs-upgrade-todo.dhall \
	  > $@
