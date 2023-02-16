include updo/alternatives/cabal2stack/Makefile
include updo/alternatives/stack2cabal/Makefile

ifeq ($(CABAL2STACK), true)
  ifeq ($(STACK2CABAL), true)
    project-files-mirror: \
      project-files-cabal2stack \
      project-files-stack2cabal
  else
    project-files-mirror: project-files-cabal2stack
  endif
else
  ifeq ($(STACK2CABAL), true)
    project-files-mirror: project-files-stack2cabal
  else
    project-files-mirror:

  endif
endif
