PROJECT = kafcod
PROJECT_DESCRIPTION = Kafka Wire Protocol Codecs
PROJECT_VERSION = $(shell scripts/git-vsn)

# Until we support COMPACT_RECORDS, we need to suppress no_return warnings.
# DIALYZER_OPTS ?= -Werror_handling -Wunmatched_returns -Wno_return

compile:
	rebar3 compile

tests: eunit

eunit:
	rebar3 eunit
	make -C tools/make_codecs eunit

dialyze:
	rebar3 dialyzer

KAFKA_SOURCE_ROOT ?= $(HOME)/Source/apache/kafka
KAFKA_MESSAGES := $(KAFKA_SOURCE_ROOT)/clients/src/main/resources/common/message

codecs:
	make -C tools/make_codecs escriptize
	mkdir -p src/codecs
	./tools/make_codecs/make_codecs $(KAFKA_MESSAGES) src

# elvis doesn't like our function names in the codecs; needs configuring.
# ci :: elvis

clean:: clean-codecs

clean-codecs:
	make -C tools/make_codecs clean

ci:: dialyze eunit

eqwalize:: compile

# Workaround for eqwalizer.mk
ERLANG_MK_TMP = $(shell TERM=dumb QUIET=1 rebar3 path --base)
include eqwalizer.mk

GNU_TAR ?= gtar
ARCHIVE := ../$(PROJECT)-$(PROJECT_VERSION).tar

archive:
	$(GNU_TAR) -c -f $(ARCHIVE) --exclude-from .archive-exclude .
