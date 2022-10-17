## shallow clone for speed

REBAR_GIT_CLONE_OPTIONS += --depth 1
export REBAR_GIT_CLONE_OPTIONS

REBAR = rebar3
all: compile

compile:
	$(REBAR) compile

clean: distclean

ct: compile
	$(REBAR) ct -v

eunit: compile
	$(REBAR) eunit

xref:
	$(REBAR) xref

distclean:
	@rm -rf _build
	@rm -f data/app.*.config data/vm.*.args rebar.lock
	@rm -rf _build erl_crash.dump rebar3.crashdump rebar.lock abplug_cli


CUTTLEFISH_SCRIPT = _build/default/lib/cuttlefish/cuttlefish

$(CUTTLEFISH_SCRIPT):
	@${REBAR} get-deps
	@if [ ! -f cuttlefish ]; then make -C _build/default/lib/cuttlefish; fi

app.config: $(CUTTLEFISH_SCRIPT) etc/abplug.config
	$(verbose) $(CUTTLEFISH_SCRIPT) -l info -e etc/ -c etc/abplug.config 
