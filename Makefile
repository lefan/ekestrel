ERL	?= erl
ERLC	= erlc
EBIN_DIR	:= $(wildcard deps/*/ebin)

.PHONY: rel deps

all: deps compile

cmplsmpl:
		./rebar compile

compile: deps
		./rebar compile

deps:
		./rebar get-deps
		./rebar check-deps

clean:
		./rebar clean

realclean: clean
		./rebar delete-deps

tests:
		./rebar skip_deps=true eunit

rel: deps
		./rebar compile generate

doc:
		rebar skip_deps=true doc

console:
		erl -pa deps/*/ebin deps/*/include ebin -boot start_sasl  -sname ekestrel -setcookie emacs


analyze: checkplt
		./rebar skip_deps=true dialyze

buildplt:
		./rebar skip_deps=true build-plt

checkplt: buildplt
		./rebar skip_deps=true check-plt


