REBAR="./rebar3"

cleanapp:
	$(REBAR) clean skip_deps=true

# sed for add ssl - is fix for https://github.com/ninenines/ranch/issues/90
compile:
	$(REBAR) compile
#	sed -i 's/\(stdlib\)/\1,\t\tssl/' deps/ranch/ebin/ranch.app

compileapp:
	$(REBAR) compile skip_deps=true


run: cleanapp compileapp
	$(REBAR) shell

repl: cleanapp compileapp
	erl -pa _build/default/lib/*/ebin

deps:
	$(REBAR) update
	$(REBAR) upgrade
