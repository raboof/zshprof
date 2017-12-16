Add to the script to be profiled:

Start:

```
PS4=$'\\\011%D{%s%6.}\011%x\011%I\011%N\011%e\011'
exec 3>&2 2>/tmp/zshstart.$$.log
setopt xtrace prompt_subst
```

End:

```
unsetopt xtrace
exec 2>&3 3>&-
```

Compile and run the processor:

```
ocamlfind ocamlopt -linkpkg -thread -package str Callgrind.ml ZshXtrace.ml ZshXtraceToCallgrind.ml && ./a.out < /tmp/zshstart.6560.log > zsh.callgrind
```

View the result:

```
kcachegrind zsh.callgrind
```

Limitations:

* This approach is utterly broken when there is any parallellism. You'll have to fall back to zprof when that is the case.
* It seems kcachegrind doesn't like it when you use the same filename for different traces.. careful.
