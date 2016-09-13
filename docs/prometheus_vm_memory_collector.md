

# Module prometheus_vm_memory_collector #
* [Description](#description)

Collects information about memory dynamically allocated
by the Erlang emulator usin
[
erlang:memory/0
](http://erlang.org/doc/man/erlang.md#statistics-1).

__Behaviours:__ [`prometheus_collector`](prometheus_collector.md).

<a name="description"></a>

## Description ##

Also provides basic (D)ETS statistics.


### <a name="Exported_metrics">Exported metrics</a> ###


* 
```
erlang_vm_memory_bytes_total
```

Labels:

* kind="system"|"processes".


<br />
The total amount of memory currently allocated.
This is the same as the sum of the memory size for processes and system.

* 
```
erlang_vm_memory_processes_bytes_total
```

Labels:

* usage="used"|"free".


<br />
The total amount of memory currently allocated for the Erlang processes.

* 
```
erlang_vm_memory_system_bytes_total
```

Labels:

* usage="atom"|"binary"|"code"|"ets"|"other".


<br />
The total amount of memory currently allocated for the emulator
that is not directly related to any Erlang process.
Memory presented as processes is not included in this memory.

* 
```
erlang_vm_memory_atom_bytes_total
```

Labels:

* usage="free"|"used".


<br />
The total amount of memory currently allocated for atoms.
This memory is part of the memory presented as system memory.

* 
```
erlang_vm_ets_tables
```

Erlang VM ETS Tables count.

* 
```
erlang_vm_dets_tables
```

Erlang VM DETS Tables count.


