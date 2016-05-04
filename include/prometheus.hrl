-define(PROMETHEUS_TABLE, prometheus_table).
-define(PROMETHEUS_COUNTER_TABLE, prometheus_counter_table).
-define(PROMETHEUS_GAUGE_TABLE, prometheus_gauge_table).
-define(PROMETHEUS_SUMMARY_TABLE, prometheus_summary_table).

-define(PROMETHEUS_COUNTER_DEFAULT, 0).

-define(PROMETHEUS_VM_STATISTICS, [context_switches,
                                   garbage_collection,
                                   io,
                                   reductions,
                                   run_queue,
                                   runtime,
                                   wall_clock]).

-define(PROMETHEUS_DEFAULT_COLLECTORS, [prometheus_vm_memory_collector,
                                        prometheus_vm_statistics_collector]).
