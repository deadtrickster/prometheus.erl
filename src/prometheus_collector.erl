-module(prometheus_collector).

-callback register(Registry :: atom) -> ok.

-callback register() -> ok.

-callback deregister(Registry :: atom) -> ok.

-callback collect_mf(Callback :: fun ((atom, atom, list(atom), string | binary) -> ok), Registry :: atom) -> ok.

-callback collect_metrics(Name :: atom, Callback :: fun(([] | list(atom), integer() | float()) -> ok), Info :: any()) -> ok.

