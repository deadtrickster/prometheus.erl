-module(prometheus_custom_collector).

-callback collect_mf(Callback :: fun ((atom, atom, list(atom), string | binary) -> ok)) -> ok.

-callback collect_metrics(Name :: atom, Callback :: fun(([] | list(atom), integer() | float()) -> ok), Info :: any()) -> ok.

-callback register(Registry :: atom) -> ok.

-callback register() -> ok.
