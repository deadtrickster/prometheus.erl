-module(prometheus_format).

-callback content_type() -> binary().

-callback format() -> binary().

-callback format(Registry :: atom()) -> binary().
