-module(prometheus_format).

%%====================================================================
%% Callbacks
%%====================================================================

-callback content_type() -> binary().

-callback format() -> binary().

-callback format(Registry :: prometheus_registry:registry()) -> binary().
